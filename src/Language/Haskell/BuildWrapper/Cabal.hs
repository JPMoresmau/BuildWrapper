{-# LANGUAGE PatternGuards,ScopedTypeVariables,CPP #-}
-- |
-- Module      : Language.Haskell.BuildWrapper.Cabal
-- Author      : JP Moresmau
-- Copyright   : (c) JP Moresmau 2011
-- License     : BSD3
-- 
-- Maintainer  : jpmoresmau@gmail.com
-- Stability   : beta
-- Portability : portable
-- 
-- Cabal operations: configure, build, retrieve information from build info, parse errors and warnings
module Language.Haskell.BuildWrapper.Cabal where

import Language.Haskell.BuildWrapper.Base
import Language.Haskell.BuildWrapper.Packages

import Control.Monad.State


import Data.Char
import Data.Ord (comparing)
import Data.List
import Data.Maybe

#if MIN_VERSION_Cabal(1,15,0)   
import Data.Version (parseVersion)
import Text.ParserCombinators.ReadP(readP_to_S)
#endif

import qualified Data.Map as DM
import qualified Data.Set as DS

import Exception (ghandle)

import Distribution.ModuleName
import Distribution.PackageDescription ( otherModules,library,executables,testSuites,Library,hsSourceDirs,libBuildInfo,Executable(..),exeName,modulePath,buildInfo,TestSuite(..),testName,TestSuiteInterface(..),testInterface,testBuildInfo,BuildInfo,cppOptions,defaultExtensions,otherExtensions,oldExtensions )
import Distribution.Simple.GHC
#if MIN_VERSION_Cabal(1,15,0)   
import Distribution.Simple.Program.GHC
#endif
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Compiler(OptimisationLevel(..))
import qualified Distribution.PackageDescription as PD 
import Distribution.Package
import Distribution.InstalledPackageInfo as IPI
import Distribution.Version
import Distribution.Text (display,simpleParse)

                    
import qualified Distribution.Simple.Configure as DSC
import qualified Distribution.Verbosity as V
import Text.Regex.TDFA

import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Data.Functor.Identity (runIdentity)

getCabalLibraryVersion :: String
getCabalLibraryVersion = VERSION_Cabal

getFilesToCopy :: BuildWrapper(OpResult [FilePath])
getFilesToCopy =do
       (mfps,bwns)<-withCabal Source getAllFiles
       return $ case mfps of
                Just fps->(nub $ concatMap (map snd . cbiModulePaths) fps,bwns)
                Nothing ->([],bwns); 


cabalV :: BuildWrapper V.Verbosity
cabalV =do
        v<-gets verbosity
        return $ toCabalV v
        where 
                toCabalV Silent =V.silent
                toCabalV Normal =V.normal
                toCabalV Verbose =V.verbose
                toCabalV Deafening =V.deafening

-- | run cabal build
cabalBuild :: Bool -- ^ do we want output (True) or just compilation without linking?
        -> WhichCabal -- ^ use original cabal or temp cabal file
        -> BuildWrapper (OpResult BuildResult)
cabalBuild = cabalBuild' True


-- | run cabal build
cabalBuild' :: Bool -- ^ can we rerun configure again
        -> Bool -- ^ do we want output (True) or just compilation without linking?
        -> WhichCabal -- ^ use original cabal or temp cabal file
        -> BuildWrapper (OpResult BuildResult)
cabalBuild' reRun output srcOrTgt= do
        dist_dir<-getDistDir
        (mr,n)<-withCabal srcOrTgt (\_->do
                cf<-getCabalFile srcOrTgt
                cp<-gets cabalPath
                v<-cabalV
                copts<-gets cabalOpts
                let args=[
                        "build",
                        "--verbose=" ++ show (fromEnum v),
                        "--builddir="++dist_dir
                        
                        ] ++ (if output 
                                then []
                                else ["--ghc-option=-c"])
                        ++ copts
                liftIO $ do
                        -- print args
                        cd<-getCurrentDirectory
                        setCurrentDirectory (takeDirectory cf)
                        (ex,out,err)<-readProcessWithExitCode cp args ""
                        -- putStrLn err
                        if isInfixOf "cannot satisfy -package-id" err ||  isInfixOf "re-run the 'configure'" err
                                then 
                                        return Nothing
                                else
                                        do
                                        let fps=mapMaybe getBuiltPath (lines out)
                                        let ret=parseBuildMessages (takeFileName cf) (takeFileName cp) dist_dir err
                                        setCurrentDirectory cd
                                        return $ Just (ex==ExitSuccess,ret,fps)
            )
        case mr of
                Nothing -> return (BuildResult False [],n)
                Just Nothing->if reRun 
                        then do
                                let setup_config = DSC.localBuildInfoFile dist_dir
                                liftIO $ removeFile setup_config
                                cabalBuild' False output srcOrTgt
                        else return (BuildResult False [],n)
                Just (Just (r,n2,fps)) -> return (BuildResult r fps, n ++ n2)

-- | run cabal configure
cabalConfigure :: WhichCabal -- ^ use original cabal or temp cabal file
        -> BuildWrapper (OpResult (Maybe LocalBuildInfo)) -- ^ return the build info on success, or Nothing on failure
cabalConfigure srcOrTgt= do
        cf<-getCabalFile srcOrTgt
        cp<-gets cabalPath
        okCF<-liftIO $ doesFileExist cf
        if okCF 
            then 
                do
                v<-cabalV
                dist_dir<-getDistDir
                uf<-gets cabalFlags
                copts<-gets cabalOpts
                let args=[
                        "configure",
                        "--verbose=" ++ show (fromEnum v),
                        "--user",
                        "--enable-tests",
                        "--builddir="++dist_dir
                        ] 
                        ++ (if null uf then [] else ["--flags="++uf])
                        ++ copts
                liftIO $ do
                        -- print args
                        cd<-getCurrentDirectory
                        setCurrentDirectory (takeDirectory cf)
                        (ex,_,err)<-readProcessWithExitCode cp args ""
                        putStrLn err
                        let msgs=parseCabalMessages (takeFileName cf) (takeFileName cp) err -- ++ (parseCabalMessages (takeFileName cf) out)
                        ret<-case ex of
                                ExitSuccess  -> if any isBWNoteError msgs 
                                        then return (Nothing,msgs)
                                        else do 
                                                lbi<-DSC.getPersistBuildConfig dist_dir
                                                return (Just lbi,msgs)
                                ExitFailure ec -> if null msgs
                                                then return (Nothing,[BWNote BWError ("Cabal configure returned error code " ++ show ec) (mkEmptySpan cf 0 1)])   
                                                else return (Nothing, msgs)
                        setCurrentDirectory cd
                        return ret
            else do
                let err="Cabal file"++ cf ++" does not exist"
                liftIO $ putStrLn err
                return (Nothing,[BWNote BWError err (mkEmptySpan cf 0 1)])       

-- | get the full path to the cabal file
getCabalFile :: WhichCabal  -- ^ use original cabal or temp cabal file
        -> BuildWrapper FilePath
getCabalFile Source= gets cabalFile
getCabalFile Target= fmap takeFileName (gets cabalFile)
                         >>=getTargetPath

-- | get Cabal build info, running configure if needed
cabalInit :: WhichCabal  -- ^ use original cabal or temp cabal file
        -> BuildWrapper (OpResult (Maybe LocalBuildInfo))
cabalInit srcOrTgt= do
   cabal_file<-getCabalFile srcOrTgt
   dist_dir<-getDistDir
   let setup_config = DSC.localBuildInfoFile dist_dir
   conf'd <- liftIO $ doesFileExist setup_config
   if not conf'd 
        then do
                liftIO $ putStrLn "configuring because setup_config not present"
                cabalConfigure srcOrTgt
        else do
             cabal_time <- liftIO $ getModificationTime cabal_file
             conf_time <- liftIO $ getModificationTime setup_config
             if cabal_time > conf_time 
                then do
                        liftIO $ putStrLn "configuring because setup_config too old"
                        cabalConfigure srcOrTgt
                else do
                        mb_lbi <- liftIO $ DSC.maybeGetPersistBuildConfig dist_dir
                        case mb_lbi of
                          Nothing -> do
                            liftIO $ putStrLn "configuring because persist build config not present"
                            cabalConfigure srcOrTgt
                          Just _lbi -> return (Just _lbi, [])

-- | run a action with the cabal build info
withCabal :: WhichCabal  -- ^ use original cabal or temp cabal file
        -> (LocalBuildInfo -> BuildWrapper a) -- ^ action to run if we get a build info
        -> BuildWrapper (OpResult (Maybe a))  -- ^ the result of the action, or Nothing if we could get Cabal info
withCabal srcOrTgt f=do
        (mlbi,notes)<-cabalInit srcOrTgt
        case mlbi of
                Nothing-> return (Nothing, notes)
                Just lbi ->do
                        r<-f lbi
                        return (Just r, notes)
     
-- | parse cabal error messages and transform them in notre
parseCabalMessages :: FilePath -- ^ cabal file
        -> FilePath -- ^ path to cabal executable
        -> String -- ^ error output
        -> [BWNote]
parseCabalMessages cf cabalExe s=let
        (m,ls)=foldl parseCabalLine (Nothing,[]) $ lines s
        in nub $ case m of
                Nothing -> ls
                Just (bwn,msgs)->ls++[makeNote bwn msgs] 
        where 
                parseCabalLine :: (Maybe (BWNote,[String]),[BWNote]) -> String ->(Maybe (BWNote,[String]),[BWNote])
                parseCabalLine (currentNote,ls) l 
                        | "Error:" `isPrefixOf` l=(Just (BWNote BWError "" (mkEmptySpan cf 1 1),[dropWhile isSpace $ drop 6 l]),addCurrent currentNote ls)
                        | "Warning:" `isPrefixOf` l=let
                                msg=(dropWhile isSpace $ drop 8 l)
                                msg2=if cf `isPrefixOf` msg
                                        then dropWhile isSpace $ drop (length cf + 1) msg
                                        else msg
                                in (Just (BWNote BWWarning "" (mkEmptySpan cf (extractLine msg2) 1),[msg2]),addCurrent currentNote ls)
                        | Just (bw,n)<- cabalErrorLine cf cabalExe l=(Just (bw,n),addCurrent currentNote ls)
                        | Just (jcn,msgs)<-currentNote=
                                if not $ null l
                                        then (Just (jcn,l:msgs),ls)
                                        else (Nothing,ls++[makeNote jcn msgs])
                        | otherwise =(Nothing,ls)
                extractLine el=let
                        (_,_,_,ls)=el =~ "\\(line ([0-9]*)\\)" :: (String,String,String,[String])
                        in if null ls
                                then 1
                                else readInt (head ls) 1
 
setupExe :: FilePath -- ^ path to cabal executable
        -> FilePath
setupExe cabalExe=addExtension "setup" $ takeExtension cabalExe 

dropPrefixes :: [String] -> String -> Maybe String
dropPrefixes prfxs s2=foldr (stripPrefixIfNeeded s2) Nothing prfxs

stripPrefixIfNeeded :: String -> String -> Maybe String -> Maybe String
stripPrefixIfNeeded _ _ j@(Just _)=j
stripPrefixIfNeeded s3 prfx  _=stripPrefix prfx s3

addCurrent :: Maybe (BWNote, [String]) -> [BWNote] -> [BWNote]
addCurrent Nothing xs=xs
addCurrent (Just (n,msgs)) xs=xs++[makeNote n msgs]

cabalErrorLine :: FilePath -- ^ cabal file
        -> FilePath -- ^ path to cabal executable
        -> String -- ^ line
        -> Maybe (BWNote,[String])
cabalErrorLine cf cabalExe l 
        | Just s4 <- dropPrefixes [cabalExe,setupExe cabalExe] l=
                                let 
                                        s2=dropWhile isSpace $ drop 1 s4 -- drop 1 for ":" that follows file name
                                in if "At least the following" `isPrefixOf` s2
                                                then Just (BWNote BWError "" (mkEmptySpan cf 1 1), [s2])
                                                else 
                                                        let
                                                                (loc,rest)=span (/= ':') s2
                                                                (realloc,line,msg)=if null rest || ":"==rest
                                                                        then    (cf,"1",s2)
                                                                        else 
                                                                                let tr=tail rest
                                                                                    (line',msg')=span (/= ':') tr
                                                                                in if null msg'
                                                                                        then (loc,"1",tr)
                                                                                        else if readInt line' (-1)==(-1)
                                                                                                then (cf,"1",s2)
                                                                                                else (loc,line',tail msg')
                                                        in Just (BWNote BWError "" (mkEmptySpan realloc (readInt line 1) 1),[msg])
         | otherwise=Nothing           

-- | parse messages from build
parseBuildMessages ::  FilePath -- ^ cabal file
        -> FilePath -- ^ path to cabal executable
        -> FilePath -- ^ the dist directory
        -> String -- ^ the build output 
        -> [BWNote]
parseBuildMessages cf cabalExe distDir s=let
        (m,ls)=foldl parseBuildLine (Nothing,[]) $ lines s
        in (nub $
           case m of
               Nothing -> ls
               Just (bwn, msgs) -> ls ++ [makeNote bwn msgs])
        where 
                parseBuildLine :: (Maybe (BWNote,[String]),[BWNote]) -> String ->(Maybe (BWNote,[String]),[BWNote])
                parseBuildLine (currentNote,ls) l  
                        | Just (jcn,msgs)<-currentNote=
                                if not (null l)  && ((' ' == head l) || (')' == last l))
                                       then (Just (jcn,l:msgs),ls)
                                       else (Nothing,ls++[makeNote jcn msgs])
                        --  | Just fp<-getBuiltPath l=(currentNote,ls,fp:fps)
                        | Just n<-extractLocation l=(Just (n,[bwnTitle n]),ls)
                        | Just (bw,n)<- cabalErrorLine cf cabalExe l=(Just (bw,n),addCurrent currentNote ls)
                        | otherwise =(Nothing,ls)
                extractLocation el=let
                        (_,_,aft,ls)=el =~ "(.+):([0-9]+):([0-9]+):" :: (String,String,String,[String])   
                        in case ls of
                                (loc:line:col:[])-> Just $ BWNote BWError (dropWhile isSpace aft) (mkEmptySpan loc (readInt line 1) (readInt col 1))
                                _ -> let
                                      (_,_,_,ls2)=el =~ "(.+)(\\(.+\\)):(.+):(.+):" :: (String,String,String,[String])
                                      in case ls2 of
                                        (loc2:ext1:_:_:[])-> Just $ BWNote BWError (drop (length loc2 + length ext1 + 1) el) (mkEmptySpan (validLoc cf distDir loc2) 1 1)
                                        _     -> Nothing
  
validLoc :: FilePath -- ^ the cabal file 
        -> FilePath -- ^ the dist dir
        -> FilePath
        -> FilePath
validLoc cf distDir f=if distDir `isPrefixOf` f
        then cf
        else f
  
readInt :: String -> Int -> Int
readInt s def=let parses=reads s ::[(Int,String)]
        in if null parses 
                then def
                else fst $ head parses
        
-- | add a message to the note
makeNote :: BWNote  -- ^ original note
        -> [String] -- ^ message lines
        ->BWNote
makeNote bwn msgs=let
        title=dropWhile isSpace $ unlines $ reverse msgs
        in if "Warning:" `isPrefixOf` title
                then bwn{bwnTitle=dropWhile isSpace $ drop 8 title,bwnStatus=BWWarning}    
                else bwn{bwnTitle=title}      

-- | get the path of a file getting compiled
getBuiltPath :: String -- ^ the message line
        -> Maybe FilePath -- ^ the path if we could parse it
getBuiltPath line=let
         (_,_,_,ls)=line =~ "\\[[0-9]+ of [0-9]+\\] Compiling .+\\( (.+), (.+)\\)" :: (String,String,String,[String])   
         in case ls of
                (src:_:[])->Just src
                _ -> Nothing
          
-- | the cabal build info for a specific component
data CabalBuildInfo=CabalBuildInfo {
        cbiBuildInfo::BuildInfo -- ^ the build info
        ,cbiComponentBuildInfo :: ComponentLocalBuildInfo -- ^ the component local build info
        ,cbiBuildFolder::FilePath -- ^ the folder to build that component into
        ,cbiIsLibrary::Bool -- ^ is the component the library
        ,cbiModulePaths::[(Maybe ModuleName,FilePath)]  -- ^ the module name and corresponding source file for each contained Haskell module
        ,cbiComponent::CabalComponent -- ^  the component handle
         } 
      deriving (Read,Show)      
            
-- | canonicalize the paths in the build info
canonicalizeBuildInfo :: CabalBuildInfo -> BuildWrapper CabalBuildInfo
canonicalizeBuildInfo =onModulePathsM (mapM (\(m,path)->do
                pathC<-canonicalizeFullPath path
                return (m,pathC)))

-- | apply a function on the build info modules and paths, in a monad
onModulePathsM :: (Monad a) 
        =>([(Maybe ModuleName,FilePath)] -> a [(Maybe ModuleName,FilePath)]) -- ^ the function to apply
        -> CabalBuildInfo -- ^ the original build info
        -> a CabalBuildInfo  -- ^ the result
onModulePathsM f cbi=do
        let ls=cbiModulePaths cbi
        fls<-f ls
        return cbi{cbiModulePaths=fls}         

-- | apply a function on the build info modules and paths
onModulePaths :: ([(Maybe ModuleName,FilePath)] -> [(Maybe ModuleName,FilePath)]) -- ^ the function to apply
        -> CabalBuildInfo -- ^ the original build info
        -> CabalBuildInfo   -- ^ the result
onModulePaths f =runIdentity . onModulePathsM (return . f)
 
-- | get the build info for a given source file
-- if a source file is in several component, get the first one             
getBuildInfo ::  FilePath  -- ^the source file
        -> Maybe String -- ^ the cabal component to use, or Nothing if not specified
        -> BuildWrapper (OpResult (Maybe (LocalBuildInfo,CabalBuildInfo)))
getBuildInfo fp mccn=do
        case mccn of
                Nothing -> do
                        (mmr,bwns)<-go getReferencedFiles Nothing
                        case mmr of
                                Just (Just a)->return (Just a, bwns)
                                _ -> do
                                        (mmr2,bwns2)<-go getAllFiles Nothing -- no component found for the asked name
                                        return $ case mmr2 of
                                                Just (Just a)-> (Just a,bwns2)
                                                _-> (Nothing,bwns)
                _ -> do
                        (mmr2,bwns2)<-go getAllFiles mccn
                        return $ case mmr2 of
                                        Just (Just a)-> (Just a,bwns2)
                                        _-> (Nothing,bwns2)
        where 
             go f mccn2=withCabal Source (\lbi->do
                fps<-f lbi
                fpsC<-mapM canonicalizeBuildInfo fps
                ok<-getComp mccn2 fpsC
                return  $ if null ok
                        then Nothing
                        else Just (lbi, head ok))
             getComp :: Maybe String -> [CabalBuildInfo] -> BuildWrapper [CabalBuildInfo] 
             getComp Nothing fps=do
                fpC<-canonicalizeFullPath fp
                fpsC<-mapM canonicalizeBuildInfo fps
                return $ filter (not . null . cbiModulePaths) $
                        map (onModulePaths (filter (\(_,b)->equalFilePath fpC b))) fpsC
             getComp (Just ccn) fps=
                return $ filter (\cbi->cabalComponentName (cbiComponent cbi) == ccn) fps
 
-- | get GHC options for a file            
fileGhcOptions :: (LocalBuildInfo,CabalBuildInfo) -- ^ the cabal info
        -> BuildWrapper [String] -- ^ the module name and the options to pass GHC
fileGhcOptions (lbi,CabalBuildInfo bi clbi fp isLib _ _)=do
        dist_dir<-getDistDir
        let inplace=dist_dir </> "package.conf.inplace"
        inplaceExist<-liftIO $ doesFileExist inplace
#if MIN_VERSION_Cabal(1,15,0)   
        v<-cabalV
        let opts l b c f=renderGhcOptions ((fst $ head $ readP_to_S  parseVersion  VERSION_ghc) :: Version) $ componentGhcOptions v l b c f 
#else
        let opts=ghcOptions
#endif        
        let pkg
                  | isLib =
                    ["-package-name", display $ packageId $ localPkgDescr lbi]
                  | inplaceExist = ["-package-conf", inplace]
                  | otherwise = []
        return (pkg ++ opts (lbi{withOptimization=NoOptimisation}) bi clbi fp)
 

                
-- | get CPP options for a file
fileCppOptions :: CabalBuildInfo -- ^ the cabal info
        -> [String] -- ^ the list of CPP options
fileCppOptions cbi=cppOptions $ cbiBuildInfo cbi      

-- | get the cabal extensions
cabalExtensions :: CabalBuildInfo -- ^ the cabal info
        -> (ModuleName,[String]) -- ^ the module name and cabal extensions
cabalExtensions CabalBuildInfo{cbiBuildInfo=bi,cbiModulePaths=ls}=(fromJust $ fst $ head ls,map show (otherExtensions bi ++ defaultExtensions bi ++ oldExtensions bi))      
       
-- | get the source directory from a build info
getSourceDirs :: BuildInfo -- ^ the build info
        -> [FilePath]   -- ^ the source paths, guaranteed non null
getSourceDirs bi=let
        hsd=hsSourceDirs bi
        in case hsd of
                [] -> ["."]
                _ -> hsd 
       
-- | get all components, referencing all the files found in the source folders 
getAllFiles :: LocalBuildInfo -- ^ the build info
        -> BuildWrapper [CabalBuildInfo]
getAllFiles lbi= do
                let pd=localPkgDescr lbi
                let libs=maybe [] extractFromLib $ library pd
                let exes=map extractFromExe $ executables pd
                let tests=map extractFromTest $ testSuites pd
                cbis<-mapM (\(a,b,c,isLib,d,cc)->do
                        mf<-copyAll d
                        return (CabalBuildInfo a b c isLib mf cc)) (libs ++ exes ++ tests)
                cbis2<-getReferencedFiles lbi
                return $ zipWith (\c1@CabalBuildInfo{cbiModulePaths=cb1} CabalBuildInfo{cbiModulePaths=cb2}->c1{cbiModulePaths=nubOrd $ cb1++cb2}) cbis cbis2
                -- return cbis
        where 
        extractFromLib :: Library -> [(BuildInfo,ComponentLocalBuildInfo,FilePath,Bool,[FilePath],CabalComponent)]
        extractFromLib l=let
                lib=libBuildInfo l
                in [(lib, fromJustDebug "extractFromLibAll" $ libraryConfig lbi,buildDir lbi, True, getSourceDirs lib,cabalComponentFromLibrary l)]
        extractFromExe :: Executable -> (BuildInfo,ComponentLocalBuildInfo,FilePath,Bool,[FilePath],CabalComponent)
        extractFromExe e@Executable{exeName=exeName'}=let
                ebi=buildInfo e
                targetDir = buildDir lbi </> exeName'
                exeDir    = targetDir </> (exeName' ++ "-tmp")
                hsd=getSourceDirs ebi
                in (ebi,fromJustDebug "extractFromExeAll" $ lookup exeName' $ executableConfigs lbi,exeDir,False, hsd,cabalComponentFromExecutable e) 
        extractFromTest :: TestSuite -> (BuildInfo,ComponentLocalBuildInfo,FilePath,Bool,[FilePath],CabalComponent)
        extractFromTest t@TestSuite {testName=testName'} =let
                tbi=testBuildInfo t
                targetDir = buildDir lbi </> testName'
                testDir    = targetDir </> (testName' ++ "-tmp")
                hsd=getSourceDirs tbi
                in (tbi,fromJustDebug "extractFromTestAll" $ lookup testName' $ testSuiteConfigs lbi,testDir,False,hsd,cabalComponentFromTestSuite t)
        copyAll :: [FilePath] -> BuildWrapper [(Maybe ModuleName,FilePath)]
        copyAll fps= do 
                  allF<-mapM copyAll' fps
                  return $ concat allF
        copyAll' :: FilePath -> BuildWrapper [(Maybe ModuleName,FilePath)]
        copyAll' fp=do
                cf<-gets cabalFile
                let dir=takeDirectory cf
                fullFP<-getFullSrc fp
                allF<-liftIO $ getRecursiveContents fullFP
                tf<-gets tempFolder
                let cabalDist=takeDirectory tf </> "dist"
                let cabalDevDist=takeDirectory tf </> "cabal-dev"
                -- exclude every file containing the temp folder name (".buildwrapper" by default)
                -- which may happen if . is a source path
                let notMyself=filter (\x->not $ any (`isInfixOf` x) [cabalDevDist, cabalDist, tf]) allF
                return $ map (\f->(simpleParse $ fileToModule $ makeRelative fullFP f,makeRelative dir f)) notMyself
                -- return $ map (\(x,y)->(fromJust x,y)) $ filter (isJust . fst) $ map (\f->(simpleParse $ fileToModule $ makeRelative fullFP f,makeRelative dir f)) notMyself
 
-- | get all components, referencing only the files explicitely indicated in the cabal file
getReferencedFiles :: LocalBuildInfo -> BuildWrapper [CabalBuildInfo]
getReferencedFiles lbi= do
                let pd=localPkgDescr lbi
                let libs=maybe [] extractFromLib $ library pd
                let exes=map extractFromExe $ executables pd
                let tests=map extractFromTest $ testSuites pd
                let cbis=libs ++ exes ++ tests
                mapM (\c1@CabalBuildInfo{cbiModulePaths=cb1}->do
                        cb2<-filterM (\(_,f)->do
                                fs<-getFullSrc f
                                liftIO $ doesFileExist fs) cb1
                        return c1{cbiModulePaths=cb2}) cbis
        where 
        extractFromLib :: Library -> [CabalBuildInfo]
        extractFromLib l=let
                lib=libBuildInfo l
                modules=PD.exposedModules l ++ otherModules lib
                in [CabalBuildInfo lib (fromJustDebug "extractFromLibRef" $ libraryConfig lbi)
                        (buildDir lbi) True (copyModules modules (getSourceDirs lib)) (cabalComponentFromLibrary l)]
        extractFromExe :: Executable ->CabalBuildInfo
        extractFromExe e@Executable{exeName=exeName'}=let
                ebi=buildInfo e
                targetDir = buildDir lbi </> exeName'
                exeDir    = targetDir </> (exeName' ++ "-tmp")
                modules= (otherModules ebi)
                hsd=getSourceDirs ebi
                in CabalBuildInfo ebi (fromJustDebug "extractFromExeRef" $ lookup exeName' $ executableConfigs lbi) exeDir False (copyMain (modulePath e) hsd ++ copyModules modules hsd) (cabalComponentFromExecutable e) 
        extractFromTest :: TestSuite -> CabalBuildInfo
        extractFromTest t@TestSuite {testName=testName'} =let
                tbi=testBuildInfo t
                targetDir = buildDir lbi </> testName'
                testDir    = targetDir </> (testName' ++ "-tmp")
                modules= (otherModules tbi )
                hsd=getSourceDirs tbi
                extras=case testInterface t of
                    (TestSuiteExeV10 _ mp) -> copyMain mp hsd
                    (TestSuiteLibV09 _ mn) -> copyModules [mn] hsd
                    _ -> []
                in CabalBuildInfo tbi (fromJustDebug ("extractFromTestRef:"++testName' ++ show (testSuiteConfigs lbi)) $ lookup testName' $ testSuiteConfigs lbi) testDir False (extras ++ copyModules modules hsd) (cabalComponentFromTestSuite t)
        copyModules :: [ModuleName] -> [FilePath] -> [(Maybe ModuleName,FilePath)]
        copyModules mods=copyFiles (concatMap (\m->[toFilePath m <.> "hs", toFilePath m <.> "lhs"]) mods)
        copyFiles :: [FilePath] -> [FilePath] -> [(Maybe ModuleName,FilePath)]
        copyFiles mods dirs=let
                rmods=filter (isJust . snd ) $ map (\x->(x,simpleParse $ fileToModule x)) mods
                in [(Just modu,d </> m)  | (m,Just modu)<-rmods, d<-dirs]    
        copyMain :: FilePath  ->[FilePath] ->  [(Maybe ModuleName,FilePath)]
        copyMain fs = map (\ d -> (Just $ fromString "Main", d </> fs)) 
       
       
stringToModuleName :: String -> Maybe ModuleName
stringToModuleName=simpleParse       
        
-- | convert a ModuleName to a String        
moduleToString :: ModuleName -> String
moduleToString = intercalate "." . components

-- | get all components in the Cabal file
cabalComponents :: BuildWrapper (OpResult [CabalComponent])
cabalComponents = do
     (rs,ns)<-withCabal Source (return . cabalComponentsFromDescription . localPkgDescr)
     return (fromMaybe [] rs,ns)   

-- | get all the dependencies in the cabal file
cabalDependencies :: Maybe FilePath   -- ^ the path to the cabal-dev sandbox if any
        -> BuildWrapper (OpResult [(FilePath,[CabalPackage])]) -- ^ the result is an array of tuples: the path to the package database, the list of packages in that db that the Cabal file references
cabalDependencies msandbox= do
     (rs,ns)<-withCabal Source (\lbi-> liftIO $
          ghandle
            (\ (e :: IOError) ->
               do print e
                  return [])
            $
            do 
               pkgs <- liftIO $ getPkgInfos msandbox
               --let m=cabalComponentsDependencies (localPkgDescr lbi)
               --print m
               --let deps=PD.buildDepends (localPkgDescr lbi)
               --print deps
               --liftIO $ mapM_ (\d->print $ (show d) ++ (show $ DM.lookup (show $ simplifyDependency d) m)) deps
               return $ dependencies (localPkgDescr lbi) pkgs
            )
     return (fromMaybe [] rs,ns)

-- | get all dependencies from the package description and the list of installed packages        
dependencies :: PD.PackageDescription -- ^ the cabal description
        -> [(FilePath,[InstalledPackageInfo])] -- ^ the installed packages, by package database location
        -> [(FilePath,[CabalPackage])] -- ^ the referenced packages, by package database location
dependencies pd pkgs=let
        pkgsMap=foldr buildPkgMap DM.empty pkgs -- build the map of package by name with ordered version (more recent first)
        -- allC= cabalComponentsFromDescription pd
        compDeps=cabalComponentsDependencies pd
        -- gdeps=PD.buildDepends pd
        cpkgs=concat $ DM.elems $ DM.map (\ipis->getDep compDeps ipis []) pkgsMap
        in DM.assocs $ DM.fromListWith (++)
                (map (\ (a, b) -> (a, [b])) cpkgs ++
                map (\ (a, _) -> (a, [])) pkgs)
        where 
                buildPkgMap :: (FilePath,[InstalledPackageInfo]) -> DM.Map String [(FilePath,InstalledPackageInfo)] -> DM.Map String  [(FilePath,InstalledPackageInfo)]
                buildPkgMap (fp,ipis) m=foldr (\i dm->let
                        key=display $ pkgName $ sourcePackageId i
                        vals=DM.lookup key dm
                        newvals=case vals of
                                Nothing->[(fp,i)]
                                Just l->sortBy (flip (comparing (pkgVersion . sourcePackageId . snd))) ((fp,i):l)
                        in DM.insert key newvals dm
                        ) m ipis
                getDep :: DM.Map CabalComponent [Dependency] -> [(FilePath,InstalledPackageInfo)] -> [(FilePath,CabalPackage)] -> [(FilePath,CabalPackage)]
                getDep _ [] acc= acc
                getDep allC ((fp,InstalledPackageInfo{sourcePackageId=i,exposed=e,IPI.exposedModules=ems,IPI.hiddenModules=hms}):xs) acc= let
                        (s,m)=DM.foldrWithKey (splitMatching i) (DS.empty,DM.empty) allC
                        --(ds,deps2)=partition (\(Dependency n v)->(pkgName i == n) && withinRange (pkgVersion i) v) deps -- find if version is referenced, remove the referencing component so that it doesn't match an older version
                        cps=DS.elems s
                        --if null ds then [] else allC
                        --filter (\c->not $ null ((PD.targetBuildDepends c) `intersect` ds)) allC
                        mns=map display (ems++hms)
                        in getDep m xs ((fp,CabalPackage (display $ pkgName i) (display $ pkgVersion i) e cps mns): acc) -- build CabalPackage structure
                splitMatching pkgId comp deps (s,m)=let
                         (ds,deps2)=partition (\(Dependency n v)->(pkgName pkgId == n) && withinRange (pkgVersion pkgId) v) deps
                         s'=if null ds 
                                then s
                                else DS.insert comp s
                         m'=DM.insertWith (++) comp deps m
                         in (s',m')        
              
-- | get all components from the package description        
cabalComponentsFromDescription :: PD.PackageDescription -- ^ the package description
        -> [CabalComponent]
cabalComponentsFromDescription pd= [cabalComponentFromLibrary $ fromJust (PD.library pd)
         | isJust (PD.library pd)] ++
              map cabalComponentFromExecutable (PD.executables pd) ++
                map cabalComponentFromTestSuite (PD.testSuites pd)
             
cabalComponentsDependencies :: PD.PackageDescription -- ^ the package description
        -> DM.Map CabalComponent [Dependency]
cabalComponentsDependencies pd=let
        mLib=case  PD.library pd of
                Nothing -> DM.empty
                Just lib->DM.singleton (cabalComponentFromLibrary lib) (PD.targetBuildDepends $ PD.libBuildInfo lib)
        mExe=DM.fromList $ map (\e->((cabalComponentFromExecutable e),(PD.targetBuildDepends $ PD.buildInfo e))) (PD.executables pd)
        mTs=DM.fromList $ map (\ts->((cabalComponentFromTestSuite ts),(PD.targetBuildDepends $ PD.testBuildInfo ts))) (PD.testSuites pd)
        in DM.unionWith (++) mTs $ DM.unionWith (++) mLib mExe
        where mapDep cc bi=DM.fromList $ map (\x->(cc)) (PD.targetBuildDepends bi)



cabalComponentFromLibrary :: Library -> CabalComponent
cabalComponentFromLibrary =CCLibrary . PD.buildable . PD.libBuildInfo

cabalComponentFromExecutable :: Executable -> CabalComponent
cabalComponentFromExecutable e =CCExecutable (PD.exeName e) (PD.buildable $ PD.buildInfo e)

cabalComponentFromTestSuite :: TestSuite -> CabalComponent
cabalComponentFromTestSuite ts=CCTestSuite (PD.testName ts) (PD.buildable $ PD.testBuildInfo ts)


