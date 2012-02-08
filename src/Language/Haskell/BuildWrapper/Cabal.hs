{-# LANGUAGE PatternGuards,ScopedTypeVariables #-}
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
import qualified Data.Map as DM

import Exception (ghandle)

import Distribution.ModuleName
import Distribution.PackageDescription ( otherModules,library,executables,testSuites,Library,hsSourceDirs,libBuildInfo,Executable(..),exeName,modulePath,buildInfo,TestSuite(..),testName,TestSuiteInterface(..),testInterface,testBuildInfo,BuildInfo,cppOptions,defaultExtensions,otherExtensions,oldExtensions )
import Distribution.Simple.GHC
import Distribution.Simple.LocalBuildInfo     

import qualified Distribution.PackageDescription as PD 
import Distribution.Package
import Distribution.InstalledPackageInfo as IPI
import Distribution.Version
import Distribution.Text (display)

                    
import qualified Distribution.Simple.Configure as DSC
import qualified Distribution.Verbosity as V
import Text.Regex.TDFA

import System.Directory
import System.Exit
import System.FilePath
import System.Process

getFilesToCopy :: BuildWrapper(OpResult [FilePath])
getFilesToCopy =do
       (mfps,bwns)<-withCabal Source getAllFiles
       return $ case mfps of
                Just fps->(nub $ concatMap (\(_,_,_,_,ls)->map snd ls) fps,bwns)
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

cabalBuild :: Bool -> WhichCabal -> BuildWrapper (OpResult BuildResult)
cabalBuild output srcOrTgt= do
        dist_dir<-getDistDir
        (mr,n)<-withCabal srcOrTgt (\_->do
                cf<-getCabalFile srcOrTgt
                cp<-gets cabalPath
                v<-cabalV
                
                let args=[
                        "build",
                        "--verbose=" ++ show (fromEnum v),
                        "--builddir="++dist_dir
                        
                        ] ++ (if output 
                                then []
                                else ["--ghc-option=-c"])
                        
                liftIO $ do
                        cd<-getCurrentDirectory
                        setCurrentDirectory (takeDirectory cf)
                        (ex,out,err)<-readProcessWithExitCode cp args ""
                        putStrLn err
                        if isInfixOf "cannot satisfy -package-id" err ||  isInfixOf "re-run the 'configure'" err
                                then 
                                        return Nothing
                                else
                                        do
                                        let fps=(mapMaybe getBuiltPath (lines out))
                                        let ret=parseBuildMessages err
                                        setCurrentDirectory cd
                                        return $ Just (ex==ExitSuccess,ret,fps)
            )
        case mr of
                Nothing -> return (BuildResult False [],n)
                Just Nothing->do
                                let setup_config = DSC.localBuildInfoFile dist_dir
                                liftIO $ removeFile setup_config
                                cabalBuild output srcOrTgt
                Just (Just (r,n2,fps)) -> return (BuildResult r fps, n ++ n2)

cabalConfigure :: WhichCabal-> BuildWrapper (OpResult (Maybe LocalBuildInfo))
cabalConfigure srcOrTgt= do
        cf<-getCabalFile srcOrTgt
        cp<-gets cabalPath
        ok<-liftIO $ doesFileExist cf
        if ok 
            then 
                do
                v<-cabalV
                dist_dir<-getDistDir
                uf<-gets cabalFlags
                let args=[
                        "configure",
                        "--verbose=" ++ show (fromEnum v),
                        "--user",
                        "--enable-tests",
                        "--builddir="++dist_dir,
                        "--flags="++uf
                        ]
                liftIO $ do
                        cd<-getCurrentDirectory
                        setCurrentDirectory (takeDirectory cf)
                        (ex,_,err)<-readProcessWithExitCode cp args ""
                        putStrLn err
                        let msgs=(parseCabalMessages (takeFileName cf) (takeFileName cp) err) -- ++ (parseCabalMessages (takeFileName cf) out)
                        ret<-case ex of
                                ExitSuccess  -> do
                                        lbi<-DSC.getPersistBuildConfig dist_dir
                                        return (Just lbi,msgs)
                                ExitFailure _ -> return (Nothing, msgs)
                        setCurrentDirectory cd
                        return ret
            else do
                liftIO $ putStrLn ("cabal file"++ cf ++" does not exist")
                return (Nothing,[])       

getCabalFile :: WhichCabal -> BuildWrapper FilePath
getCabalFile Source= gets cabalFile
getCabalFile Target= fmap takeFileName (gets cabalFile)
                         >>=getTargetPath

cabalInit :: WhichCabal -> BuildWrapper (OpResult (Maybe LocalBuildInfo))
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


withCabal :: WhichCabal -> (LocalBuildInfo -> BuildWrapper a)-> BuildWrapper (OpResult (Maybe a))  
withCabal srcOrTgt f=do
        (mlbi,notes)<-cabalInit srcOrTgt
        case mlbi of
                Nothing-> return (Nothing, notes)
                Just lbi ->do
                        r<-(f lbi)
                        return (Just r, notes)
     

parseCabalMessages :: FilePath -> FilePath -> String -> [BWNote]
parseCabalMessages cf cabalExe s=let
        (m,ls)=foldl parseCabalLine (Nothing,[]) $ lines s
        in nub $ case m of
                Nothing -> ls
                Just (bwn,msgs)->ls++[makeNote bwn msgs] 
        where 
                setupExe :: String
                setupExe=addExtension "setup" $ takeExtension cabalExe
                dropPrefixes :: [String] -> String -> Maybe String
                dropPrefixes prfxs s2=foldr (stripPrefixIfNeeded s2) Nothing prfxs
                stripPrefixIfNeeded :: String -> String -> Maybe String -> Maybe String
                stripPrefixIfNeeded _ _ j@(Just _)=j
                stripPrefixIfNeeded s3 prfx  _=stripPrefix prfx s3
                parseCabalLine :: (Maybe (BWNote,[String]),[BWNote]) -> String ->(Maybe (BWNote,[String]),[BWNote])
                parseCabalLine (currentNote,ls) l 
                        | "Error:" `isPrefixOf` l=(Just (BWNote BWError "" (BWLocation cf 1 1),[dropWhile isSpace $ drop 6 l]),addCurrent currentNote ls)
                        | "Warning:" `isPrefixOf` l=let
                                msg=(dropWhile isSpace $ drop 8 l)
                                msg2=if cf `isPrefixOf` msg
                                        then dropWhile isSpace $ drop (length cf + 1) msg
                                        else msg
                                in (Just (BWNote BWWarning "" (BWLocation cf (extractLine msg2) 1),[msg2]),addCurrent currentNote ls)
                        | Just s4 <- dropPrefixes [cabalExe,setupExe] l=
                                let 
                                        s2=dropWhile isSpace $ drop 1 s4 -- drop 1 for ":" that follows file name
                                in if "At least the following" `isPrefixOf` s2
                                                then (Just (BWNote BWError "" (BWLocation cf 1 1), [s2]),addCurrent currentNote ls)
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
                                                                                        else (loc,line',tail msg')
                                                        in (Just (BWNote BWError "" (BWLocation realloc (read line) 1),[msg]),addCurrent currentNote ls)
                        | Just (jcn,msgs)<-currentNote=
                                if not $ null l
                                        then (Just (jcn,l:msgs),ls)
                                        else (Nothing,ls++[makeNote jcn msgs])
                        | otherwise =(Nothing,ls)
                addCurrent Nothing xs=xs
                addCurrent (Just (n,msgs)) xs=xs++[makeNote n msgs]
                extractLine el=let
                        (_,_,_,ls)=el =~ "\\(line ([0-9]*)\\)" :: (String,String,String,[String])
                        in if null ls
                                then 1
                                else read $ head ls
 

parseBuildMessages :: String -> [BWNote]
parseBuildMessages s=let
        (m,ls)=foldl parseBuildLine (Nothing,[]) $ lines s
        in (nub $
           case m of
               Nothing -> ls
               Just (bwn, msgs) -> ls ++ [makeNote bwn msgs])
        where 
                parseBuildLine :: (Maybe (BWNote,[String]),[BWNote]) -> String ->(Maybe (BWNote,[String]),[BWNote])
                parseBuildLine (currentNote,ls) l  
                        | Just (jcn,msgs)<-currentNote=
                                if not (null l) && (' ' == head l)
                                       then (Just (jcn,l:msgs),ls)
                                       else (Nothing,ls++[makeNote jcn msgs])
                        -- | Just fp<-getBuiltPath l=(currentNote,ls,fp:fps)             
                        | Just n<-extractLocation l=(Just (n,[bwn_title n]),ls)
                        | otherwise =(Nothing,ls)
                extractLocation el=let
                        (_,_,aft,ls)=el =~ "(.+):([0-9]+):([0-9]+):" :: (String,String,String,[String])   
                        in case ls of
                                (loc:line:col:[])-> (Just $ BWNote BWError (dropWhile isSpace aft) (BWLocation loc (read line) (read col)))
                                _ -> Nothing

makeNote :: BWNote  -> [String] ->BWNote
makeNote bwn msgs=let
        title=dropWhile isSpace $ unlines $ reverse msgs
        in if "Warning:" `isPrefixOf` title
                then bwn{bwn_title=dropWhile isSpace $ drop 8 title,bwn_status=BWWarning}    
                else bwn{bwn_title=title}      

getBuiltPath :: String -> Maybe FilePath
getBuiltPath line=let
         (_,_,_,ls)=line =~ "\\[[0-9]+ of [0-9]+\\] Compiling .+\\( (.+), (.+)\\)" :: (String,String,String,[String])   
         in case ls of
                (src:_:[])->Just src
                _ -> Nothing
          
type CabalBuildInfo=(BuildInfo,ComponentLocalBuildInfo,FilePath,Bool,[(ModuleName,FilePath)])             
            
canonicalizeBuildInfo :: CabalBuildInfo -> BuildWrapper CabalBuildInfo
canonicalizeBuildInfo (n1,n2,n3,n4,ls)=do
        lsC<-mapM (\(m,path)->do
                pathC<-canonicalizeFullPath path
                return (m,pathC)) ls
        return (n1,n2,n3,n4,lsC)
             
getBuildInfo ::  FilePath  -> BuildWrapper (OpResult (Maybe (LocalBuildInfo,CabalBuildInfo)))
getBuildInfo fp=do
        (mmr,bwns)<-go getReferencedFiles
        case mmr of
                Just (Just a)->return (Just a, bwns)
                _ -> do
                        (mmr2,bwns2)<-go getAllFiles
                        return $ case mmr2 of
                                Just (Just a)-> (Just a,bwns2)
                                _-> (Nothing,bwns)
        where 
             go f=withCabal Source (\lbi->do
                fps<-f lbi
                fpC<-canonicalizeFullPath fp
                fpsC<-mapM canonicalizeBuildInfo fps
                --liftIO $ putStrLn $ (show $ length fps)
                --liftIO $ mapM_ (\(_,_,_,_,ls)->mapM_ (putStrLn . snd) ls) fps
                let ok=filter (\(_,_,_,_,ls)->not $ null ls ) $
                        --b==fpC || b==("." </> fpC)
                        map (\(n1,n2,n3,n4,ls)->(n1,n2,n3,n4,filter (\(_,b)->equalFilePath fpC b) ls) ) 
                                fpsC
                --liftIO $ putStrLn $ (show $ length ok)      
                --liftIO $ mapM_ (\(_,_,_,_,ls)->mapM_ (putStrLn . snd) ls) ok          
                return  $ if null ok
                        then Nothing
                        else Just (lbi, head ok))
             
fileGhcOptions :: (LocalBuildInfo,CabalBuildInfo) -> BuildWrapper(ModuleName,[String])
fileGhcOptions (lbi,(bi,clbi,fp,isLib,ls))=do
        dist_dir<-getDistDir
        let inplace=dist_dir </> "package.conf.inplace"
        inplaceExist<-liftIO $ doesFileExist inplace
        let pkg
                  | isLib =
                    ["-package-name", display $ packageId $ localPkgDescr lbi]
                  | inplaceExist = ["-package-conf", inplace]
                  | otherwise = []
        return (fst $ head ls,pkg ++ ghcOptions lbi bi clbi fp)


fileCppOptions :: CabalBuildInfo -> [String]
fileCppOptions (bi,_,_,_,_)=cppOptions bi      

cabalExtensions :: CabalBuildInfo -> (ModuleName,[String])
cabalExtensions (bi,_,_,_,ls)=(fst $ head ls,map show (otherExtensions bi ++ defaultExtensions bi ++ oldExtensions bi))      
       
getSourceDirs :: BuildInfo -> [FilePath]       
getSourceDirs bi=let
        hsd=hsSourceDirs bi
        in case hsd of
                [] -> ["."]
                _ -> hsd 
       
getAllFiles :: LocalBuildInfo -> BuildWrapper [CabalBuildInfo]
getAllFiles lbi= do
                let pd=localPkgDescr lbi
                let libs=maybe [] extractFromLib $ library pd
                let exes=map extractFromExe $ executables pd
                let tests=map extractFromTest $ testSuites pd
                mapM (\(a,b,c,isLib,d)->do
                        mf<-copyAll d
                        return (a,b,c,isLib,mf)) (libs ++ exes ++ tests)
        where 
        extractFromLib :: Library -> [(BuildInfo,ComponentLocalBuildInfo,FilePath,Bool,[FilePath])]
        extractFromLib l=let
                lib=libBuildInfo l
                in [(lib, fromJustDebug "extractFromLibAll" $ libraryConfig lbi,buildDir lbi, True, getSourceDirs lib)]
        extractFromExe :: Executable -> (BuildInfo,ComponentLocalBuildInfo,FilePath,Bool,[FilePath])
        extractFromExe e@Executable{exeName=exeName'}=let
                ebi=buildInfo e
                targetDir = buildDir lbi </> exeName'
                exeDir    = targetDir </> (exeName' ++ "-tmp")
                hsd=getSourceDirs ebi
                in (ebi,fromJustDebug "extractFromExeAll" $ lookup exeName' $ executableConfigs lbi,exeDir,False, hsd) 
        extractFromTest :: TestSuite -> (BuildInfo,ComponentLocalBuildInfo,FilePath,Bool,[FilePath])
        extractFromTest t@TestSuite {testName=testName'} =let
                tbi=testBuildInfo t
                targetDir = buildDir lbi </> testName'
                testDir    = targetDir </> (testName' ++ "-tmp")
                hsd=getSourceDirs tbi
                in (tbi,fromJustDebug "extractFromTestAll" $ lookup testName' $ testSuiteConfigs lbi,testDir,False,hsd)
        copyAll :: [FilePath] -> BuildWrapper [(ModuleName,FilePath)]
        copyAll fps= do 
--                cf<-gets cabalFile
--                let dir=(takeDirectory cf)
--                ffps<-mapM getFullSrc fps
--                allF<-liftIO $ mapM getRecursiveContents ffps
--                liftIO $ putStrLn ("allF:" ++ (show allF))
--                return $ map (\f->(fromString $ fileToModule f,f)) $ map (\f->makeRelative dir f) $ concat allF
                  allF<-mapM copyAll' fps
                  return $ concat allF
        copyAll' :: FilePath -> BuildWrapper [(ModuleName,FilePath)]
        copyAll' fp=do
                cf<-gets cabalFile
                let dir=(takeDirectory cf)
                fullFP<-getFullSrc fp
                allF<-liftIO $ getRecursiveContents fullFP
                tf<-gets tempFolder
                -- exclude every file containing the temp folder name (".buildwrapper" by default)
                -- which may happen if . is a source path
                let notMyself=filter (not . isInfixOf tf) allF
                return $ map (\f->(fromString $ fileToModule $ makeRelative fullFP f,makeRelative dir f)) notMyself
     
getReferencedFiles :: LocalBuildInfo -> BuildWrapper [CabalBuildInfo]
getReferencedFiles lbi= do
                let pd=localPkgDescr lbi
                let libs=maybe [] extractFromLib $ library pd
                let exes=map extractFromExe $ executables pd
                let tests=map extractFromTest $ testSuites pd
                return (libs ++ exes ++ tests)
        where 
        extractFromLib :: Library -> [CabalBuildInfo]
        extractFromLib l=let
                lib=libBuildInfo l
                modules=PD.exposedModules l ++ otherModules lib
                in [(lib, fromJustDebug "extractFromLibRef" $ libraryConfig lbi,
                        buildDir lbi, True, copyModules modules (getSourceDirs lib))]
        extractFromExe :: Executable ->CabalBuildInfo
        extractFromExe e@Executable{exeName=exeName'}=let
                ebi=buildInfo e
                targetDir = buildDir lbi </> exeName'
                exeDir    = targetDir </> (exeName' ++ "-tmp")
                modules= (otherModules ebi)
                hsd=getSourceDirs ebi
                in (ebi,fromJustDebug "extractFromExeRef" $ lookup exeName' $ executableConfigs lbi,exeDir,False, copyMain (modulePath e) hsd ++ copyModules modules hsd ) 
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
                in (tbi,fromJustDebug ("extractFromTestRef:"++testName' ++ show (testSuiteConfigs lbi)) $ lookup testName' $ testSuiteConfigs lbi,testDir,False,extras ++ copyModules modules hsd       )
        copyModules :: [ModuleName] -> [FilePath] -> [(ModuleName,FilePath)]
        copyModules mods=copyFiles (concatMap (\m->[toFilePath m <.> "hs", toFilePath m <.> "lhs"]) mods)
        copyFiles :: [FilePath] -> [FilePath] -> [(ModuleName,FilePath)]
        copyFiles mods dirs=[(fromString $ fileToModule m,d </> m)  | m<-mods, d<-dirs]    
        copyMain :: FilePath  ->[FilePath] ->  [(ModuleName,FilePath)]
        copyMain fs = map (\ d -> (fromString "Main", d </> fs)) 
        
moduleToString :: ModuleName -> String
moduleToString = intercalate "." . components

cabalComponents :: BuildWrapper (OpResult [CabalComponent])
cabalComponents = do
     (rs,ns)<-withCabal Source (return . cabalComponentsFromDescription . localPkgDescr)
     return (fromMaybe [] rs,ns)   

cabalDependencies :: BuildWrapper (OpResult [(FilePath,[CabalPackage])])
cabalDependencies = do
     (rs,ns)<-withCabal Source (\lbi-> liftIO $
          ghandle
            (\ (e :: IOError) ->
               do (print e)
                  return [])
            $
            do pkgs <- liftIO getPkgInfos
               return $ dependencies (localPkgDescr lbi) pkgs
            )
     return (fromMaybe [] rs,ns)

        
dependencies :: PD.PackageDescription -> [(FilePath,[InstalledPackageInfo])] -> [(FilePath,[CabalPackage])]
dependencies pd pkgs=let
        pkgsMap=foldr buildPkgMap DM.empty pkgs -- build the map of package by name with ordered version (more recent first)
        allC= cabalComponentsFromDescription pd
        gdeps=PD.buildDepends pd
        cpkgs=concat $ DM.elems $ DM.map (\ipis->getDep allC ipis gdeps []) pkgsMap
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
                getDep :: [CabalComponent] -> [(FilePath,InstalledPackageInfo)] -> [Dependency]-> [(FilePath,CabalPackage)] -> [(FilePath,CabalPackage)]
                getDep _ [] _ acc= acc
                getDep allC ((fp,InstalledPackageInfo{sourcePackageId=i,exposed=e,IPI.exposedModules=ems}):xs) deps acc= let
                        (ds,deps2)=partition (\(Dependency n v)->(pkgName i == n) && withinRange (pkgVersion i) v) deps -- find if version is referenced, remove the referencing component so that it doesn't match an older version
                        cps=if null ds then [] else allC
                        mns=map display ems
                        in getDep allC xs deps2 ((fp,CabalPackage (display $ pkgName i) (display $ pkgVersion i) e cps mns): acc) -- build CabalPackage structure
                
                --DM.map (sortBy (flip (compare `on` (pkgVersion . sourcePackageId . snd)))) $ DM.fromListWith (++) (map (\i->((display $ pkgName $ sourcePackageId i),[i]) ) ipis) --concatenates all version and sort them, most recent first
        
cabalComponentsFromDescription :: PD.PackageDescription -> [CabalComponent]
cabalComponentsFromDescription pd= [CCLibrary
           (PD.buildable $ PD.libBuildInfo $ fromJust (PD.library pd))
         | isJust (PD.library pd)] ++
              [ CCExecutable (PD.exeName e) (PD.buildable $ PD.buildInfo e)
              | e <- PD.executables pd ]
               ++ [ CCTestSuite (PD.testName e) (PD.buildable $ PD.testBuildInfo e)
                | e <- PD.testSuites pd ]


