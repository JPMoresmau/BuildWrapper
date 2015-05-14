{-# LANGUAGE PatternGuards,ScopedTypeVariables,CPP,FlexibleContexts #-}
-- |
-- Module      : Language.Haskell.BuildWrapper.Cabal
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
import Language.Haskell.Packages

import Control.Monad.State


import Data.Char
import Data.Ord (comparing)
import Data.List
import Data.Maybe

import qualified Data.Map as DM
import qualified Data.Set as DS

import Exception (ghandle)

import Distribution.ModuleName

import qualified Distribution.PackageDescription as PD
import qualified Distribution.PackageDescription.Parse as PD
import Distribution.Package
import Distribution.InstalledPackageInfo as IPI
import Distribution.Version
import Distribution.Text (display,simpleParse)
import Distribution.PackDeps

import qualified Distribution.Simple.Configure as DSC
import qualified Distribution.Verbosity as V
import Text.Regex.TDFA

import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Data.Functor.Identity (runIdentity)


import qualified Distribution.Client.Dynamic as DCD

-- | get the version of the cabal library
getCabalLibraryVersion :: String
getCabalLibraryVersion = VERSION_Cabal

-- | get all files to copy to temp folder
getFilesToCopy :: BuildWrapper(OpResult [FilePath])
getFilesToCopy =do
       (mfps,bwns)<-withCabal Source getAllFiles
       return $ case mfps of
                Just fps->(nub $ concatMap (map snd . cbiModulePaths) fps,bwns)
                Nothing ->([],bwns);

-- | get cabal verbose level
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
                logC<-gets logCabalArgs
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
                        when logC (putStrLn $ showCommandForUser cp args)
                        cd<-getCurrentDirectory
                        setCurrentDirectory (takeDirectory cf)
                        (ex,out,err)<-runAndPrint cp args
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

-- | the file where we save the targets
targetFile :: String
targetFile="dynamic_targets"

-- | run cabal configure
cabalConfigure :: WhichCabal -- ^ use original cabal or temp cabal file
        -> BuildWrapper (OpResult (Maybe [DCD.Target])) -- ^ return the build info on success, or Nothing on failure
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
                logC<-gets logCabalArgs
                copts<-gets cabalOpts
                let sb = (takeDirectory cf) </> "cabal.sandbox.config"
                hasSb <- liftIO $ doesFileExist sb
                let args=[
                        "configure",
                        "--verbose=" ++ show (fromEnum v),
                        "--enable-tests",
                        "--enable-benchmarks",
                        "--builddir="++dist_dir
                        ]
                        ++ (if null uf then [] else ["--flags="++uf])
                        ++ copts
                        ++ if hasSb then [] else ["--user"]
                liftIO $ do
                        when logC (putStrLn $ showCommandForUser cp args)
                        cd<-getCurrentDirectory
                        setCurrentDirectory (takeDirectory cf)
                        (ex,_,err)<-runAndPrint cp args
                        let msgs=parseCabalMessages (takeFileName cf) (takeFileName cp) err -- ++ (parseCabalMessages (takeFileName cf) out)
                        ret<-case ex of
                                ExitSuccess  -> if any isBWNoteError msgs
                                        then return (Nothing,msgs)
                                        else do
                                                let mghcp=fmap (tail . dropWhile (/= '=')) $ listToMaybe $ filter ("--with-ghc=" `isPrefixOf`) copts
                                                --lbi<-DSC.getPersistBuildConfig dist_dir
                                                let setup_config = DSC.localBuildInfoFile dist_dir
                                                tgs <- DCD.runQuery (DCD.on DCD.localPkgDesc DCD.targets) setup_config
                                                tgs'<- setOptions dist_dir mghcp tgs
                                                let tgsF=dist_dir </> targetFile
                                                Prelude.writeFile tgsF $ show tgs'
                                                pdmsgs <- packDepsMsgs cf
                                                return (Just tgs',msgs ++ pdmsgs)
                                ExitFailure ec -> return $ if null msgs
                                                then (Nothing,[BWNote BWError ("Cabal configure returned error code " ++ show ec) (mkEmptySpan cf 1 1)])
                                                else (Nothing, msgs)
                        setCurrentDirectory cd
                        return ret
            else do
                let err="Cabal file "++ cf ++" does not exist"
                liftIO $ putStrLn err
                return (Nothing,[BWNote BWError err (mkEmptySpan cf 0 1)])
  where
    packDepsMsgs cf = do
      mp <- loadPackage cf
      case mp of
        Nothing -> return []
        Just p  -> do
          nwst <- loadNewest
          let (_,_,deps) = checkDeps nwst p
          case deps of
            WontAccept pkgs _ -> return $ map (toMsg cf) pkgs
            _ -> return []
    toMsg cf (pkg,v)=BWNote BWWarning ("Version " ++ v ++ " of package " ++ pkg ++ " is excluded") (mkEmptySpan cf 1 1)


-- | get the full path to the cabal file
getCabalFile :: WhichCabal  -- ^ use original cabal or temp cabal file
        -> BuildWrapper FilePath
getCabalFile Source= gets cabalFile
getCabalFile Target= fmap takeFileName (gets cabalFile)
                         >>=getTargetPath

-- | get package name from cabal file
getPackageName :: BuildWrapper String
getPackageName = do
        cf<-gets cabalFile
        gpd<-liftIO $ PD.readPackageDescription V.silent cf
        return $ display $ packageId $ PD.packageDescription gpd

-- | get Cabal build info, running configure if needed
cabalInit :: WhichCabal  -- ^ use original cabal or temp cabal file
        -> BuildWrapper (OpResult (Maybe [DCD.Target]))
cabalInit srcOrTgt= do
   cabal_file<-getCabalFile srcOrTgt
   dist_dir<-getDistDir
   -- let setup_config = DSC.localBuildInfoFile dist_dir
   let tgtF=dist_dir </> targetFile
   conf'd <- liftIO $ doesFileExist tgtF
   if not conf'd
        then do
                liftIO $ putStrLn "configuring because persisted build config not present"
                cabalConfigure srcOrTgt
        else do
             cabal_time <- liftIO $ getModificationTime cabal_file
             conf_time <- liftIO $ getModificationTime tgtF
             if cabal_time > conf_time
                then do
                        liftIO $ putStrLn "configuring because persisted build config too old"
                        cabalConfigure srcOrTgt
                else do
                        tgs <-liftM tryReadList $ liftIO $ Prelude.readFile tgtF
                        -- tgs <- liftIO $ DCD.runQuery (DCD.on DCD.localPkgDesc DCD.targets) setup_config
                        --mb_lbi <- liftIO $ DSC.maybeGetPersistBuildConfig dist_dir
                        case tgs of
                          [] -> do
                            liftIO $ putStrLn "configuring because persisted build config not present"
                            cabalConfigure srcOrTgt
                          _ -> return (Just tgs, [])

-- | run a action with the cabal build info
withCabal :: WhichCabal  -- ^ use original cabal or temp cabal file
        -> ([DCD.Target] -> BuildWrapper a) -- ^ action to run if we get a build info
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
                        | Just (bw,n)<- cabalErrorLine cf cabalExe l (not (any isBWNoteError ls))=(Just (bw,n),addCurrent currentNote ls)
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

-- | get the setup exe file name
setupExe :: FilePath -- ^ path to cabal executable
        -> FilePath
setupExe cabalExe=addExtension "setup" $ takeExtension cabalExe

-- | get cabal executable from cabal-dev
fromCabalDevExe :: FilePath -- ^ path to cabal executable
        -> FilePath
fromCabalDevExe cabalExe | "cabal-dev"<-dropExtension cabalExe=addExtension "cabal" $ takeExtension cabalExe
fromCabalDevExe cabalExe=cabalExe

-- | drop all potential prefixes from the given string
dropPrefixes :: [String] -> String -> Maybe String
dropPrefixes prfxs s2=foldr (stripPrefixIfNeeded s2) Nothing prfxs

-- | stop prefix if the given string starts by it
stripPrefixIfNeeded :: String -> String -> Maybe String -> Maybe String
stripPrefixIfNeeded _ _ j@(Just _)=j
stripPrefixIfNeeded s3 prfx  _=stripPrefix prfx s3

-- | add a note with a potential additional message
addCurrent :: Maybe (BWNote, [String]) -> [BWNote] -> [BWNote]
addCurrent Nothing xs=xs
addCurrent (Just (n,msgs)) xs=xs++[makeNote n msgs]

-- | parse a Cabal error line
cabalErrorLine :: FilePath -- ^ cabal file
        -> FilePath -- ^ path to cabal executable
        -> String -- ^ line
        -> Bool -- ^ first error?
        -> Maybe (BWNote,[String])
cabalErrorLine cf cabalExe l fstErr
        | Just s4 <- dropPrefixes [cabalExe,setupExe cabalExe,fromCabalDevExe cabalExe] l=
                                let
                                        s2=dropWhile isSpace $ drop 1 s4 -- drop 1 for ":" that follows file name
                                in if "At least the following" `isPrefixOf` s2
                                                then Just (BWNote BWError "" (mkEmptySpan cf 1 1), [s2])
                                                else
                                                        let
                                                                (loc1,_)=span (/= '\'') s2
                                                                (loc2,rest2)=span (/= ':') s2
                                                                (loc,rest)=if length loc1<length loc2 then (s2,"") else (loc2,rest2)
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
         | not fstErr=cabalErrorLine cf cabalExe (cabalExe ++ ":" ++ l) False
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
                        | Just (bw,n)<- cabalErrorLine cf cabalExe l (not (any isBWNoteError ls))=(Just (bw,n),addCurrent currentNote ls)
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

-- | get a valid path
validLoc :: FilePath -- ^ the cabal file
        -> FilePath -- ^ the dist dir
        -> FilePath
        -> FilePath
validLoc cf distDir f=if distDir `isPrefixOf` f
        then cf
        else f

-- | read an integer and return a default value if not readable
readInt :: String -> Int -> Int
readInt s def=let parses=reads s ::[(Int,String)]
        in if null parses
                then def
                else fst $ head parses

-- | read a list and return the empty list if not readable
tryReadList :: Read a => String -> [a]
tryReadList s=let parses=reads s
        in if null parses
                then []
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
        cbiTarget :: DCD.Target -- ^ the target
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
getBuildInfo ::  FilePath  -- ^ the source file
        -> Maybe String -- ^ the cabal component to use, or Nothing if not specified
        -> BuildWrapper (OpResult (Maybe ([DCD.Target],CabalBuildInfo)))
getBuildInfo fp mccn=
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

-- | set the GHC options on targets
setOptions
  :: FilePath -- ^ dist directory
  -> Maybe FilePath -- ^ path to GHC if explicitely specified
  -> [DCD.Target]  -- ^ targets to set options onto
  -> IO [DCD.Target]
setOptions dist_dir mghcp tgs=do
  let setup_config = DSC.localBuildInfoFile dist_dir
  cv<-DCD.getCabalVersion setup_config
  let optStr1 | cv>=Version [1,19,0] [] ="(compiler,_ ,_)<-configure normal ("++ (show mghcp) ++ ") Nothing defaultProgramDb"
              | otherwise =""
  let optStr | cv>=Version [1,19,0] [] ="renderGhcOptions compiler $ componentGhcOptions V.silent lbi{withOptimization=NoOptimisation} b clbi fp"
             | cv>=Version [1,15,0] [] ="renderGhcOptions ((fst $ head $ readP_to_S  parseVersion  \""++VERSION_ghc++"\") :: Version) $ componentGhcOptions V.silent lbi{withOptimization=NoOptimisation} b clbi fp"
             | otherwise               ="ghcOptions lbi{withOptimization=NoOptimisation} b clbi fp"
  let withStr=if cv>=Version [1,17,0] []
                then "withAllComponentsInBuildOrder"
                else "withComponentsLBI"
  let bd=dist_dir </> "build"
  let fmp=foldr (\t m->DM.insert (DCD.targetName t) (getBuildDir bd t) m) DM.empty tgs
  let src= unlines [
                "module DynamicCabalQuery where"
                ,"import Distribution.PackageDescription"
                ,"import Distribution.Simple.LocalBuildInfo"
                ,"import Distribution.Simple.Configure (maybeGetPersistBuildConfig)"
                ,"import Data.IORef"
                ,"import qualified Data.Map as DM"
                ,"import qualified Distribution.Verbosity as V"
                ,"import Data.Version (parseVersion)"
                ,"import Text.ParserCombinators.ReadP(readP_to_S)"
                ,"import Distribution.Simple.Program.GHC"
                ,"import Distribution.Simple.GHC"
                ,"import Distribution.Version"
                ,"import Control.Monad"
                ,"import Distribution.Simple.Compiler(OptimisationLevel(..))"
                ,"import Distribution.Simple.Program.Db(defaultProgramDb)"
                ,"import Distribution.Verbosity(normal)"
                ,"import Data.Maybe"
                ,""
                ,"result :: IO (DM.Map String [String])"
                ,"result=do"
                --,"lbi<-liftM (read . Prelude.unlines . drop 1 . lines) $ Prelude.readFile "++show setup_config
                ,"Just lbi <- maybeGetPersistBuildConfig "++show dist_dir
                ,"let pkg=localPkgDescr lbi"
                ,"r<-newIORef DM.empty"
                ,"let fmp=DM."++ show fmp
                , optStr1
                ,withStr++" pkg lbi (\\c clbi->do"
                ,"       let b=componentBuildInfo c"
                ,"       let n=foldComponent (const \"\") exeName testName benchmarkName c"
                ,"       let fp=fromJust $ DM.lookup n fmp"
                ,"       let opts=" ++ optStr
                ,"       modifyIORef r (DM.insert n opts)"
                ,"       return ()"
                ,"       )"
                ,"readIORef r" ]
  m::DM.Map String [String]<-liftIO $ DCD.runRawQuery src setup_config
  return $ map (set m) tgs
  where
        set mOpts t=case DM.lookup (DCD.targetName t) mOpts of
                        Just opts->t{DCD.ghcOptions=opts}
                        _->t


-- | get GHC options for a file
fileGhcOptions :: CabalBuildInfo -- ^ the cabal info
        -> BuildWrapper [String] -- ^ the module name and the options to pass GHC
fileGhcOptions (CabalBuildInfo tgt _ isLib _ _)=do
        dist_dir<-getDistDir
        let inplace=dist_dir </> "package.conf.inplace"
        inplaceExist<-liftIO $ do
          f<-doesFileExist inplace
          d<-doesDirectoryExist inplace -- in Cabal 1.22
          return $ f || d
        n<-getPackageName
        let pkg
                  | isLib =
                    ["-package-name",n]
#if __GLASGOW_HASKELL__ < 706
                  | inplaceExist = ["-package-conf", inplace]
#else
                  | inplaceExist = ["-package-db", inplace]
#endif
                  | otherwise = []
        return (pkg ++ DCD.ghcOptions tgt)

-- | get CPP options for a file
fileCppOptions :: CabalBuildInfo -- ^ the cabal info
        -> [String] -- ^ the list of CPP options
fileCppOptions cbi=DCD.cppOptions $ cbiTarget cbi

-- | get the cabal extensions
cabalExtensions :: CabalBuildInfo -- ^ the cabal info
        -> (ModuleName,[String]) -- ^ the module name and cabal extensions
cabalExtensions CabalBuildInfo{cbiTarget=bi,cbiModulePaths=ls}=(fromJust $ fst $ head ls,DCD.extensions bi)

-- | get the source directory from a build info
getSourceDirs :: DCD.Target -- ^ the build info
        -> [FilePath]   -- ^ the source paths, guaranteed non null
getSourceDirs bi=let
        hsd=DCD.sourceDirs bi
        in case hsd of
                [] -> ["."]
                _ -> hsd

-- | get all components, referencing all the files found in the source folders
getAllFiles :: [DCD.Target] -- ^ the build info
        -> BuildWrapper [CabalBuildInfo]
getAllFiles tgs= do
                dist_dir<-getDistDir
                let bd=dist_dir </> "build"
                let libs=map (extractFromLib bd) $ filter  DCD.isLibrary tgs
                let exes=map (extractFromExe bd) $ filter DCD.isExecutable tgs
                let tests=map (extractFromTest bd) $ filter DCD.isTest tgs
                let benches=map (extractFromBench bd) $ filter DCD.isBench tgs
                cbis<-mapM (\(a,c,isLib,d,cc)->do
                        mf<-copyAll d
                        return (CabalBuildInfo a c isLib mf cc)) (libs ++ exes ++ tests ++benches)
                cbis2<-getReferencedFiles tgs
                return $ zipWith (\c1@CabalBuildInfo{cbiModulePaths=cb1} CabalBuildInfo{cbiModulePaths=cb2}->c1{cbiModulePaths=nubOrd $ cb1++cb2}) cbis cbis2
                -- return cbis
        where
        extractFromLib :: FilePath-> DCD.Target -> (DCD.Target,FilePath,Bool,[FilePath],CabalComponent)
        extractFromLib bd l=(l,bd, True, getSourceDirs l,cabalComponentFromLibrary l)
        extractFromExe :: FilePath-> DCD.Target -> (DCD.Target,FilePath,Bool,[FilePath],CabalComponent)
        extractFromExe bd e=let
                exeDir    = getBuildDir bd e
                hsd=getSourceDirs e
                in (e,exeDir,False, hsd,cabalComponentFromExecutable e)
        extractFromTest :: FilePath-> DCD.Target -> (DCD.Target,FilePath,Bool,[FilePath],CabalComponent)
        extractFromTest bd t=let
                testDir    = getBuildDir bd t
                hsd=getSourceDirs t
                in (t,testDir,False,hsd,cabalComponentFromTestSuite t)
        extractFromBench :: FilePath-> DCD.Target -> (DCD.Target,FilePath,Bool,[FilePath],CabalComponent)
        extractFromBench bd t=let
                benchDir    = getBuildDir bd t
                hsd=getSourceDirs t
                in (t,benchDir,False,hsd,cabalComponentFromBenchmark t)
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

-- | get build dir for a target
getBuildDir :: FilePath ->  DCD.Target -> FilePath
getBuildDir bd t=case DCD.info t of
                DCD.Library _-> bd
                DCD.Executable n _->bd </> n </> (n ++ "-tmp")
                DCD.TestSuite n _->bd </> n </> (n ++ "-tmp")
                DCD.BenchSuite n _->bd </> n </> (n ++ "-tmp")

-- | get all components, referencing only the files explicitely indicated in the cabal file
getReferencedFiles :: [DCD.Target] -> BuildWrapper [CabalBuildInfo]
getReferencedFiles tgs= do
                dist_dir<-getDistDir
                let bd=dist_dir </> "build"
                let libs=map (extractFromLib bd) $ filter DCD.isLibrary tgs
                let exes=map (extractFromExe bd) $ filter DCD.isExecutable tgs
                let tests=map (extractFromTest bd) $ filter DCD.isTest tgs
                let benches=map (extractFromBench bd) $ filter DCD.isBench tgs
                let cbis=libs ++ exes ++ tests ++ benches
                mapM (\c1@CabalBuildInfo{cbiModulePaths=cb1}->do
                        cb2<-filterM (\(_,f)->do
                                fs<-getFullSrc f
                                liftIO $ doesFileExist fs) cb1
                        return c1{cbiModulePaths=cb2}) cbis
        where
        extractFromLib :: FilePath -> DCD.Target -> CabalBuildInfo
        extractFromLib bd l=let
                DCD.Library mods=DCD.info l
                modules=mods ++ DCD.otherModules l
                in CabalBuildInfo l
                        bd True (copyModules modules (getSourceDirs l)) (cabalComponentFromLibrary l)
        extractFromExe :: FilePath -> DCD.Target ->CabalBuildInfo
        extractFromExe bd e=let
                DCD.Executable _ mp=DCD.info e
                exeDir = getBuildDir bd e
                modules= DCD.otherModules e
                hsd=getSourceDirs e
                in CabalBuildInfo e exeDir False (copyMain mp hsd ++ copyModules modules hsd) (cabalComponentFromExecutable e)
        extractFromTest :: FilePath -> DCD.Target -> CabalBuildInfo
        extractFromTest bd t =let
                DCD.TestSuite _ mmp=DCD.info t
                testDir = getBuildDir bd t
                modules= DCD.otherModules t
                hsd=getSourceDirs t
                extras=case mmp of
                    Just mp -> copyMain mp hsd
                    _ -> []
                in CabalBuildInfo t testDir False (extras ++ copyModules modules hsd) (cabalComponentFromTestSuite t)
        extractFromBench :: FilePath -> DCD.Target -> CabalBuildInfo
        extractFromBench bd t =let
                DCD.BenchSuite _ mmp=DCD.info t
                benchDir = getBuildDir bd t
                modules= DCD.otherModules t
                hsd=getSourceDirs t
                extras=case mmp of
                    Just mp -> copyMain mp hsd
                    _ -> []
                in CabalBuildInfo t benchDir False (extras ++ copyModules modules hsd) (cabalComponentFromBenchmark t)

        copyModules :: [String] -> [FilePath] -> [(Maybe ModuleName,FilePath)]
        copyModules mods=copyFiles (concatMap (\m->[toFilePath m <.> "hs", toFilePath m <.> "lhs"]) $ mapMaybe stringToModuleName mods)
        copyFiles :: [FilePath] -> [FilePath] -> [(Maybe ModuleName,FilePath)]
        copyFiles mods dirs=let
                rmods=filter (isJust . snd ) $ map (\x->(x,simpleParse $ fileToModule x)) mods
                in [(Just modu,d </> m)  | (m,Just modu)<-rmods, d<-dirs]
        copyMain :: FilePath  ->[FilePath] ->  [(Maybe ModuleName,FilePath)]
        copyMain fs = map (\ d -> (Just $ fromString "Main", d </> fs))

-- | parse a string into a module name
stringToModuleName :: String -> Maybe ModuleName
stringToModuleName=simpleParse

-- | convert a ModuleName to a String
moduleToString :: ModuleName -> String
moduleToString = intercalate "." . components

-- | get all components in the Cabal file
cabalComponents :: BuildWrapper (OpResult [CabalComponent])
cabalComponents = do
     (rs,ns)<-withCabal Source (return . cabalComponentsFromDescription)
     return (fromMaybe [] rs,ns)

-- | get all the dependencies in the cabal file
cabalDependencies :: Maybe FilePath   -- ^ the path to the cabal-dev sandbox if any
        -> BuildWrapper (OpResult [(FilePath,[CabalPackage])]) -- ^ the result is an array of tuples: the path to the package database, the list of packages in that db that the Cabal file references
cabalDependencies msandbox= do
     (rs,ns)<-withCabal Source (\tgs-> liftIO $
          ghandle
            (\ (e :: IOError) ->
               do print e
                  return [])
            $
            do
               pkgs <- liftIO $ getPkgInfos msandbox
               --let m=cabalComponentsDependencies tgs
               --print msandbox
               --let deps=PD.buildDepends (localPkgDescr lbi)
               --print deps
               --liftIO $ mapM_ (\d->print $ (show d) ++ (show $ DM.lookup (show $ simplifyDependency d) m)) deps
               return $ dependencies tgs pkgs
            )
     return (fromMaybe [] rs,ns)

-- | get all dependencies from the package description and the list of installed packages
dependencies ::  [DCD.Target] -- ^ the cabal description
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
                getDep :: DM.Map CabalComponent [(String, Maybe Version)] -> [(FilePath,InstalledPackageInfo)] -> [(FilePath,CabalPackage)] -> [(FilePath,CabalPackage)]
                getDep _ [] acc= acc
                getDep allC ((fp,InstalledPackageInfo{sourcePackageId=i,exposed=e,IPI.exposedModules=ems,IPI.hiddenModules=hms}):xs) acc= let
                        (s,m)=DM.foldrWithKey (splitMatching i) (DS.empty,DM.empty) allC
                        --(ds,deps2)=partition (\(Dependency n v)->(pkgName i == n) && withinRange (pkgVersion i) v) deps -- find if version is referenced, remove the referencing component so that it doesn't match an older version
                        cps=DS.elems s
                        --if null ds then [] else allC
                        --filter (\c->not $ null ((PD.targetBuildDepends c) `intersect` ds)) allC
#if MIN_VERSION_Cabal(1,22,0)
                        mns=map display (map exposedName ems ++ hms)
#else
                        mns=map display (ems++hms)
#endif
                        in getDep m xs ((fp,CabalPackage (display $ pkgName i) (display $ pkgVersion i) e cps mns): acc) -- build CabalPackage structure
                splitMatching pkgId comp deps (s,m)=let
                         (ds,_)=partition (\(n,mv)->(pkgName pkgId == PackageName n) && isJust mv && pkgVersion pkgId == fromJust mv) deps
                         s'=if null ds
                                then s
                                else DS.insert comp s
                         m'=DM.insertWith (++) comp deps m
                         in (s',m')

-- | get all components from the package description
cabalComponentsFromDescription :: [DCD.Target] -- ^ the package description
        -> [CabalComponent]
cabalComponentsFromDescription = map cabalComponentFromTarget

-- | get dependencies for all stanzas
cabalComponentsDependencies :: [DCD.Target] -- ^ the package description
        -> DM.Map CabalComponent [(String, Maybe Version)]
cabalComponentsDependencies =foldr f DM.empty
        where
                f tg =DM.insert (cabalComponentFromTarget tg) (DCD.dependencies tg)
--        let
--        mLib=case  PD.library pd of
--                Nothing -> DM.empty
--                Just lib->DM.singleton (cabalComponentFromLibrary lib) (PD.targetBuildDepends $ PD.libBuildInfo lib)
--        mExe=DM.fromList $ map (\e->((cabalComponentFromExecutable e),(PD.targetBuildDepends $ PD.buildInfo e))) (PD.executables pd)
--        mTs=DM.fromList $ map (\ts->((cabalComponentFromTestSuite ts),(PD.targetBuildDepends $ PD.testBuildInfo ts))) (PD.testSuites pd)
--        in DM.unionWith (++) mTs $ DM.unionWith (++) mLib mExe
        -- where mapDep cc bi=DM.fromList $ map (\x->(cc)) (PD.targetBuildDepends bi)

-- | convert a dynamic cabla target into a CabalComponent
cabalComponentFromTarget :: DCD.Target -> CabalComponent
cabalComponentFromTarget t=let
        b=DCD.buildable t
        in case DCD.info t of
                DCD.Library _->  CCLibrary b
                DCD.Executable n _->CCExecutable n b
                DCD.TestSuite n _->CCTestSuite n b
                DCD.BenchSuite n _->CCBenchmark n b

-- | transform a library target into a CabalComponent
cabalComponentFromLibrary :: DCD.Target -> CabalComponent
cabalComponentFromLibrary =CCLibrary . DCD.buildable

-- | transform an executable target into a CabalComponent
cabalComponentFromExecutable :: DCD.Target -> CabalComponent
cabalComponentFromExecutable e =let
        DCD.Executable exeName' _=DCD.info e
        in CCExecutable exeName' (DCD.buildable e)

-- | transform a test suite target into a CabalComponent
cabalComponentFromTestSuite :: DCD.Target -> CabalComponent
cabalComponentFromTestSuite ts=let
        DCD.TestSuite testName' _=DCD.info ts
        in CCTestSuite testName' (DCD.buildable ts)

-- | transform a benchmark target into a CabalComponent
cabalComponentFromBenchmark :: DCD.Target -> CabalComponent
cabalComponentFromBenchmark ts=let
        DCD.BenchSuite benchName' _=DCD.info ts
        in CCBenchmark benchName' (DCD.buildable ts)
