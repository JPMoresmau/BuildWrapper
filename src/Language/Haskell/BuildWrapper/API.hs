{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Language.Haskell.BuildWrapper.API where

import Control.Exception.Base
import Control.Monad.State

import Data.Maybe
import Data.Char
import Data.List

import Distribution.ModuleName
import Distribution.PackageDescription (exposedModules, otherModules,library,executables,testSuites,Library,hsSourceDirs,libBuildInfo,Executable,modulePath,buildInfo,TestSuite,TestSuiteInterface(..),testInterface,testBuildInfo )
import Distribution.PackageDescription.Parse
import qualified Distribution.Verbosity as V 
                        ( Verbosity )
import Distribution.Simple.Program 
                        ( defaultProgramConfiguration,
                          userSpecifyPaths )
import Distribution.Simple.Setup 
                        ( defaultConfigFlags, ConfigFlags(..),Flag(..)
                         )
import Distribution.Simple.LocalBuildInfo                         
import Distribution.Simple.Build
import qualified Distribution.Simple.Configure as DSC
import Distribution.Simple.PreProcess


import System.Directory
import System.Exit
import System.FilePath
import System.Process

data BuildWrapperState=BuildWrapperState{
        tempFolder::String,
        cabalPath::FilePath,
        cabalFile::FilePath,
        cabalVerbosity::V.Verbosity
        }

data BWNoteStatus=BWError | BWWarning
        deriving (Show,Read,Eq)
 
data BWLocation=BWLocation {
        bwl_src::FilePath,
        bwl_line::Int,
        bwl_col::Int
        }
        deriving (Show,Read,Eq)

data BWNote=BWNote {
        bwn_status :: BWNoteStatus,
        bwn_title :: String,
        bwn_description :: String,
        bwn_location :: BWLocation
        }
        deriving (Show,Read,Eq)

--class ToBWNote a where
--        toBWNote :: a -> BWNote

--peErrorToBWNote :: FilePath -> PError -> BWNote
--peErrorToBWNote cf (AmbigousParse t ln)= BWNote BWError "AmbigousParse" t (BWLocation cf ln 1)
--peErrorToBWNote cf (NoParse t ln)      = BWNote BWError "NoParse" t (BWLocation cf ln 1)
--peErrorToBWNote cf (TabsError ln)      = BWNote BWError "TabsError" "" (BWLocation cf ln 1)    
--peErrorToBWNote cf (FromString t mln)  = BWNote BWError "FromString" t (BWLocation cf (fromMaybe 1 mln) 1)    

type BuildWrapper=StateT BuildWrapperState IO

synchronize ::  BuildWrapper([FilePath])
synchronize =do
        cf<-gets cabalFile
        m<-copyFromMain $ takeFileName cf
        otherFiles<-getFilesToCopy
        let fileList=case otherFiles of
                Left _->[]
                Right fps->fps
        m1<-mapM copyFromMain (
                "Setup.hs":
                "Setup.lhs":
                fileList)
        return $ catMaybes (m:m1)

configure ::  BuildWrapper([BWNote])
configure = do
        synchronize
        eNL<-cabalConfigure False
        return $ case eNL of
                Left n->n
                Right _->[]


cabalConfigure :: Bool-> BuildWrapper(Either [BWNote] LocalBuildInfo)
cabalConfigure srcOrTgt= do
        cf<-getCabalFile srcOrTgt
        cp<-gets cabalPath
        v<-gets cabalVerbosity
        dist_dir<-getDistDir
       
        let args=[
                "configure",
                "--verbose="++(show $ fromEnum v),
                "--user",
                "--enable-tests",
                "--builddir="++dist_dir
                ]
        {--(_,Just hOut,Just hErr)<-liftIO $ createProcess 
                ((proc cp args){
                        cwd = Just $takeDirectory cf,
                        std_out = CreatePipe,
                        std_err = CreatePipe
                        })--}
        liftIO $ do
                cd<-getCurrentDirectory
                setCurrentDirectory (takeDirectory cf)
        
                (ex,_,err)<-readProcessWithExitCode cp args ""
                ret<-case ex of
                        ExitSuccess  -> do
                                lbi<-DSC.getPersistBuildConfig dist_dir
                                return $ Right lbi
                        ExitFailure _ -> return $ Left $ parseCabalMessages (takeFileName cf) err
                setCurrentDirectory cd
                return ret
        {-- 
        v<-gets cabalVerbosity
        gen_pkg_descr <- liftIO $ readPackageDescription v cf
        dist_dir<-getDistDir
        let prog_conf =defaultProgramConfiguration
        let user_flags = [] --getSessionSelector userFlags     
        let config_flags = 
                 (defaultConfigFlags prog_conf)
                   { configDistPref = Flag dist_dir
                   , configVerbosity = Flag v
                   , configUserInstall = Flag True
                , configTests = Flag True
              --  , configConfigurationsFlags = map (\(n,v)->(FlagName n,v)) user_flags
           }
        lbi<-liftIO $ handle (\(e :: IOError) ->  return $ Left $ BWNote BWError "Cannot configure:" (show e) (BWLocation cf 1 1)) $ do   
                lbi <- liftIO $ DSC.configure (gen_pkg_descr, (Nothing, []))
                           config_flags
                liftIO $ DSC.writePersistBuildConfig dist_dir lbi
                liftIO $ initialBuildSteps dist_dir (localPkgDescr lbi) lbi v
                            knownSuffixHandlers
                return $ Right lbi
        liftIO $ setCurrentDirectory cd
        return lbi --}
        -- cabal configure --buildir dist_dir -v=verbosity --user --enable-tests

getCabalFile :: Bool -> BuildWrapper FilePath
getCabalFile True= gets cabalFile
getCabalFile False= gets cabalFile
                         >>=return . takeFileName
                         >>=getTargetPath

cabalInit :: Bool -> BuildWrapper (Either [BWNote] LocalBuildInfo)  
cabalInit srcOrTgt= do
   cabal_file<-getCabalFile srcOrTgt
   dist_dir<-getDistDir
   let setup_config = DSC.localBuildInfoFile dist_dir
   conf'd <- liftIO $ doesFileExist setup_config
   if not conf'd 
        then cabalConfigure srcOrTgt
        else do
             cabal_time <- liftIO $ getModificationTime cabal_file
             conf_time <- liftIO $ getModificationTime setup_config
             if cabal_time >= conf_time 
                then cabalConfigure srcOrTgt
                else do
                        mb_lbi <- liftIO $ DSC.maybeGetPersistBuildConfig setup_config
                        case mb_lbi of
                          Nothing -> do
                            cabalConfigure srcOrTgt
                          Just _lbi -> do
                            return $ Right _lbi


withCabal :: Bool -> (LocalBuildInfo -> BuildWrapper (a))-> BuildWrapper (Either [BWNote] a)  
withCabal srcOrTgt f=do
        r<-cabalInit srcOrTgt
        case r of
                Left err-> do
                        liftIO $ putStrLn (show err)
                        return $ Left err
                Right lbi ->liftM Right (f lbi)
     
getFilesToCopy :: BuildWrapper(Either [BWNote] [FilePath])
getFilesToCopy = withCabal True (\gpd->do
                let pd=localPkgDescr  gpd
                let libs=maybe [] extractFromLib $ library pd
                let exes=concatMap extractFromExe $ executables pd
                let tests=concatMap extractFromTest $ testSuites pd
                return (libs ++ exes ++ tests)
        )
        where 
        extractFromLib :: Library -> [FilePath]
        extractFromLib l=let
                modules=(exposedModules l) ++ (otherModules $ libBuildInfo l)
                in copyModules modules (hsSourceDirs  $ libBuildInfo l)
        extractFromExe :: Executable -> [FilePath]
        extractFromExe e=let
                modules= (otherModules $ buildInfo e)
                hsd=hsSourceDirs  $ buildInfo e
                in (copyFiles [modulePath e] hsd)++ (copyModules modules hsd)     
        extractFromTest :: TestSuite -> [FilePath]
        extractFromTest e =let
                modules= (otherModules $ testBuildInfo e)
                hsd=hsSourceDirs  $ testBuildInfo e
                extras=case testInterface e of
                       (TestSuiteExeV10 _ mp)->(copyFiles [mp] hsd)
                       (TestSuiteLibV09 _ mn)->copyModules [mn] hsd
                       _->[]
                        in extras++ (copyModules modules hsd)
        copyModules :: [ModuleName] -> [FilePath] -> [FilePath]
        copyModules mods=copyFiles (concatMap (\m->[(toFilePath m) <.> "hs",(toFilePath m) <.> "lhs"]) mods)
        copyFiles :: [FilePath] -> [FilePath] -> [FilePath]
        copyFiles mods dirs=[d </> m  | m<-mods, d<-dirs]
        
--withCabal :: (GenericPackageDescription -> BuildWrapper a) -> BuildWrapper (Either BWNote a)
--withCabal f =do
--        cf<-gets cabalFile
--        pr<-parseCabal
--        case pr of
--                ParseOk _ a  ->(liftM Right) $ f a
--                ParseFailed p->return $ Left $ peErrorToBWNote (takeFileName cf) p  
--
--parseCabal :: BuildWrapper(ParseResult GenericPackageDescription)
--parseCabal = do
--        cf<-gets cabalFile
--        return $ parsePackageDescription cf

getDistDir ::  BuildWrapper(FilePath)
getDistDir = do
        cf<-gets cabalFile
        temp<-gets tempFolder
        let dir=(takeDirectory cf)
        return $ (dir </> temp </> "dist")

getTargetPath :: FilePath -> BuildWrapper(FilePath)
getTargetPath src=do
        cf<-gets cabalFile
        temp<-gets tempFolder
        let dir=(takeDirectory cf)
        liftIO $ createDirectoryIfMissing True (dir </> temp)
        return $ (dir </> temp </> src)

getFullSrc :: FilePath -> BuildWrapper(FilePath)
getFullSrc src=do
        cf<-gets cabalFile
        let dir=(takeDirectory cf)
        return $ (dir </> src)

copyFromMain :: FilePath -> BuildWrapper(Maybe FilePath)
copyFromMain src=do
        fullSrc<-getFullSrc src
        exSrc<-liftIO $ doesFileExist fullSrc
        if exSrc 
                then do
                        fullTgt<-getTargetPath src
                        ex<-liftIO $ doesFileExist fullTgt
                        shouldCopy<-if (not ex )
                                then return True
                                else do
                                        modSrc<-liftIO $ getModificationTime fullSrc
                                        modTgt<-liftIO $ getModificationTime fullTgt
                                        return (modSrc>modTgt)
                        if shouldCopy
                                then do
                                        liftIO $ copyFileFull fullSrc fullTgt
                                        return $ Just src
                                else return Nothing
                 else return Nothing

copyFileFull :: FilePath -> FilePath -> IO()
copyFileFull src tgt=do
        createDirectoryIfMissing True (takeDirectory tgt)
        putStrLn tgt
        copyFile src tgt

parseCabalMessages :: FilePath -> String -> [BWNote]
parseCabalMessages cf=catMaybes . (map parseCabalLine) . lines
        where 
                parseCabalLine :: String -> Maybe BWNote
                parseCabalLine s 
                        | isPrefixOf "Error:" s=Just $ BWNote BWError (dropWhile isSpace $ drop 6 s) "" (BWLocation cf 1 1)
                        | otherwise =Nothing
                