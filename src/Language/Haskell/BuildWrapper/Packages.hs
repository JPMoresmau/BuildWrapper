{-# LANGUAGE CPP, ScopedTypeVariables #-}
-- |
-- Module      : Language.Haskell.BuildWrapper.Packages
-- Copyright   : (c) Thiago Arrais 2009
-- License     : BSD3
-- 
-- Maintainer  : jpmoresmau@gmail.com
-- Stability   : beta
-- Portability : portable
-- 
-- Packages from packages databases (global, user).
-- see <http://stackoverflow.com/questions/1522104/how-to-programmatically-retrieve-ghc-package-information>
module Language.Haskell.BuildWrapper.Packages ( getPkgInfos ) where

import Language.Haskell.BuildWrapper.Base

import Prelude hiding (Maybe)
import qualified Config
import qualified System.Info
import Data.List
import Data.Maybe
import Control.Monad
import Distribution.InstalledPackageInfo
import Distribution.Text
import System.Directory
import System.Environment (getEnv)
import System.FilePath
import System.IO
import qualified Control.Exception as Exc

import GHC.Paths

import qualified Control.Exception as Exception

-- This was borrowed from the ghc-pkg source:
type InstalledPackageInfoString = InstalledPackageInfo_ String

-- | Types of cabal package databases
data CabalPkgDBType =
    PkgDirectory FilePath
  | PkgFile      FilePath

type InstalledPackagesList = [(FilePath, [InstalledPackageInfo])]

-- | Fetch the installed package info from the global and user package.conf
-- databases, mimicking the functionality of ghc-pkg.

getPkgInfos :: Maybe FilePath   -- ^ the path to the cabal-dev sandbox if any
        -> IO InstalledPackagesList
getPkgInfos msandbox= 
  let
    -- | Test for package database's presence in a given directory
    -- NB: The directory is returned for later scanning by listConf,
    -- which parses the actual package database file(s).
    lookForPackageDBIn :: FilePath -> IO (Maybe InstalledPackagesList)
    lookForPackageDBIn dir =
      let
        path_dir = dir </> "package.conf.d"
        path_file = dir </> "package.conf"
        path_sd_dir= dir </> ("packages-" ++ ghcVersion ++ ".conf")
        -- cabal sandboxes
        path_ghc_dir= dir </> currentArch ++ '-' : currentOS ++ "-ghc-" ++ ghcVersion ++ "-packages.conf.d"
                           
      in do
        exists_dir <- doesDirectoryExist path_dir
        if exists_dir
          then do
            pkgs <- readContents (PkgDirectory path_dir)
            return $ Just pkgs
          else do
            exists_file <- doesFileExist path_file
            if exists_file
              then do
                pkgs <- readContents (PkgFile path_file)
                return $ Just pkgs 
              else  do
                exists_dirSd <- doesDirectoryExist path_sd_dir
                if exists_dirSd
                  then do
                    pkgs <- readContents (PkgDirectory path_sd_dir)
                    return $ Just pkgs
                  else  do
                    exists_dirGhc <- doesDirectoryExist path_ghc_dir
                    if exists_dirGhc
                      then do
                        pkgs <- readContents (PkgDirectory path_ghc_dir)
                        return $ Just pkgs
                      else return Nothing
    currentArch :: String
    currentArch = System.Info.arch

    currentOS :: String
    currentOS = System.Info.os

    ghcVersion :: String
    ghcVersion = Config.cProjectVersion
  in do
    -- Get the global package configuration database:
    global_conf <- do
      r <- lookForPackageDBIn getLibDir
      case r of
        Nothing   -> ioError $ userError ("Can't find package database in " ++ getLibDir)
        Just pkgs -> return pkgs

    -- Get the user package configuration database
    user_conf <- case msandbox of
        Nothing -> do
            e_appdir <- Exc.try $ getAppUserDataDirectory "ghc"
            case e_appdir of
                    Left (_::Exc.IOException) -> return []
                    Right appdir -> do 
                       let subdir
                             = currentArch ++ '-' : currentOS ++ '-' : ghcVersion
                           dir = appdir </> subdir
                       r <- lookForPackageDBIn dir
                       case r of
                           Nothing -> return []
                           Just pkgs -> return pkgs
        Just sd->do
                r <- lookForPackageDBIn sd
                case r of
                           Nothing ->do
                             return []
                           Just pkgs -> return pkgs
    -- Process GHC_PACKAGE_PATH, if present:
    e_pkg_path <- Exc.try (getEnv "GHC_PACKAGE_PATH")
    env_stack <- case e_pkg_path of
        Left (_::Exc.IOException)     -> return []
        Right path -> do
          pkgs <- mapM readContents [PkgDirectory pkg | pkg <- splitSearchPath path]
          return $ concat pkgs

    -- Send back the combined installed packages list:
    return (env_stack ++ user_conf ++ global_conf)

-- | Read the contents of the given directory, searching for ".conf" files, and parse the
-- package contents. Returns a singleton list (directory, [installed packages])

readContents :: CabalPkgDBType                                  -- ^ The package database
                -> IO [(FilePath, [InstalledPackageInfo])]      -- ^ Installed packages

readContents pkgdb =
  let 
    -- | List package configuration files that might live in the given directory
    listConf :: FilePath -> IO [FilePath]
    listConf dbdir = do
      conf_dir_exists <- doesDirectoryExist dbdir
      if conf_dir_exists
        then do
          files <- getDirectoryContents dbdir
          return  [ dbdir </> file | file <- files, ".conf" `isSuffixOf` file]
        else return []

    -- | Read a file, ensuring that UTF8 coding is used for GCH >= 6.12
    readUTF8File :: FilePath -> IO String
    readUTF8File file = do
      h <- openFile file ReadMode
#if __GLASGOW_HASKELL__ >= 612
      -- fix the encoding to UTF-8
      hSetEncoding h utf8
      Exc.catch (hGetContents h) (\(err :: Exc.IOException)->do
         putStrLn $ show err
         hClose h
         h' <- openFile file ReadMode
         hSetEncoding h' localeEncoding
         hGetContents h'
         )
#else
      hGetContents h
#endif
      

    -- | This function was lifted directly from ghc-pkg. Its sole purpose is
    -- parsing an input package description string and producing an
    -- InstalledPackageInfo structure.
    convertPackageInfoIn :: InstalledPackageInfoString -> InstalledPackageInfo
    convertPackageInfoIn
        (pkgconf@(InstalledPackageInfo { exposedModules = e,
                                         hiddenModules = h })) =
            pkgconf{ exposedModules = convert e,
                     hiddenModules  = convert h }
        where convert = mapMaybe simpleParse

    -- | Utility function that just flips the arguments to Control.Exception.catch
    catchError :: IO a -> (String -> IO a) -> IO a
    catchError io handler = io `Exception.catch` handler'
        where handler' (Exception.ErrorCall err) = handler err

    -- | Slightly different approach in Cabal 1.8 series, with the package.conf.d
    -- directories, where individual package configuration files are association
    -- pairs.
    pkgInfoReader ::  FilePath
                      -> IO [InstalledPackageInfo]
    pkgInfoReader f = 
      Exc.catch (
         do
              pkgStr <- readUTF8File f
              let pkgInfo = parseInstalledPackageInfo pkgStr
              case pkgInfo of
                ParseOk _ info -> return [info]
                ParseFailed err  -> do
                        print err
                        return [emptyInstalledPackageInfo]
        ) (\(_::Exc.IOException)->return [emptyInstalledPackageInfo])
        
  in case pkgdb of
      (PkgDirectory pkgdbDir) -> do
        confs <- listConf pkgdbDir
        pkgInfoList <- mapM pkgInfoReader confs
        return [(pkgdbDir, join pkgInfoList)]

      (PkgFile dbFile) -> do
        pkgStr <- readUTF8File dbFile
        let pkgs = map convertPackageInfoIn $ readObj "InstalledPackageInfo" pkgStr
        pkgInfoList <-
          Exception.evaluate pkgs
            `catchError`
            (\e-> ioError $ userError $ "parsing " ++ dbFile ++ ": " ++ show e)
        return [(takeDirectory dbFile, pkgInfoList)]

-- GHC.Path sets libdir for us...
getLibDir :: String
getLibDir = libdir
