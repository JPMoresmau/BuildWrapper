{-# LANGUAGE DeriveDataTypeable,OverloadedStrings #-}
-- |
-- Module      : Language.Haskell.BuildWrapper.CMD
-- Author      : JP Moresmau
-- Copyright   : (c) JP Moresmau 2011
-- License     : BSD3
-- 
-- Maintainer  : jpmoresmau@gmail.com
-- Stability   : beta
-- Portability : portable
-- 
-- CmdArgs configuration for executable option handling
module Language.Haskell.BuildWrapper.CMD where

import Language.Haskell.BuildWrapper.API
import Language.Haskell.BuildWrapper.Base hiding (tempFolder,cabalPath, cabalFile, cabalFlags,verbosity)
import Control.Monad.State
import System.Console.CmdArgs hiding (Verbosity(..),verbosity)
import System.Directory (canonicalizePath)
import Paths_buildwrapper

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Version (showVersion)


type CabalFile = FilePath
type CabalPath = FilePath
type TempFolder = FilePath

-- | all the different actions and their parameters
data BWCmd=Synchronize {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], force::Bool}
        | Synchronize1 {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], force::Bool, file:: FilePath}
        | Write {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath, contents::String}  
        | Configure {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], verbosity::Verbosity,cabalTarget::WhichCabal}
        | Build {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], verbosity::Verbosity,output::Bool,cabalTarget::WhichCabal}
        | Build1 {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath, component:: Maybe String}
        | Outline {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath, component:: Maybe String} 
        | TokenTypes {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath, component:: Maybe String} 
        | Occurrences {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath,token::String, component:: Maybe String}
        | ThingAtPointCmd {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath, line::Int, column::Int, component:: Maybe String}
        | NamesInScope {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath, component:: Maybe String} 
        | Dependencies {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String]}
        | Components {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String]}
        | GetBuildFlags {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath, component:: Maybe String}
        | GenerateUsage {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], returnAll:: Bool, cabalComponent::String}
    deriving (Show,Read,Data,Typeable)    
  

tf :: TempFolder
tf=".dist-buildwrapper" &= typDir &= help "temporary folder, relative to cabal file folder"
cp :: CabalPath
cp="cabal" &= typFile &= help "location of cabal executable" 
cf :: CabalFile
cf=def &= typFile &= help "cabal file" 
fp :: FilePath
fp=def &= typFile &= help "relative path of file to process"
ff :: Bool
ff=def &= help "overwrite newer file"
uf :: String
uf=def &= help "user cabal flags"
co :: [String]
co=def &= help "cabal extra parameters"

v :: Verbosity
v=Normal &= help "verbosity"
wc :: WhichCabal
wc=Target &= help "which cabal file to use: original or temporary"

cc :: String
cc=def &= help "cabal component"

mcc :: Maybe String
mcc=def &= help "cabal component"

ra :: Bool
ra=def &= help "return all source paths"

msynchronize :: BWCmd
msynchronize = Synchronize tf cp cf uf co ff
msynchronize1 :: BWCmd
msynchronize1 = Synchronize1 tf cp cf uf co ff fp
mconfigure :: BWCmd
mconfigure = Configure tf cp cf uf co v wc
mwrite :: BWCmd
mwrite= Write tf cp cf uf co fp (def &= help "file contents")
mbuild :: BWCmd
mbuild = Build tf cp cf uf co v (def &= help "output compilation and linking result") wc
mbuild1 :: BWCmd
mbuild1 = Build1 tf cp cf uf co fp mcc
mgetbf :: BWCmd
mgetbf = GetBuildFlags tf cp cf uf co fp mcc
moutline :: BWCmd
moutline = Outline tf cp cf uf co fp mcc
mtokenTypes :: BWCmd
mtokenTypes= TokenTypes tf cp cf uf co fp mcc
moccurrences :: BWCmd
moccurrences=Occurrences tf cp cf uf co fp (def &= help "text to search occurrences of" &= name "token") mcc
mthingAtPoint :: BWCmd
mthingAtPoint=ThingAtPointCmd tf cp cf uf co fp 
        (def &= help "line" &= name "line")
        (def &= help "column" &= name "column") mcc
mnamesInScope :: BWCmd
mnamesInScope=NamesInScope tf cp cf uf co fp mcc
mdependencies :: BWCmd
mdependencies=Dependencies tf cp cf uf co
mcomponents :: BWCmd
mcomponents=Components tf cp cf uf co
mgenerateUsage :: BWCmd
mgenerateUsage=GenerateUsage tf cp cf uf co ra cc

-- | main method for command handling
cmdMain :: IO ()
cmdMain = cmdArgs
  (modes
     [msynchronize, msynchronize1, mconfigure, mwrite, mbuild, mbuild1,
      mgetbf, moutline, mtokenTypes, moccurrences, mthingAtPoint,
      mnamesInScope, mdependencies, mcomponents, mgenerateUsage]
     &= helpArg [explicit, name "help", name "h"]
     &= help "buildwrapper executable"
     &= program "buildwrapper"
     &=
     summary
       ("buildwrapper executable, version " ++ showVersion version))
  >>= handle
        where   handle ::BWCmd -> IO ()
                handle c@Synchronize{force=f}=runCmd c (synchronize f)
                handle c@Synchronize1{force=f,file=fi}=runCmd c (synchronize1 f fi)
                handle c@Write{file=fi,contents=s}=runCmd c (write fi s)
                handle c@Configure{cabalTarget=w}=runCmd c (configure w)
                handle c@Build{verbosity=ve,output=o,cabalTarget=w}=runCmdV ve c (build o w)
                handle c@Build1{file=fi,component=mcomp}=runCmd c (build1 fi mcomp)
                handle c@GetBuildFlags{file=fi,component=mcomp}=runCmd c (getBuildFlags fi mcomp)
                handle c@Outline{file=fi,component=mcomp}=runCmd c (getOutline fi mcomp)
                handle c@TokenTypes{file=fi}=runCmd c (getTokenTypes fi)
                handle c@Occurrences{file=fi,token=t,component=mcomp}=runCmd c (getOccurrences fi t mcomp)
                handle c@ThingAtPointCmd{file=fi,line=l,column=col,component=mcomp}=runCmd c (getThingAtPoint fi l col mcomp)
                handle c@NamesInScope{file=fi,component=mcomp}=runCmd c (getNamesInScope fi mcomp)
                handle c@Dependencies{}=runCmd c getCabalDependencies
                handle c@Components{}=runCmd c getCabalComponents
                handle c@GenerateUsage{returnAll=reta,cabalComponent=comp}=runCmd c (generateUsage reta comp)
                runCmd :: (ToJSON a) => BWCmd -> StateT BuildWrapperState IO a -> IO ()
                runCmd=runCmdV Normal
                runCmdV:: (ToJSON a) => Verbosity -> BWCmd -> StateT BuildWrapperState IO a -> IO ()
                runCmdV vb cmd f=
                 do { cabalFile' <- canonicalizePath $ cabalFile cmd -- canonicalize cabal-file path because Eclipse does not correctly keep track of case changes on the project path
                    ; resultJson <- evalStateT f (BuildWrapperState (tempFolder cmd) (cabalPath cmd) cabalFile' vb (cabalFlags cmd) (cabalOption cmd))
                    ; BSC.putStrLn . BS.append "build-wrapper-json:" . encode $ resultJson
                    }