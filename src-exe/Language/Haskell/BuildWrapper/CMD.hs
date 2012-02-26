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

import Paths_buildwrapper

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Version (showVersion)


type CabalFile = FilePath
type CabalPath = FilePath
type TempFolder = FilePath

-- | all the different actions and their parameters
data BWCmd=Synchronize {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, force::Bool}
        | Synchronize1 {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, force::Bool, file:: FilePath}
        | Write {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, file:: FilePath, contents::String}  
        | Configure {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, verbosity::Verbosity,cabalTarget::WhichCabal}
        | Build {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, verbosity::Verbosity,output::Bool,cabalTarget::WhichCabal}
        | Build1 {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, file:: FilePath}
        | Outline {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, file:: FilePath} 
        | TokenTypes {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, file:: FilePath} 
        | Occurrences {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, file:: FilePath,token::String}
        | ThingAtPointCmd {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, file:: FilePath, line::Int, column::Int}
        | NamesInScope {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, file:: FilePath} 
        | Dependencies {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String}
        | Components {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String}
        | GetBuildFlags {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, file:: FilePath}
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

v :: Verbosity
v=Normal &= help "verbosity"
wc :: WhichCabal
wc=Target &= help "which cabal file to use: original or temporary"

msynchronize :: BWCmd
msynchronize = Synchronize tf cp cf uf ff
msynchronize1 :: BWCmd
msynchronize1 = Synchronize1 tf cp cf uf ff fp
mconfigure :: BWCmd
mconfigure = Configure tf cp cf uf v wc
mwrite :: BWCmd
mwrite= Write tf cp cf uf fp (def &= help "file contents")
mbuild :: BWCmd
mbuild = Build tf cp cf uf v (def &= help "output compilation and linking result") wc
mbuild1 :: BWCmd
mbuild1 = Build1 tf cp cf uf fp
mgetbf :: BWCmd
mgetbf = GetBuildFlags tf cp cf uf fp
moutline :: BWCmd
moutline = Outline tf cp cf uf fp
mtokenTypes :: BWCmd
mtokenTypes= TokenTypes tf cp cf uf fp
moccurrences :: BWCmd
moccurrences=Occurrences tf cp cf uf fp (def &= help "text to search occurrences of" &= name "token")
mthingAtPoint :: BWCmd
mthingAtPoint=ThingAtPointCmd tf cp cf uf fp 
        (def &= help "line" &= name "line")
        (def &= help "column" &= name "column")
mnamesInScope :: BWCmd
mnamesInScope=NamesInScope tf cp cf uf fp 
mdependencies :: BWCmd
mdependencies=Dependencies tf cp cf uf
mcomponents :: BWCmd
mcomponents=Components tf cp cf uf

-- | main method for command handling
cmdMain :: IO ()
cmdMain = cmdArgs
  (modes
     [msynchronize, msynchronize1, mconfigure, mwrite, mbuild, mbuild1,
      mgetbf, moutline, mtokenTypes, moccurrences, mthingAtPoint,
      mnamesInScope, mdependencies, mcomponents]
     &= helpArg [explicit, name "help", name "h"]
     &= help "buildwrapper executable"
     &= program "buildwrapper"
     &=
     summary
       ("buildwrapper executable, version " ++ showVersion version))
  >>= handle
        where 
                handle ::BWCmd -> IO ()
                handle c@Synchronize{force=f}=runCmd c (synchronize f)
                handle c@Synchronize1{force=f,file=fi}=runCmd c (synchronize1 f fi)
                handle c@Write{file=fi,contents=s}=runCmd c (write fi s)
                handle c@Configure{cabalTarget=w}=runCmd c (configure w)
                handle c@Build{verbosity=ve,output=o,cabalTarget=w}=runCmdV ve c (build o w)
                handle c@Build1{file=fi}=runCmd c (build1 fi)
                handle c@GetBuildFlags{file=fi}=runCmd c (getBuildFlags fi)
                handle c@Outline{file=fi}=runCmd c (getOutline fi)
                handle c@TokenTypes{file=fi}=runCmd c (getTokenTypes fi)
                handle c@Occurrences{file=fi,token=t}=runCmd c (getOccurrences fi t)
                handle c@ThingAtPointCmd{file=fi,line=l,column=co}=runCmd c (getThingAtPoint fi l co)
                handle c@NamesInScope{file=fi}=runCmd c (getNamesInScope fi)
                handle c@Dependencies{}=runCmd c getCabalDependencies
                handle c@Components{}=runCmd c getCabalComponents
                runCmd :: (ToJSON a) => BWCmd -> StateT BuildWrapperState IO a -> IO ()
                runCmd=runCmdV Normal
                runCmdV:: (ToJSON a) => Verbosity -> BWCmd -> StateT BuildWrapperState IO a -> IO ()
                runCmdV vb cmd f=evalStateT f (BuildWrapperState (tempFolder cmd) (cabalPath cmd) (cabalFile cmd) vb (cabalFlags cmd))
                                >>= BSC.putStrLn . BS.append "build-wrapper-json:" . encode
                        