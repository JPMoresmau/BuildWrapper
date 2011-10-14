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
import Language.Haskell.BuildWrapper.Base
import Control.Monad.State
import System.Console.CmdArgs hiding (Verbosity(..))

import Data.Aeson
import qualified Data.ByteString.Lazy as BS


type CabalFile = FilePath
type CabalPath = FilePath
type TempFolder = FilePath

data BWCmd=Synchronize {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, force::Bool}
        | Synchronize1 {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, force::Bool, file:: FilePath}
        | Write {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, file:: FilePath, contents::String}  
        | Configure {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, verbosity::Verbosity,cabalTarget::WhichCabal}
        | Build {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, verbosity::Verbosity,output::Bool,cabalTarget::WhichCabal}
        | Build1 {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, file:: FilePath}
        | Outline {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, file:: FilePath} 
        | TokenTypes {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, file:: FilePath} 
        | Occurrences {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, file:: FilePath,token::String}
        | ThingAtPoint {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, file:: FilePath, line::Int, column::Int, qualify::Bool, typed::Bool}
        | NamesInScope {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, file:: FilePath} 
        | Dependencies {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String}
        | Components {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String}
    deriving (Show,Read,Data,Typeable)    
  
 
tf=".dist-buildwrapper" &= typDir &= help "temporary folder, relative to cabal file folder"
cp="cabal" &= typFile &= help "location of cabal executable" 
cf=def &= typFile &= help "cabal file" 
fp=def &= typFile &= help "relative path of file to process"
ff=def &= help "overwrite newer file"
uf=def &= help "user cabal flags"

v=Normal &= help "verbosity"
wc=Target &= help "which cabal file to use: original or temporary"

msynchronize = Synchronize tf cp cf uf ff
msynchronize1 = Synchronize1 tf cp cf uf ff fp
mconfigure = Configure tf cp cf uf v wc
mwrite= Write tf cp cf uf fp (def &= help "file contents")
mbuild = Build tf cp cf uf v (def &= help "output compilation and linking result") wc
mbuild1 = Build1 tf cp cf uf fp
moutline = Outline tf cp cf uf fp
mtokenTypes= TokenTypes tf cp cf uf fp
moccurrences=Occurrences tf cp cf uf fp (def &= help "text to search occurrences of" &= name "token")
mthingAtPoint=ThingAtPoint tf cp cf uf fp 
        (def &= help "line" &= name "line")
        (def &= help "column" &= name "column")
        (def &= help "qualify results")
        (def &= help "type results")
mnamesInScope=NamesInScope tf cp cf uf fp 
mdependencies=Dependencies tf cp cf uf
mcomponents=Components tf cp cf uf

cmdMain = (cmdArgs $ 
                modes [msynchronize, msynchronize1, mconfigure,mwrite,mbuild,mbuild1, moutline, mtokenTypes,moccurrences,mthingAtPoint,mnamesInScope,mdependencies,mcomponents]
                &= helpArg [explicit, name "help", name "h"]
                &= help "buildwrapper executable"
                &= program "buildwrapper"
                )
        >>= handle
        where 
                handle ::BWCmd -> IO ()
                handle (Synchronize tf cp cf uf ff )=run tf cp cf uf (synchronize ff)
                handle (Synchronize1 tf cp cf uf ff fp)=run tf cp cf uf (synchronize1 ff fp)
                handle (Write tf cp cf uf fp s)=run tf cp cf uf (write fp s)
                handle (Configure tf cp cf uf v wc)=runV v tf cp cf uf (configure wc)
                handle (Build tf cp cf uf v output wc)=runV v tf cp cf uf (build output wc)
                handle (Build1 tf cp cf uf fp)=runV v tf cp cf uf (build1 fp)
                handle (Outline tf cp cf uf fp)=run tf cp cf uf (getOutline fp)
                handle (TokenTypes tf cp cf uf fp)=run tf cp cf uf (getTokenTypes fp)
                handle (Occurrences tf cp cf uf fp token)=run tf cp cf uf (getOccurrences fp token)
                handle (ThingAtPoint tf cp cf uf fp line column qual typed)=run tf cp cf uf (getThingAtPoint fp line column qual typed)
                handle (NamesInScope tf cp cf uf fp)=run tf cp cf uf (getNamesInScope fp)
                handle (Dependencies tf cp cf uf)=run tf cp cf uf getCabalDependencies
                handle (Components tf cp cf uf)=run tf cp cf uf getCabalComponents
                run:: (ToJSON a) => FilePath -> FilePath -> FilePath -> String -> StateT BuildWrapperState IO a -> IO ()
                run = runV Normal
                runV:: (ToJSON a) => Verbosity -> FilePath -> FilePath -> FilePath -> String -> StateT BuildWrapperState IO a -> IO ()
                runV v tf cp cf uf f=(evalStateT f (BuildWrapperState tf cp cf v uf))
                                >>= BS.putStrLn . BS.append "build-wrapper-json:" . encode
                        