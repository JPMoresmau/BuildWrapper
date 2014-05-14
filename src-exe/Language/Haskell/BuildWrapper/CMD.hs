{-# LANGUAGE DeriveDataTypeable,OverloadedStrings, NamedFieldPuns, CPP #-}
-- |
-- Module      : Language.Haskell.BuildWrapper.CMD
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
import Language.Haskell.BuildWrapper.Cabal (getCabalLibraryVersion)
import Language.Haskell.BuildWrapper.Base hiding (tempFolder,cabalPath, cabalFile, cabalFlags,verbosity)
import Control.Monad.State
import System.Console.CmdArgs hiding (Verbosity(..),verbosity)
import System.Directory (canonicalizePath)
import Paths_buildwrapper
import System.IO

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Version (showVersion)
import Control.Exception.Base (catch, IOException)
#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif

type CabalFile = FilePath
type CabalPath = FilePath
type TempFolder = FilePath

-- | all the different actions and their parameters
data BWCmd=Synchronize {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], force::Bool, logCabal::Bool}
        | Synchronize1 {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], force::Bool, file:: FilePath, logCabal::Bool}
        | Write {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath, contents::String, logCabal::Bool}  
        | Configure {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], verbosity::Verbosity,cabalTarget::WhichCabal, logCabal::Bool}
        | Build {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], verbosity::Verbosity,output::Bool,cabalTarget::WhichCabal, logCabal::Bool}
        | Build1 {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath, component:: Maybe String, longRunning :: Bool, logCabal::Bool}
        | Outline {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath, component:: Maybe String, logCabal::Bool} 
        | TokenTypes {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath, component:: Maybe String, logCabal::Bool} 
        | Occurrences {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath,token::String, component:: Maybe String, logCabal::Bool}
        | ThingAtPointCmd {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath, line::Int, column::Int, component:: Maybe String, logCabal::Bool}
        | Locals {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath, sline::Int, scolumn::Int,eline::Int, ecolumn::Int, component:: Maybe String, logCabal::Bool}
        | Eval {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath, expression::String, component:: Maybe String, logCabal::Bool}
        | NamesInScope {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath, component:: Maybe String, logCabal::Bool} 
        | Dependencies {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], sandbox::FilePath, logCabal::Bool}
        | Components {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], logCabal::Bool}
        | GetBuildFlags {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath, component:: Maybe String, logCabal::Bool}
        | GenerateUsage {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], returnAll:: Bool, cabalComponent::String, logCabal::Bool}
        | CleanImports {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], file:: FilePath, format :: Bool, component:: Maybe String, logCabal::Bool}
        | Clean {tempFolder::TempFolder, cabalPath::CabalPath, cabalFile::CabalFile, cabalFlags::String, cabalOption::[String], everything:: Bool, logCabal::Bool}
        
    deriving (Show,Read,Data,Typeable)    
  

tf :: TempFolder
tf=".dist-buildwrapper" &= typDir &= help "temporary folder, relative to cabal file folder"
cp :: CabalPath
cp="cabal" &= typFile &= help "location of cabal executable" 
cf :: CabalFile
cf=def &= typFile &= help "cabal file" 
fp :: FilePath
fp=def &= typFile &= help "relative path of file to process"
sd :: FilePath
sd=def &= typDir &= help "path of the sandbox"
ff :: Bool
ff=def &= help "overwrite newer file"
uf :: String
uf=def &= help "user cabal flags"
co :: [String]
co=def &= help "cabal extra parameters"

lc :: Bool
lc=def &= help "log calls to cabal?"

formatF :: Bool
formatF=def &= help "format imports"

longRunningF :: Bool
longRunningF=def &= help "long running process"

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
msynchronize = Synchronize tf cp cf uf co ff lc
msynchronize1 :: BWCmd
msynchronize1 = Synchronize1 tf cp cf uf co ff fp lc
mconfigure :: BWCmd
mconfigure = Configure tf cp cf uf co v wc lc
mwrite :: BWCmd
mwrite= Write tf cp cf uf co fp (def &= help "file contents") lc
mbuild :: BWCmd
mbuild = Build tf cp cf uf co v (def &= help "output compilation and linking result") wc lc
mbuild1 :: BWCmd
mbuild1 = Build1 tf cp cf uf co fp mcc longRunningF lc
mgetbf :: BWCmd
mgetbf = GetBuildFlags tf cp cf uf co fp mcc lc
mcleanimports :: BWCmd
mcleanimports = CleanImports tf cp cf uf co fp formatF mcc lc
moutline :: BWCmd
moutline = Outline tf cp cf uf co fp mcc lc
mtokenTypes :: BWCmd
mtokenTypes= TokenTypes tf cp cf uf co fp mcc lc
moccurrences :: BWCmd
moccurrences=Occurrences tf cp cf uf co fp (def &= help "text to search occurrences of" &= name "token") mcc lc
mthingAtPoint :: BWCmd
mthingAtPoint=ThingAtPointCmd tf cp cf uf co fp 
        (def &= help "line" &= name "line")
        (def &= help "column" &= name "column") mcc lc
mlocals :: BWCmd
mlocals=Locals tf cp cf uf co fp 
        (def &= help "start line" &= name "start line")
        (def &= help "start column" &= name "start column")
        (def &= help "end line" &= name "end line")
        (def &= help "end column" &= name "end column") mcc lc
meval :: BWCmd
meval=Eval tf cp cf uf co fp 
        (def &= help "expression to evaluation" &= name "expression")
        mcc lc
mnamesInScope :: BWCmd
mnamesInScope=NamesInScope tf cp cf uf co fp mcc lc
mdependencies :: BWCmd
mdependencies=Dependencies tf cp cf uf co sd lc
mcomponents :: BWCmd
mcomponents=Components tf cp cf uf co lc
mgenerateUsage :: BWCmd
mgenerateUsage=GenerateUsage tf cp cf uf co ra cc lc
mclean :: BWCmd
mclean=Clean tf cp cf uf co (def &= help "delete everything or only generated files" &= name "everything") lc

-- | main method for command handling
cmdMain :: IO ()
cmdMain = cmdArgs
  (modes
     [msynchronize, msynchronize1, mconfigure, mwrite, mbuild, mbuild1,
      mgetbf,mcleanimports, moutline, mtokenTypes, moccurrences, mthingAtPoint, mlocals, meval,
      mnamesInScope, mdependencies, mcomponents, mgenerateUsage,mclean]
     &= helpArg [explicit, name "help", name "h"]
     &= help "buildwrapper executable"
     &= program "buildwrapper"
     &=
     summary
       ("buildwrapper executable, version " ++ showVersion version++ "\nusing version "++getCabalLibraryVersion ++" of the Cabal library"))
  >>= handle
        where   handle ::BWCmd -> IO ()
                handle c@Synchronize{force=f}=runCmd c (synchronize f)
                handle c@Synchronize1{force=f,file=fi}=runCmd c (synchronize1 f fi)
                handle c@Write{file=fi,contents=s}=runCmd c (write fi s)
                handle c@Configure{verbosity=ve,cabalTarget=w}=runCmdV ve c (configure w)
                handle c@Build{verbosity=ve,output=o,cabalTarget=w}=runCmdV ve c (build o w)
                handle c@Build1{file=fi,component=mcomp}= if longRunning c
                                then runCmd c $ build1LongRunning fi mcomp 
                                else runCmd c $ build1 fi mcomp
                handle c@GetBuildFlags{file=fi,component=mcomp}=runCmd c (getBuildFlags fi mcomp)
                handle c@CleanImports{file=fi,format=fo,component=mcomp}=runCmd c (cleanImports fi fo mcomp)
                handle c@Outline{file=fi,component=mcomp}=runCmd c (getOutline fi mcomp)
                handle c@TokenTypes{file=fi}=runCmd c (getTokenTypes fi)
                handle c@Occurrences{file=fi,token=t,component=mcomp}=runCmd c (getOccurrences fi t mcomp)
                handle c@ThingAtPointCmd{file=fi,line=l,column=col,component=mcomp}=runCmd c (getThingAtPoint fi l col mcomp)
                handle c@Locals{file=fi,sline=sl,scolumn=scol,eline=el,ecolumn=ecol,component=mcomp}=runCmd c (getLocals fi sl scol el ecol mcomp)
                handle c@Eval{file=fi,expression=expr,component=mcomp}=runCmd c (evalExpression fi expr mcomp)
                handle c@NamesInScope{file=fi,component=mcomp}=runCmd c (getNamesInScope fi mcomp)
                handle c@Dependencies{sandbox=sd'}=runCmd c (getCabalDependencies sd')
                handle c@Components{}=runCmd c getCabalComponents
                handle c@GenerateUsage{returnAll=reta,cabalComponent=comp}=runCmd c (generateUsage reta comp)
                handle c@Clean{everything=e}=runCmd c (clean e)
                runCmd :: (ToJSON a) => BWCmd -> StateT BuildWrapperState IO a -> IO ()
                runCmd=runCmdV Normal
                runCmdV:: (ToJSON a) => Verbosity -> BWCmd -> StateT BuildWrapperState IO a -> IO ()
                runCmdV vb  cmd f=
                 do { cabalFile' <- canonicalizePath (cabalFile cmd) `catch` ((\_->return $ cabalFile cmd)::(IOException -> IO String)) -- canonicalize cabal-file path because Eclipse does not correctly keep track of case changes on the project path, but for preview the file does not exist!
                    ; resultJson <- evalStateT f (BuildWrapperState (tempFolder cmd) (cabalPath cmd) cabalFile' vb (cabalFlags cmd) (cabalOption cmd) (logCabal cmd))
                    ;  hFlush stdout; hFlush stderr;  BSC.putStrLn "" -- ensure streams are flushed, and prefix and start of the line
                    ; BSC.putStrLn . BS.append "build-wrapper-json:" . encode $ resultJson
                    }