{-# LANGUAGE CPP #-}
-- |
-- Module      : Language.Haskell.BuildWrapper.CMDTests
-- Author      : JP Moresmau
-- Copyright   : (c) JP Moresmau 2011
-- License     : BSD3
-- 
-- Maintainer  : jpmoresmau@gmail.com
-- Stability   : beta
-- Portability : portable
-- 
-- Testing via the executable interface
module Language.Haskell.BuildWrapper.CMDTests where

import Language.Haskell.BuildWrapper.Tests
import Test.HUnit

import Control.Monad

import Data.Attoparsec
import Data.Aeson
import Data.Aeson.Parser
import qualified Data.ByteString.Char8 as BS
import Data.List
import System.Exit
import System.Process
import System.FilePath
import System.Directory

cmdTests::[Test]
cmdTests= map (\f->f CMDAPI) tests

data CMDAPI=CMDAPI

instance APIFacade CMDAPI where
        synchronize _ r ff= runAPI r "synchronize" ["--force="++(show ff)]
        synchronize1 _ r ff fp= runAPI r "synchronize1" ["--force="++(show ff),"--file="++fp]
        write _ r fp s= runAPI r "write" ["--file="++fp,"--contents="++s]
        configure _ r t= runAPI r "configure" ["--cabaltarget="++(show t)]
        build _ r b wc= runAPI r "build" ["--output="++(show b),"--cabaltarget="++(show wc)]
        build1 _ r fp= runAPI r "build1" ["--file="++fp]
        getOutline _ r fp= runAPI r "outline" ["--file="++fp]
        getTokenTypes _ r fp= runAPI r "tokentypes" ["--file="++fp]
        getOccurrences _ r fp s= runAPI r "occurrences" ["--file="++fp,"--token="++s]
        getThingAtPoint _ r fp l c q t= runAPI r "thingatpoint" ["--file="++fp,"--line="++(show l),"--column="++(show c),"--qualify="++(show q),"--typed="++(show t)]
        getNamesInScope _ r fp= runAPI r "namesinscope" ["--file="++fp]
        getCabalDependencies _ r= runAPI r "dependencies" []
        getCabalComponents _ r= runAPI r "components" []
        
exeExtension :: String
#ifdef mingw32_HOST_OS
exeExtension = "exe"
#else
exeExtension = ""
#endif        
        
runAPI:: (FromJSON a,Show a) => FilePath -> String -> [String] -> IO a
runAPI root command args= do
        let fullargs=[command,"--tempfolder=.dist-buildwrapper","--cabalpath=cabal","--cabalfile="++(testCabalFile root)] ++ args
        exePath<-filterM doesFileExist [".dist-buildwrapper/dist/build/buildwrapper/buildwrapper" <.> exeExtension,"dist/build/buildwrapper/buildwrapper" <.> exeExtension]
        (ex,out,err)<-readProcessWithExitCode (head exePath) fullargs ""
        putStrLn ("out:"++out)
        putStrLn ("err:"++err)
        assertEqual ("returned error: "++show fullargs++"\n:"++show err) ExitSuccess ex
        let res=map (drop $ length "build-wrapper-json:") $ filter (isPrefixOf "build-wrapper-json:") $ lines out
        assertEqual ("no json: "++show fullargs++"\n:"++show out) 1 (length res)
        let r=parse value $ BS.pack (head res)
        case r of
                Done _ js->do 
                        let r1= fromJSON js
                        case r1 of 
                                Success fin->return fin
                                a->do
                                        assertFailure (show a)
                                        error ""
                a->do
                        assertFailure (show a)
                        error ""
