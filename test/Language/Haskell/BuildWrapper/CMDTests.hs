
module Language.Haskell.BuildWrapper.CMDTests where

import Language.Haskell.BuildWrapper.Tests
import Test.HUnit

import Data.Attoparsec
import Data.Aeson
import Data.Aeson.Parser
import qualified Data.ByteString.Char8 as BS
import Data.List
import System.Exit
import System.Process

cmdTests::Test
cmdTests=TestList $ map (\f->f CMDAPI) tests

data CMDAPI=CMDAPI

instance APIFacade CMDAPI where
        synchronize _ r= runAPI r "synchronize" []
        synchronize1 _ r fp= runAPI r "synchronize1" ["--file="++fp]
        write _ r fp s= runAPI r "write" ["--file="++fp,"--contents="++s]
        configure _ r t= runAPI r "configure" ["--cabaltarget="++(show t)]
        build _ r b= runAPI r "build" ["--output="++(show b)]
        getOutline _ r fp= runAPI r "outline" ["--file="++fp]
        getTokenTypes _ r fp= runAPI r "tokentypes" ["--file="++fp]
        getOccurrences _ r fp s= runAPI r "occurrences" ["--file="++fp,"--token="++s]
        getThingAtPoint _ r fp l c q t= runAPI r "thingatpoint" ["--file="++fp,"--line="++(show l),"--column="++(show c),"--qualify="++(show q),"--typed="++(show t)]
        getNamesInScope _ r fp= runAPI r "namesinscope" ["--file="++fp]

runAPI:: (FromJSON a,Show a) => FilePath -> String -> [String] -> IO a
runAPI root command args= do
        let fullargs=[command,"--tempfolder=.dist-buildwrapper","--cabalpath=cabal","--cabalfile="++(testCabalFile root)] ++ args
        (ex,out,err)<-readProcessWithExitCode "dist/build/buildwrapper/buildwrapper" fullargs ""
        --putStrLn ("out:"++out)
        --putStrLn ("err:"++err)
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
