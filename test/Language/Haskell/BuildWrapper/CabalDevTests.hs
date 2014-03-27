{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- |
-- Module      : Language.Haskell.BuildWrapper.CabalDevTests
-- Copyright   : (c) JP Moresmau 2013
-- License     : BSD3
-- 
-- Maintainer  : jpmoresmau@gmail.com
-- Stability   : beta
-- Portability : portable
-- 
-- Test cabal dev invocations
module Language.Haskell.BuildWrapper.CabalDevTests where

import Language.Haskell.BuildWrapper.Base hiding (readFile,writeFile)

import Language.Haskell.BuildWrapper.CMDTests

import Data.Maybe

import System.Directory                        (createDirectory, doesDirectoryExist, getCurrentDirectory, getTemporaryDirectory, removeDirectoryRecursive, setCurrentDirectory)
import System.FilePath                         ((<.>), (</>))

import Control.Monad                           (when)

import Test.Framework                          (assertBool_, assertEqual_, makeLoc, makeTestSuite, makeUnitTest, TestSuite)                         
import Test.HUnit                              (Assertion)                    
import System.Exit                             (ExitCode (ExitSuccess))
import System.Process                          (readProcessWithExitCode) 
import Data.List (isInfixOf)

test_CabalDev2Projects :: Assertion
test_CabalDev2Projects= do
        let api=cabalDevAPI
        (root1,root2)<-createTestProjects
        runCabalDev root2 ["install",root1]
        runCabalDev root2 ["install-deps"]
        ((fps,dels),_)<-synchronize api root2 False
        assertBool (not $ null fps) 
        assertBool (null dels) 
        configure api root2 Source
        (BuildResult bool1b _,nsErrors1b)<-build api root2 True Source
        assertBool bool1b
        assertEqual 0 (length nsErrors1b)
        let relB="src"</>"B.hs"
        (mtts1,nsErrors1)<-getNamesInScope api root2 relB
        assertBool (null nsErrors1)
        assertBool (isJust mtts1)
        let tts1=fromJust mtts1
        assertBool ("B.fB" `elem` tts1)
        assertBool ("A.fA1" `elem` tts1)
        let relA="src"</>"A.hs"
        writeFile (root1 </> relA) $ unlines [
                "module A where",
                "fA1=reverse",
                "fA2=head"
                ]
        synchronize api root1 False
        runCabalDev root2 ["install",root1,"--force-reinstalls"] 
        synchronize api root2 False
        configure api root2 Source    
        (BuildResult bool2b _,nsErrors2b)<-build api root2 True Source
        assertBool bool2b
        assertEqual 0 (length nsErrors2b)
        (mtts2,nsErrors2)<-getNamesInScope api root2 relB
        assertBool (null nsErrors2)
        assertBool (isJust mtts2)
        let tts2=fromJust mtts2
        assertBool ("B.fB" `elem` tts2)
        assertBool ("A.fA1" `elem` tts2)
        assertBool ("A.fA2" `elem` tts2)
        return ()

test_CabalDevDependencies :: Assertion
test_CabalDevDependencies= do
        let api=cabalDevAPI
        (root1,root2)<-createTestProjects
        runCabalDev root2 ["install",root1] --,"-s","cabal-dev"
        runCabalDev root2 ["install-deps"]
        ((fps,dels),_)<-synchronize api root2 False
        assertBool (not $ null fps) 
        assertBool (null dels) 
        configure api root2 Source
        (BuildResult bool1b _,nsErrors1b)<-build api root2 True Source
        assertBool bool1b
        assertEqual 0 (length nsErrors1b)
        (cps,nsOK)<-getCabalDependencies api root2 (Just "./.dist-buildwrapper/cabal-dev")
        assertBool (null nsOK)
        assertEqual 2 (length cps)
        let sbs=filter (\(f,_)->"dist-buildwrapper/cabal-dev" `isInfixOf` f) cps
        assertEqual 1 (length sbs)
        let bwt1=filter (\pkg->cpName pkg == testProject1Name) $ concatMap snd sbs
        assertEqual 1 (length bwt1)
        let deps=cpDependent $ head bwt1
        assertEqual [CCLibrary True] deps
        return ()

cabalDevAPI :: CMDAPI
cabalDevAPI= CMDAPI "cabal-dev" ["--sandbox=./.dist-buildwrapper/cabal-dev"]

testProject1Name :: String
testProject1Name="BWTest1"         

testProject2Name :: String
testProject2Name="BWTest2" 
        
testCabalContents1 :: String
testCabalContents1 = unlines ["name: "++testProject1Name,
        "version:0.1",
        "cabal-version:  >= 1.8",
        "build-type:     Simple",
        "",
        "library",
        "  hs-source-dirs:  src",
        "  exposed-modules: A",
        "  build-depends:  base",
        ""
        ]        
     
testCabalContents2 :: String
testCabalContents2 = unlines ["name: "++testProject2Name,
        "version:0.1",
        "cabal-version:  >= 1.8",
        "build-type:     Simple",
        "",
        "library",
        "  hs-source-dirs:  src",
        "  exposed-modules: B",
        "  build-depends:  base,"++testProject1Name,
        ""
        ]     
     
testCabalFile1 :: FilePath -> FilePath
testCabalFile1 root =root </> (testProject1Name <.> ".cabal") 

testCabalFile2 :: FilePath -> FilePath
testCabalFile2 root =root </> (testProject2Name <.> ".cabal") 
     
testAContents1 :: String     
testAContents1=unlines ["module A where","fA1=reverse"]

testBContents2 :: String     
testBContents2=unlines ["module B where","import A","fB=fA1"]         
        
        
createTestProjects :: IO (FilePath,FilePath)
createTestProjects = do
        temp<-getTemporaryDirectory
       
        let root1=temp </> testProject1Name
        ex1<-doesDirectoryExist root1
        when ex1 (removeDirectoryRecursive root1)
        createDirectory root1
        writeFile (testCabalFile1 root1) testCabalContents1
        writeFile (root1 </> "Setup.hs") testSetupContents
        let srcF1=root1 </> "src"
        createDirectory srcF1
        writeFile (srcF1 </> "A.hs") testAContents1
        
        let root2=temp </> testProject2Name
        ex2<-doesDirectoryExist root2
        when ex2 (removeDirectoryRecursive root2)
        createDirectory root2
        writeFile (testCabalFile2 root2) testCabalContents2
        writeFile (root2 </> "Setup.hs") testSetupContents
        let srcF2=root2 </> "src"
        createDirectory srcF2
        writeFile (srcF2 </> "B.hs") testBContents2
        
        return (root1,root2)


runCabalDev:: FilePath ->  [String] -> IO ()
runCabalDev root args= do
        cd<-getCurrentDirectory
        setCurrentDirectory root
        (ex,out,err)<-readProcessWithExitCode "cabal-dev" ("--sandbox=./.dist-buildwrapper/cabal-dev":args) ""
        setCurrentDirectory cd
        putStrLn ("cabal-dev out:"++out)
        putStrLn ("cabal-dev err:"++err)
        assertEqual ExitSuccess ex         
        
        
{--
$ cabal-dev install <packagePath> --sandbox=<sandbox> 

should do what you're accomplishing with:

$ cabal-dev add-source <packagePath>
$ cabal-dev install --reinstall <thePackage> --force-reinstalls

With the added benefit that you don't need the --force to install direct to a sandbox :)
--}