{-# OPTIONS_GHC -F -pgmF htfpp #-}
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

test_CabalDev2Projects :: Assertion
test_CabalDev2Projects= do
        let api=cabalDevAPI
        (root1,root2)<-createTestProjects
        runCabalDev root1 ["install"]
        runCabalDev root2 ["add-source",root1]
        runCabalDev root2 ["install"]
        ((fps,dels),_)<-synchronize api root2 False
        assertBool (not $ null fps) 
        assertBool (null dels) 
        configure api root2 Source
        build api root2 True Source     
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
       -- runCabalDev root2 ["ghc-pkg","unregister","--force",testProject1Name]
        runCabalDev root2 ["add-source",root1]
        runCabalDev root2 ["install" ,"--reinstall",testProject1Name,"--force-reinstalls"]
        removeDirectoryRecursive (root2 </> ".dist-buildwrapper")
--        writeFile (root2 </> relB) $ unlines [
--                "module B where","import A","fB=fA2"
--                ]
        synchronize api root2 False
        configure api root2 Source    
        build api root2 True Source       
        (mtts2,nsErrors2)<-getNamesInScope api root2 relB
        assertBool (null nsErrors2)
        assertBool (isJust mtts2)
        let tts2=fromJust mtts2
        assertBool ("B.fB" `elem` tts2)
        assertBool ("A.fA1" `elem` tts2)
        assertBool ("A.fA2" `elem` tts2)
        return ()

cabalDevAPI :: CMDAPI
cabalDevAPI= CMDAPI "cabal-dev"

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
        (ex,out,err)<-readProcessWithExitCode "cabal-dev" args ""
        setCurrentDirectory cd
        putStrLn ("cabal-dev out:"++out)
        putStrLn ("cabal-dev err:"++err)
        assertEqual ExitSuccess ex         
        