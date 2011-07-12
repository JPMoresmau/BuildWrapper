
module Language.Haskell.BuildWrapper.APITest where

import Language.Haskell.BuildWrapper.Base
import Language.Haskell.BuildWrapper.API

import qualified Distribution.Verbosity as V 
                        ( silent,normal )
import Test.HUnit

import System.Directory
import System.FilePath

import Control.Monad
import Control.Monad.State

apiTests::Test
apiTests=TestList[testSynchronizeAll,testConfigureErrors,testConfigureWarnings]

testSynchronizeAll :: Test
testSynchronizeAll = TestLabel "testSynchronizeAll" (TestCase ( do
        root<-createTestProject
        fps<-runAPI root synchronize
        assertBool "no file path on creation" (not $ null fps) 
        assertEqual "no cabal file" (testProjectName <.> ".cabal") (head fps)
        assertBool "no A" (elem ("src" </> "A.hs") fps)
        assertBool "no C" (elem ("src" </> "B" </> "C.hs") fps)
        assertBool "no Main" (elem ("src"  </> "Main.hs") fps)
        assertBool "no D" (elem ("src" </> "B" </> "D.hs") fps)
        assertBool "no Main test" (elem ("test" </> "Main.hs") fps)
        assertBool "no TestA" (elem ("test" </> "TestA.hs") fps)
        ))

testConfigureErrors :: Test
testConfigureErrors = TestLabel "testConfigureErrors" (TestCase ( do
        root<-createTestProject
        (boolOK,nsOK)<-runAPI root configure
        assertBool ("returned false") boolOK
        assertBool ("errors or warnings:"++show nsOK) (null nsOK)
        let cf=testCabalFile root
        let cfn=takeFileName cf
        writeFile cf $ unlines ["version:0.1",
                "build-type:     Simple"]
        (bool1,nsErrors1)<-runAPI root configure
        assertBool ("bool1 returned true") (not bool1)
        assertEqual "no errors on no name" 2 (length nsErrors1)
        let (nsError1:nsError2:[])=nsErrors1
        assertEqual "not proper error 1" (BWNote BWError "No 'name' field." "" (BWLocation cfn 1 1)) nsError1
        assertEqual "not proper error 2" (BWNote BWError "No executables and no library found. Nothing to do." "" (BWLocation cfn 1 1)) nsError2
        writeFile cf $ unlines ["name: 4 P1",
                "version:0.1",
                "build-type:     Simple"]
        (bool2,nsErrors2)<-runAPI root configure
        assertBool ("bool2 returned true") (not bool2)
        assertEqual "no errors on invalid name" 1 (length nsErrors2)
        let (nsError3:[])=nsErrors2
        assertEqual "not proper error 3" (BWNote BWError "Parse of field 'name' failed." "" (BWLocation cfn 1 1)) nsError3
        writeFile cf $ unlines ["name: "++testProjectName,
                "version:0.1",
                "cabal-version:  >= 1.8",
                "build-type:     Simple",
                "",
                "library",
                "  hs-source-dirs:  src",
                "  exposed-modules: A",
                "  other-modules:  B.C",
                "  build-depends:   base, toto"]
        (bool3,nsErrors3)<-runAPI root configure
        assertBool ("bool3 returned true") (not bool3)
        assertEqual "no errors on unknown dependency" 1 (length nsErrors3)
        let (nsError4:[])=nsErrors3
        assertEqual "not proper error 4" (BWNote BWError "At least the following dependencies are missing:\ntoto -any\n" "" (BWLocation cfn 1 1)) nsError4
        writeFile cf $ unlines ["name: "++testProjectName,
                "version:0.1",
                "cabal-version:  >= 1.8",
                "build-type:     Simple",
                "",
                "library",
                "  hs-source-dirs:  src",
                "  exposed-modules: A",
                "  other-modules:  B.C",
                "  build-depends:   base, toto, titi"]
        (bool4,nsErrors4)<-runAPI root configure
        assertBool ("bool4 returned true") (not bool4)
        assertEqual "no errors on unknown dependencies" 1 (length nsErrors4)
        let (nsError5:[])=nsErrors4
        assertEqual "not proper error 5" (BWNote BWError "At least the following dependencies are missing:\ntiti -any, toto -any\n" "" (BWLocation cfn 1 1)) nsError5
        ))
        
testConfigureWarnings :: Test
testConfigureWarnings = TestLabel "testConfigureWarnings" (TestCase ( do
        root<-createTestProject
        let cf=testCabalFile root
        let cfn=takeFileName cf
        writeFile cf $ unlines ["name: "++testProjectName,
                "version:0.1",
                "cabal-version:  >= 1.8",
                "build-type:     Simple",
                "field1:         toto",
                "",
                "library",
                "  hs-source-dirs:  src",
                "  exposed-modules: A",
                "  other-modules:  B.C",
                "  build-depends:   base"]
        (bool1,ns1)<-runAPI root configure
        assertBool ("returned false") bool1
        putStrLn $ show ns1
        let (nsWarning1:[])=ns1
        assertEqual "not proper warning 1" (BWNote BWWarning "Unknown fields: field1 (line 5)" "" (BWLocation cfn 5 1)) nsWarning1
        ))   
        
runAPI::
        Monad m =>
        FilePath -> StateT BuildWrapperState m a -> m a
runAPI root f= do
        evalStateT f (BuildWrapperState ".dist-buildwrapper" "cabal" (testCabalFile root) V.normal)
        
testProjectName :: String
testProjectName="BWTest"         
        
testCabalContents :: String
testCabalContents = unlines ["name: "++testProjectName,
        "version:0.1",
        "cabal-version:  >= 1.8",
        "build-type:     Simple",
        "",
        "library",
        "  hs-source-dirs:  src",
        "  exposed-modules: A",
        "  other-modules:  B.C",
        "  build-depends:  base",
        "",
        "executable BWTest",
        "  hs-source-dirs:  src",
        "  main-is:         Main.hs",
        "  other-modules:  B.D",
        "  build-depends:  base",
        "",
        "test-suite BWTest-test",
        "  type:            exitcode-stdio-1.0",
        "  hs-source-dirs:  test",
        "  main-is:         Main.hs",
        "  other-modules:  TestA",
        "  build-depends:  base",
        ""
        ]        
     
testCabalFile :: FilePath -> FilePath
testCabalFile root =root </> (testProjectName <.> ".cabal") 
     
testAContents :: String     
testAContents=unlines ["module A where","fA=undefined"]
testCContents :: String
testCContents=unlines ["module B.C where","fC=undefined"]     
testDContents :: String
testDContents=unlines ["module B.D where","fD=undefined"]     
testMainContents :: String
testMainContents=unlines ["module Main where","main=undefined"]   
testMainTestContents :: String
testMainTestContents=unlines ["module Main where","main=undefined"]   
testTestAContents :: String
testTestAContents=unlines ["module TestA where","fTA=undefined"]           
        
createTestProject :: IO(FilePath)
createTestProject = do
        temp<-getTemporaryDirectory
        let root=temp </> testProjectName
        ex<-(doesDirectoryExist root)
        when ex (removeDirectoryRecursive root)
        createDirectory root
        writeFile (testCabalFile root) testCabalContents
        let srcF=root </> "src"
        createDirectory srcF
        writeFile (srcF </> "A.hs") testAContents
        let b=srcF </> "B"
        createDirectory b
        writeFile (b </> "C.hs") testCContents
        writeFile (b </> "D.hs") testDContents
        writeFile (srcF </> "Main.hs") testMainContents
        let testF=root </> "test"
        createDirectory testF
        writeFile (testF </> "Main.hs") testMainTestContents
        writeFile (testF </> "TestA.hs") testTestAContents
        return root
        