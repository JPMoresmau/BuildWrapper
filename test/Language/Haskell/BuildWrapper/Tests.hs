{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.BuildWrapper.Tests where

import Language.Haskell.BuildWrapper.Base

import Data.Maybe
import Data.Aeson

import Test.HUnit

import System.Directory
import System.FilePath
import System.Info

import Control.Monad
import Control.Monad.State

import System.Time

tests :: (APIFacade a)=> [(a -> Test)]
tests=  [
        testSynchronizeAll,
        testConfigureWarnings ,testConfigureErrors ,
        testBuildErrors,testBuildWarnings,
        testBuildOutput,
        testModuleNotInCabal,
        testOutline,
        testOutlinePreproc ,
        testPreviewTokenTypes,
        testThingAtPoint,
        testNamesInScope,
        testInPlaceReference,
        testCabalComponents,
        testCabalDependencies
        ]

class APIFacade a where
        synchronize :: a -> FilePath -> IO (OpResult [FilePath])
        synchronize1 :: a -> FilePath -> FilePath -> IO (Maybe FilePath)
        write :: a -> FilePath -> FilePath -> String -> IO ()
        configure :: a -> FilePath -> WhichCabal -> IO (OpResult Bool)
        build :: a -> FilePath -> Bool -> IO (OpResult Bool)
        getOutline :: a -> FilePath -> FilePath -> IO (OpResult [OutlineDef])
        getTokenTypes :: a -> FilePath -> FilePath -> IO (OpResult [TokenDef])
        getOccurrences :: a -> FilePath -> FilePath -> String -> IO (OpResult [TokenDef])
        getThingAtPoint :: a -> FilePath -> FilePath -> Int -> Int -> Bool -> Bool -> IO (OpResult (Maybe String))
        getNamesInScope :: a -> FilePath -> FilePath-> IO (OpResult (Maybe [String]))
        getCabalDependencies :: a -> FilePath -> IO (OpResult [(FilePath,[CabalPackage])])
        getCabalComponents :: a -> FilePath -> IO (OpResult [CabalComponent])
        
testSynchronizeAll :: (APIFacade a)=> a -> Test
testSynchronizeAll api= TestLabel "testSynchronizeAll" (TestCase ( do
        root<-createTestProject
        (fps,ns)<-synchronize api root
        assertBool "no file path on creation" (not $ null fps) 
        assertEqual "no cabal file" (testProjectName <.> ".cabal") (head fps)
        assertBool "no A" (elem ("src" </> "A.hs") fps)
        assertBool "no C" (elem ("src" </> "B" </> "C.hs") fps)
        assertBool "no Main" (elem ("src"  </> "Main.hs") fps)
        assertBool "no D" (elem ("src" </> "B" </> "D.hs") fps)
        assertBool "no Main test" (elem ("test" </> "Main.hs") fps)
        assertBool "no TestA" (elem ("test" </> "TestA.hs") fps)
        ))

testConfigureErrors :: (APIFacade a)=> a -> Test
testConfigureErrors api= TestLabel "testConfigureErrors" (TestCase ( do
        root<-createTestProject
        (boolNoCabal,nsNoCabal)<- configure api root Target
        assertBool ("configure returned true on no cabal") (not boolNoCabal)
        assertEqual ("no errors or warnings on no cabal") 1 (length nsNoCabal)        
        assertEqual ("wrong error on no cabal") (BWNote BWError "No cabal file found.\nPlease create a package description file <pkgname>.cabal\n" (BWLocation "" 0 1)) (head nsNoCabal)   
        
        synchronize api root
        (boolOK,nsOK)<-configure api root Target
        assertBool ("configure returned false") boolOK
        assertBool ("errors or warnings:"++show nsOK) (null nsOK)
        let cf=testCabalFile root
        let cfn=takeFileName cf
        writeFile cf $ unlines ["version:0.1",
                "build-type:     Simple"]
        synchronize api root
        (bool1,nsErrors1)<-configure api root Target
        assertBool ("bool1 returned true") (not bool1)
        assertEqual "no errors on no name" 2 (length nsErrors1)
        let (nsError1:nsError2:[])=nsErrors1
        assertEqual "not proper error 1" (BWNote BWError "No 'name' field.\n" (BWLocation cfn 1 1)) nsError1
        assertEqual "not proper error 2" (BWNote BWError "No executables and no library found. Nothing to do.\n" (BWLocation cfn 1 1)) nsError2
        writeFile cf $ unlines ["name: 4 P1",
                "version:0.1",
                "build-type:     Simple"]
        synchronize api root
        (bool2,nsErrors2)<-configure api root Target
        assertBool ("bool2 returned true") (not bool2)
        assertEqual "no errors on invalid name" 1 (length nsErrors2)
        let (nsError3:[])=nsErrors2
        assertEqual "not proper error 3" (BWNote BWError "Parse of field 'name' failed.\n" (BWLocation cfn 1 1)) nsError3
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
        synchronize api root
        (bool3,nsErrors3)<-configure api root Target
        assertBool ("bool3 returned true") (not bool3)
        assertEqual "no errors on unknown dependency" 1 (length nsErrors3)
        let (nsError4:[])=nsErrors3
        assertEqual "not proper error 4" (BWNote BWError "At least the following dependencies are missing:\ntoto -any\n" (BWLocation cfn 1 1)) nsError4
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
        synchronize api root
        (bool4,nsErrors4)<-configure api root Target
        assertBool ("bool4 returned true") (not bool4)
        assertEqual "no errors on unknown dependencies" 1 (length nsErrors4)
        let (nsError5:[])=nsErrors4
        assertEqual "not proper error 5" (BWNote BWError "At least the following dependencies are missing:\ntiti -any, toto -any\n" (BWLocation cfn 1 1)) nsError5
        writeFile cf $ unlines ["name: "++testProjectName,
                "version:0.1",
                "cabal-version:  >= 1.8",
                "build-type:     Simple",
                "",
                "executable BWTest",
                "  hs-source-dirs:  src",
                "  other-modules:  B.D",
                "  build-depends:  base"]
        synchronize api root
        (bool5,nsErrors5)<-configure api root Target
        assertBool ("bool5 returned true") (not bool5)
        assertEqual "no errors on no main" 1 (length nsErrors5)
        let (nsError6:[])=nsErrors5
        assertEqual "not proper error 6" (BWNote BWError "No 'Main-Is' field found for executable BWTest\n" (BWLocation cfn 1 1)) nsError6
        
        ))
        
testConfigureWarnings :: (APIFacade a)=> a -> Test
testConfigureWarnings api = TestLabel "testConfigureWarnings" (TestCase ( do
        root<-createTestProject
        synchronize api root
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
        synchronize api root
        (bool1,ns1)<- configure api root Target
        assertBool ("returned false 1 "++ (show ns1)) bool1
        assertEqual ("didn't return 1 warning") 1 (length ns1)
        let (nsWarning1:[])=ns1
        assertEqual "not proper warning 1" (BWNote BWWarning "Unknown fields: field1 (line 5)\nFields allowed in this section:\nname, version, cabal-version, build-type, license, license-file,\ncopyright, maintainer, build-depends, stability, homepage,\npackage-url, bug-reports, synopsis, description, category, author,\ntested-with, data-files, data-dir, extra-source-files,\nextra-tmp-files\n" (BWLocation cfn 5 1)) nsWarning1
        writeFile cf $ unlines ["name: "++testProjectName,
                "version:0.1",
                "build-type:     Simple",
                "",
                "library",
                "  hs-source-dirs:  src",
                "  exposed-modules: A",
                "  other-modules:  B.C",
                "  build-depends:   base"]
        synchronize api root
        (bool2,ns2)<- configure api root Target
        assertBool ("returned false 2 "++ (show ns2)) bool2
        assertEqual ("didn't return 1 warning") 1 (length ns2)
        let (nsWarning2:[])=ns2
        assertEqual "not proper warning 2" (BWNote BWWarning "A package using section syntax must specify at least\n'cabal-version: >= 1.2'.\n" (BWLocation cfn 0 1)) nsWarning2
        writeFile cf $ unlines ["name: "++testProjectName,
                "version:0.1",
                "cabal-version:  >= 1.2",
                "",
                "library",
                "  hs-source-dirs:  src",
                "  exposed-modules: A",
                "  other-modules:  B.C",
                "  build-depends:   base"]
        writeFile ((takeDirectory cf) </> "Setup.hs") $ unlines ["import Distribution.Simple",
			"main = defaultMain"]
        synchronize api root
        (bool3,ns3)<- configure api root Target
        assertBool ("returned false 3 "++ (show ns3)) bool3
        assertEqual ("didn't return 1 warning") 1 (length ns3)
        let (nsWarning3:[])=ns3
        assertEqual "not proper warning 3" (BWNote BWWarning "No 'build-type' specified. If you do not need a custom Setup.hs or\n./configure script then use 'build-type: Simple'.\n" (BWLocation cfn 0 1)) nsWarning3

        ))   
        
testBuildErrors :: (APIFacade a)=> a -> Test
testBuildErrors api = TestLabel "testBuildErrors" (TestCase ( do
        root<-createTestProject
        synchronize api root
        (boolOKc,nsOKc)<-configure api root Target
        assertBool ("returned false on configure") boolOKc
        assertBool ("errors or warnings on configure:"++show nsOKc) (null nsOKc)
        (boolOK,nsOK)<-build api root False
        assertBool ("returned false on build") boolOK
        assertBool ("errors or warnings on build:"++show nsOK) (null nsOK)
        --let srcF=root </> "src"
        let rel="src"</>"A.hs"
        -- use api to write temp file
        write api root rel $ unlines ["module A where","import toto","fA=undefined"]
        --mf1<-runAPI root $ synchronize1 rel
        --assertBool ("mf1 not just") (isJust mf1)
        (bool1,nsErrors1)<-build api root False
        assertBool ("returned true on bool1") (not bool1)
        assertBool ("no errors or warnings on nsErrors") (not $ null nsErrors1)
        let (nsError1:[])=nsErrors1
        assertEqual "not proper error 1" (BWNote BWError "parse error on input `toto'\n" (BWLocation rel 2 8)) nsError1
        -- write file and synchronize
        writeFile (root </> "src"</>"A.hs")$ unlines ["module A where","import Toto","fA=undefined"]
        --runAPI root $ write rel $ unlines ["module A where","import Toto","fA=undefined"]
        mf2<-synchronize1 api root rel
        assertBool ("mf2 not just") (isJust mf2)
        (bool2,nsErrors2)<-build api root False
        assertBool ("returned true on bool2") (not bool2)
        assertBool ("no errors or warnings on nsErrors2") (not $ null nsErrors2)
        let (nsError2:[])=nsErrors2
        assertEqual "not proper error 2" (BWNote BWError "Could not find module `Toto':\n      Use -v to see a list of the files searched for.\n" (BWLocation rel 2 8)) nsError2
        ))        
        
testBuildWarnings :: (APIFacade a)=> a -> Test
testBuildWarnings api = TestLabel "testBuildWarnings" (TestCase ( do
        root<-createTestProject
        synchronize api root
        --let cf=testCabalFile root      
        write  api root(testProjectName <.> ".cabal") $ unlines ["name: "++testProjectName,
                "version:0.1",
                "cabal-version:  >= 1.8",
                "build-type:     Simple",
                "",
                "library",
                "  hs-source-dirs:  src",
                "  exposed-modules: A",
                "  build-depends:   base",
                "  ghc-options:     -Wall"]
        --let srcF=root </> "src"
        (boolOK,nsOK)<-configure api root Target
        assertBool ("returned false") boolOK
        assertBool ("errors or warnings:"++show nsOK) (null nsOK)
        let rel="src"</>"A.hs"
        write api root rel $ unlines ["module A where","import Data.List","fA=undefined"] 

        (bool1,nsErrors1)<-build api root False
        assertBool ("returned false on bool1") bool1
        assertBool ("no errors or warnings on nsErrors1") (not $ null nsErrors1)
        let (nsError1:nsError2:[])=nsErrors1
        assertEqual "not proper error 1" (BWNote BWWarning "The import of `Data.List' is redundant\n               except perhaps to import instances from `Data.List'\n             To import instances alone, use: import Data.List()\n" (BWLocation rel 2 1)) nsError1
        assertEqual "not proper error 2" (BWNote BWWarning "Top-level binding with no type signature:\n               fA :: forall a. a\n" (BWLocation rel 3 1)) nsError2
        
        )) 
        
testBuildOutput :: (APIFacade a)=> a -> Test
testBuildOutput api = TestLabel "testBuildOutput" (TestCase ( do       
        root<-createTestProject
        synchronize api root
        build api root True
        let exeN=case os of
        	"mingw32"->(addExtension testProjectName "exe")
        	_->testProjectName
        let exeF=root </> ".dist-buildwrapper" </> "dist" </> "build" </> testProjectName </> exeN
        exeE1<-doesFileExist exeF
        assertBool ("exe does not exist on build output: "++exeF) exeE1
        removeFile exeF
        exeE2<-doesFileExist exeF
        assertBool ("exe does still exist after deletion: "++exeF) (not exeE2)
        build api root False
        exeE3<-doesFileExist exeF
        assertBool ("exe exists after build no output: "++exeF) (not exeE3)
        ))
        
testModuleNotInCabal :: (APIFacade a)=> a -> Test
testModuleNotInCabal api = TestLabel "testModuleNotInCabal" (TestCase ( do
        root<-createTestProject
        synchronize api root
        let rel="src"</>"A.hs"
        write api root rel $ unlines ["module A where","import Auto","fA=undefined"] 
        let rel2="src"</>"Auto.hs"
        putStrLn (root </> rel2) 
        writeFile (root </> rel2) $ unlines ["module Auto where","fAuto=undefined"] 
        (fps,ns)<-synchronize api root
        putStrLn $ show fps
        putStrLn $ show ns
        (bool1,nsErrors1)<-build api root False
        putStrLn $ show nsErrors1
        assertBool ("returned false on bool1") bool1
        assertBool ("errors or warnings on nsErrors1") (null nsErrors1)
        ))      
        
--testAST :: Test
--testAST = TestLabel "testAST" (TestCase ( do
--        root<-createTestProject
--        runAPI root synchronize
--        (boolOKc,nsOKc)<-runAPI root $ configure Target
--        assertBool ("returned false on configure") boolOKc
--        assertBool ("errors or warnings on configure:"++show nsOKc) (null nsOKc)
--        
--        (boolOK,nsOK)<-runAPI root $ build False
--        assertBool ("returned false on build") boolOK
--        assertBool ("errors or warnings on build:"++show nsOK) (null nsOK)
--        (mast,nsOK2)<-runAPI root $ getAST ("src" </> "A.hs")
--        assertBool ("errors or warnings on getAST:"++show nsOK2) (null nsOK2)
--        case mast of
--                Just ast->do
--                        let json=makeObj  [("parse" , (showJSON $ ast))]
--                        putStrLn $ show $ encode json
--                Nothing -> assertFailure "no ast"
--        ))
        
testOutline :: (APIFacade a)=> a -> Test
testOutline api= TestLabel "testOutline" (TestCase ( do
        root<-createTestProject
        synchronize api root
        let rel="src"</>"A.hs"
        -- use api to write temp file
        write api root rel $ unlines [
                "{-# LANGUAGE RankNTypes, TypeSynonymInstances, TypeFamilies #-}",
                "",
                "module Module1 where",
                "",
                "import Data.Char",
                "",
                "-- Declare a list-like data family",
                "data family XList a",
                "",
                "-- Declare a list-like instance for Char",
                "data instance XList Char = XCons !Char !(XList Char) | XNil",
                "",
                "type family Elem c",
                "",
                "type instance Elem [e] = e",
                "",
                "testfunc1 :: [Char]",
                "testfunc1=reverse \"test\"",
                "",
                "testfunc1bis :: String -> [Char]",
                "testfunc1bis []=\"nothing\"",
                "testfunc1bis s=reverse s",
                "",
                "testMethod :: forall a. (Num a) => a -> a -> a",
                "testMethod a b=",
                "    let e=a + (fromIntegral $ length testfunc1)",
                "    in e * 2",
                "",
                "class ToString a where",
                "    toString :: a -> String",
                "",
                "instance ToString String where",
                "    toString = id",
                "",    
                "type Str=String",
                "",
                "data Type1=MkType1_1 Int",
                "    | MkType1_2 {",
                "        mkt2_s :: String,",
                "        mkt2_i :: Int",
                "        }" ]
        (defs,nsErrors1)<-getOutline api root rel
        assertBool ("errors or warnings on getOutline:"++show nsErrors1) (null nsErrors1)
        let expected=[
                OutlineDef "XList" [Data,Family] (InFileSpan (InFileLoc 8 1)(InFileLoc 8 20))  []
                ,OutlineDef "XList Char" [Data,Instance] (InFileSpan (InFileLoc 11 1)(InFileLoc 11 60)) [
                        OutlineDef "XCons" [Constructor] (InFileSpan (InFileLoc 11 28)(InFileLoc 11 53))  []
                        ,OutlineDef "XNil" [Constructor] (InFileSpan (InFileLoc 11 56)(InFileLoc 11 60))  []
                        ]
                ,OutlineDef "Elem" [Type,Family] (InFileSpan (InFileLoc 13 1)(InFileLoc 13 19))  []
                ,OutlineDef "Elem [e]" [Type,Instance] (InFileSpan (InFileLoc 15 1)(InFileLoc 15 27))  []
                ,OutlineDef "testfunc1" [Function] (InFileSpan (InFileLoc 18 1)(InFileLoc 18 25))  []
                ,OutlineDef "testfunc1bis" [Function] (InFileSpan (InFileLoc 21 1)(InFileLoc 22 25))  []                  
                ,OutlineDef "testMethod" [Function] (InFileSpan (InFileLoc 25 1)(InFileLoc 27 13))  []  
                ,OutlineDef "ToString" [Class] (InFileSpan (InFileLoc 29 1)(InFileLoc 32 0))  [
                        OutlineDef "toString" [Function] (InFileSpan (InFileLoc 30 5)(InFileLoc 30 28))  []
                        ]          
                ,OutlineDef "ToString String" [Instance] (InFileSpan (InFileLoc 32 1)(InFileLoc 35 0))  [
                        OutlineDef "toString" [Function] (InFileSpan (InFileLoc 33 5)(InFileLoc 33 18))  []
                        ]    
                ,OutlineDef "Str" [Type] (InFileSpan (InFileLoc 35 1)(InFileLoc 35 16))  []                
                ,OutlineDef "Type1" [Data] (InFileSpan (InFileLoc 37 1)(InFileLoc 41 10))  [
                         OutlineDef "MkType1_1" [Constructor] (InFileSpan (InFileLoc 37 12)(InFileLoc 37 25)) []
                        ,OutlineDef "MkType1_2" [Constructor] (InFileSpan (InFileLoc 38 7)(InFileLoc 41 10)) [
                                OutlineDef "mkt2_s" [Field] (InFileSpan (InFileLoc 39 9)(InFileLoc 39 25)) []
                                ,OutlineDef "mkt2_i" [Field] (InFileSpan (InFileLoc 40 9)(InFileLoc 40 22)) []
                                
                                ]
                        
                        ]
                ]
        assertEqual "length" (length expected) (length defs)
        mapM_ (\(e,c)->assertEqual "outline" e c) (zip expected defs)
      ))   
        
testOutlinePreproc :: (APIFacade a)=> a -> Test
testOutlinePreproc api= TestLabel "testOutlinePreproc" (TestCase ( do
        root<-createTestProject
        synchronize api root
        let rel="src"</>"A.hs"
        write api root (testProjectName <.> ".cabal") $ unlines ["name: "++testProjectName,
                "version:0.1",
                "cabal-version:  >= 1.8",
                "build-type:     Simple",
                "",
                "library",
                "  hs-source-dirs:  src",
                "  exposed-modules: A",
                "  build-depends:   base",
                "  extensions:      CPP",
                "  ghc-options:     -Wall",
                "  cpp-options:     -DCABAL_VERSION=112"]
        configure api root Target        
        -- use api to write temp file
        write api root rel $ unlines [
                "{-# LANGUAGE CPP #-}",
                "",
                "module Module1 where",
                "",
                "#if CABAL_VERSION > 110",
                "testfunc1=reverse \"test\"",
                "#endif",
                ""
                 ]
        (defs1,nsErrors1)<-getOutline api root rel
        assertBool ("errors or warnings on getOutlinePreproc 1:"++show nsErrors1) (null nsErrors1)
        let expected1=[
                OutlineDef "testfunc1" [Function] (InFileSpan (InFileLoc 6 1)(InFileLoc 6 25))  []
                ]
        assertEqual "length" (length expected1) (length defs1)
        mapM_ (\(e,c)->assertEqual "outline" e c) (zip expected1 defs1)
        write api root rel $ unlines [
                "{-# LANGUAGE CPP #-}",
                "",
                "module Module1 where",
                "",
                "data Name",
                "   = Ident String    -- ^ /varid/ or /conid/.",
                "   | Symbol String   -- ^ /varsym/ or /consym/",
                "#ifdef __GLASGOW_HASKELL__",
                " deriving (Eq,Ord,Show,Typeable,Data)",
                "#else",
                " deriving (Eq,Ord,Show)",
                "#endif",
                ""
                 ]
        (defs2,nsErrors2)<-getOutline api root rel
        assertBool ("errors or warnings on getOutlinePreproc:"++show nsErrors2) (null nsErrors2)
        let expected2=[
                OutlineDef "Name" [Data] (InFileSpan (InFileLoc 5 1)(InFileLoc 9 38))  [
                  OutlineDef "Ident" [Constructor] (InFileSpan (InFileLoc 6 6)(InFileLoc 6 18))  [],
                  OutlineDef "Symbol" [Constructor] (InFileSpan (InFileLoc 7 6)(InFileLoc 7 19))  []
                  ]
                ] 
        assertEqual "length" (length expected2) (length defs2)
        mapM_ (\(e,c)->assertEqual "outline" e c) (zip expected2 defs2)
        write api root rel $ unlines [
                "{-# LANGUAGE CPP #-}",
                "",
                "module Module1 where",
                "",
                "#ifdef __GLASGOW_HASKELL__",
                "testfunc1=reverse \"test\"",
                "#else",
                "testfunc2=reverse \"test\"",
                "#endif",
                ""
                 ]
        (defs3,nsErrors3)<-getOutline api root rel
        assertBool ("errors or warnings on getOutlinePreproc 3:"++show nsErrors3) (null nsErrors3)
        let expected3=[
                OutlineDef "testfunc1" [Function] (InFileSpan (InFileLoc 6 1)(InFileLoc 6 25))  []
                ]
        assertEqual "length" (length expected3) (length defs3)
        mapM_ (\(e,c)->assertEqual "outline" e c) (zip expected3 defs3)
      ))      
        
testPreviewTokenTypes :: (APIFacade a)=> a -> Test
testPreviewTokenTypes api= TestLabel "testPreviewTokenTypes" (TestCase ( do
        root<-createTestProject
        synchronize api root
        configure api root Target        
        let rel="src"</>"Main.hs"
        write api root rel $ unlines [
                "{-# LANGUAGE TemplateHaskell,CPP #-}",
                "-- a comment",
                "module Main where", 
                "",
                "main :: IO (Int)",
                "main = do" ,
                "        putStr ('h':\"ello Prefs!\")",
                "        return (2 + 2)",
                "",
                "#if USE_TH",
                "$( derive makeTypeable ''Extension )",
                "#endif",
                ""
                ]
        (tts,nsErrors1)<-getTokenTypes api root rel
        assertBool ("errors or warnings on getTokenTypes:"++show nsErrors1) (null nsErrors1)
        let expectedS="[{\"D\":[1,0,1,36]},{\"D\":[2,0,2,12]},{\"K\":[3,0,3,6]},{\"IC\":[3,7,3,11]},{\"K\":[3,12,3,17]},{\"IV\":[5,0,5,4]},{\"S\":[5,5,5,7]},{\"IC\":[5,8,5,10]},{\"SS\":[5,11,5,12]},{\"IC\":[5,12,5,15]},{\"SS\":[5,15,5,16]},{\"IV\":[6,0,6,4]},{\"S\":[6,5,6,6]},{\"K\":[6,7,6,9]},{\"IV\":[7,8,7,14]},{\"SS\":[7,15,7,16]},{\"LC\":[7,16,7,19]},{\"S\":[7,19,7,20]},{\"LS\":[7,20,7,33]},{\"SS\":[7,33,7,34]},{\"IV\":[8,8,8,14]},{\"SS\":[8,15,8,16]},{\"LI\":[8,16,8,17]},{\"IV\":[8,18,8,19]},{\"LI\":[8,20,8,21]},{\"SS\":[8,21,8,22]},{\"PP\":[10,0,10,10]},{\"TH\":[11,0,11,2]},{\"IV\":[11,3,11,9]},{\"IV\":[11,10,11,22]},{\"TH\":[11,23,11,25]},{\"IC\":[11,25,11,34]},{\"SS\":[11,35,11,36]},{\"PP\":[12,0,12,6]}]"
        assertEqual "" expectedS (encode $ toJSON tts)
        ))
        
testThingAtPoint :: (APIFacade a)=> a -> Test
testThingAtPoint api= TestLabel "testThingAtPoint" (TestCase ( do
        root<-createTestProject
        synchronize api root
        configure api root Target        
        let rel="src"</>"Main.hs"
        write api root rel $ unlines [  
                  "module Main where",
                  "main=return $ map id \"toto\""
                  ] 
        (tap1,nsErrors1)<-getThingAtPoint api root rel 2 16 True True
        assertBool ("errors or warnings on getThingAtPoint1:"++show nsErrors1) (null nsErrors1)
        assertEqual "not just typed qualified" (Just "GHC.Base.map :: forall a b.\n                (a -> b) -> [a] -> [b] GHC.Types.Char GHC.Types.Char") tap1
        (tap2,nsErrors2)<-getThingAtPoint api root rel 2 16 False True
        assertBool ("errors or warnings on getThingAtPoint2:"++show nsErrors2) (null nsErrors2)
        assertEqual "not just typed unqualified" (Just "map :: forall a b. (a -> b) -> [a] -> [b] Char Char") tap2
        (tap3,nsErrors3)<-getThingAtPoint api root rel 2 16 True False
        assertBool ("errors or warnings on getThingAtPoint3:"++show nsErrors3) (null nsErrors3)
        assertEqual "not just untyped qualified" (Just "GHC.Base.map v") tap3
       
        (tap4,nsErrors4)<-getThingAtPoint api root rel 2 16 False False
        assertBool ("errors or warnings on getThingAtPoint4:"++show nsErrors4) (null nsErrors4)
        assertEqual "not just untyped unqualified" (Just "map v") tap4
        
        )) 

testNamesInScope :: (APIFacade a)=> a -> Test
testNamesInScope api= TestLabel "testNamesInScope" (TestCase ( do
        root<-createTestProject
        synchronize api root
        configure api root Target        
        let rel="src"</>"Main.hs"
        write api root rel $ unlines [  
                  "module Main where",
                  "import B.D",
                  "main=return $ map id \"toto\""
                  ] 
        build api root True
        --c1<-getClockTime
        (mtts,nsErrors1)<-getNamesInScope api root rel
        --c2<-getClockTime
        --putStrLn ("testNamesInScope: " ++ (timeDiffToString  $ diffClockTimes c2 c1))
        assertBool ("errors or warnings on getNamesInScope:"++show nsErrors1) (null nsErrors1)
        assertBool "getNamesInScope not just" (isJust mtts)
        let tts=fromJust mtts
        assertBool "does not contain Main.main" (elem "Main.main" tts)
        assertBool "does not contain B.D.fD" (elem "B.D.fD" tts)
        assertBool "does not contain GHC.Types.Char" (elem "GHC.Types.Char" tts)
        )) 
        
testInPlaceReference  :: (APIFacade a)=> a -> Test
testInPlaceReference api= TestLabel "testInPlaceReference" (TestCase ( do
        root<-createTestProject
        synchronize api root
        write api root (testProjectName <.> ".cabal") $ unlines ["name: "++testProjectName,
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
                "  build-depends:  base, BWTest",
                "",
                "test-suite BWTest-test",
                "  type:            exitcode-stdio-1.0",
                "  hs-source-dirs:  test",
                "  main-is:         Main.hs",
                "  other-modules:  TestA",
                "  build-depends:  base, BWTest",
                ""
                ]   
        (boolOKc,nsOKc)<-configure api root Target
        assertBool ("returned false on configure") boolOKc
        assertBool ("errors or warnings on configure:"++show nsOKc) (null nsOKc)
        (boolOK,nsOK)<-build api root False
        assertBool ("returned false on build") boolOK
        assertBool ("errors or warnings on build:"++show nsOK) (null nsOK)
        ))

testCabalComponents  :: (APIFacade a)=> a -> Test
testCabalComponents api= TestLabel "testCabalComponents" (TestCase ( do
        root<-createTestProject
        synchronize api root
        (cps,nsOK)<-getCabalComponents api root
        assertBool ("errors or warnings on getCabalComponents:"++show nsOK) (null nsOK)
        assertEqual "not three components" 3 (length cps)
        let (l:ex:ts:[])=cps
        assertEqual "not library true" (CCLibrary True) l
        assertEqual "not executable true" (CCExecutable "BWTest" True) ex
        assertEqual "not test suite true" (CCTestSuite "BWTest-test" True) ts
        let cf=testCabalFile root
        writeFile cf $ unlines ["name: "++testProjectName,
	        "version:0.1",
	        "cabal-version:  >= 1.8",
	        "build-type:     Simple",
	        "",
	        "library",
	        "  hs-source-dirs:  src",
	        "  exposed-modules: A",
	        "  other-modules:  B.C",
	        "  build-depends:  base",
	        "  buildable: False",
	        "",
	        "executable BWTest",
	        "  hs-source-dirs:  src",
	        "  main-is:         Main.hs",
	        "  other-modules:  B.D",
	        "  build-depends:  base",
	        "  buildable: False",
	        "",
	        "test-suite BWTest-test",
	        "  type:            exitcode-stdio-1.0",
	        "  hs-source-dirs:  test",
	        "  main-is:         Main.hs",
	        "  other-modules:  TestA",
	        "  build-depends:  base",
	        "  buildable: False",
	        ""
	        ]
        synchronize api root
        (cps2,nsOK2)<-getCabalComponents api root
        assertBool ("errors or warnings on getCabalComponents:"++show nsOK2) (null nsOK2)
        assertEqual "not three components" 3 (length cps2)
        let (l2:ex2:ts2:[])=cps2
        assertEqual "not library false" (CCLibrary False) l2
        assertEqual "not executable false" (CCExecutable "BWTest" False) ex2
        assertEqual "not test suite false" (CCTestSuite "BWTest-test" False) ts2          
        ))

testCabalDependencies  :: (APIFacade a)=> a -> Test
testCabalDependencies api= TestLabel "testCabalDependencies" (TestCase ( do
        root<-createTestProject
        synchronize api root
        (cps,nsOK)<-getCabalDependencies api root
        assertBool ("errors or warnings on getCabalDependencies:"++show nsOK) (null nsOK)
        assertEqual "not two databases" 2 (length cps)
        let (f1:(fp,pkgs):[])=cps
        let base=filter (\pkg->(cp_name pkg)=="base") pkgs
        assertEqual "not 1 base" 1 (length base)
        let (l:ex:ts:[])=cp_dependent $ head base
        assertEqual "not library true" (CCLibrary True) l
        assertEqual "not executable true" (CCExecutable "BWTest" True) ex
        assertEqual "not test suite true" (CCTestSuite "BWTest-test" True) ts
		))

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
        