{-# LANGUAGE CPP, OverloadedStrings #-}
-- |
-- Module      : Language.Haskell.BuildWrapper.Tests
-- Author      : JP Moresmau
-- Copyright   : (c) JP Moresmau 2011
-- License     : BSD3
-- 
-- Maintainer  : jpmoresmau@gmail.com
-- Stability   : beta
-- Portability : portable
-- 
-- Abstract tests of the behavior
module Language.Haskell.BuildWrapper.Tests where

import Language.Haskell.BuildWrapper.Base

import Data.ByteString.Lazy ()
import Data.ByteString.Lazy.Char8()

import Data.Maybe
import Data.Aeson
import Data.Char

import Test.HUnit

import System.Directory
import System.FilePath
import System.Info

import Control.Monad

--import System.Time
tests :: (APIFacade a)=> [a -> Test]
tests=  [
        testSynchronizeAll,
        testSynchronizeDelete,
        testSynchronizeExtraFiles,
        testConfigureWarnings, 
        testConfigureErrors,
        testBuildErrors,
        testBuildWarnings,
        testBuildOutput,
        testModuleNotInCabal,
        testOutline,
        testOutlinePreproc,
        testOutlineImportExport,
        testOutlineLiterate,
        testOutlineComments,
        testOutlineMultiParam,
        testOutlineOperator,
        testOutlinePatternGuards,
        testOutlineExtension,
        testOutlineOptions,
        testPreviewTokenTypes,
        testThingAtPoint,
        testThingAtPointTypeReduction,
        testThingAtPointNotInCabal,
        testThingAtPointMain,
        testThingAtPointMainSubFolder,
        testNamesInScope,
        testNameDefsInScope,
        testInPlaceReference,
        testCabalComponents,
        testCabalDependencies,
        testNoSourceDir,
        testFlags,
        testBuildFlags
        ]

class APIFacade a where
        synchronize :: a -> FilePath -> Bool -> IO (OpResult ([FilePath],[FilePath]))
        synchronize1 :: a  -> FilePath -> Bool -> FilePath -> IO (Maybe FilePath)
        write :: a -> FilePath -> FilePath -> String -> IO ()
        configure :: a -> FilePath -> WhichCabal -> IO (OpResult Bool)
        configureWithFlags :: a -> FilePath -> WhichCabal -> String -> IO (OpResult Bool)
        build :: a -> FilePath -> Bool -> WhichCabal -> IO (OpResult BuildResult)
        build1 :: a -> FilePath -> FilePath -> IO (OpResult (Maybe [NameDef]))
        getBuildFlags :: a -> FilePath -> FilePath -> IO (OpResult BuildFlags)
        getOutline :: a -> FilePath ->  FilePath -> IO (OpResult OutlineResult)
        getTokenTypes :: a -> FilePath -> FilePath -> IO (OpResult [TokenDef])
        getOccurrences :: a -> FilePath -> FilePath -> String -> IO (OpResult [TokenDef])
        getThingAtPoint :: a -> FilePath -> FilePath -> Int -> Int -> IO (OpResult (Maybe ThingAtPoint))
        getNamesInScope :: a -> FilePath -> FilePath-> IO (OpResult (Maybe [String]))
        getCabalDependencies :: a -> FilePath -> IO (OpResult [(FilePath,[CabalPackage])])
        getCabalComponents :: a -> FilePath -> IO (OpResult [CabalComponent])
        generateUsage :: a -> FilePath -> Bool -> CabalComponent -> IO (OpResult (Maybe [FilePath]))

exeExtension :: String
#ifdef mingw32_HOST_OS
exeExtension = "exe"
#else
exeExtension = ""
#endif        

testSynchronizeAll :: (APIFacade a)=> a -> Test
testSynchronizeAll api= TestLabel "testSynchronizeAll" (TestCase ( do
        root<-createTestProject
        ((fps,dels),_)<-synchronize api root False
        assertBool "no file path on creation" (not $ null fps) 
        assertBool "deletions!" (null dels) 
        assertEqual "no cabal file" (testProjectName <.> ".cabal") (head fps)
        assertBool "no A" (("src" </> "A.hs") `elem` fps)
        assertBool "no C" (("src" </> "B" </> "C.hs") `elem` fps)
        assertBool "no Main" (("src" </> "Main.hs") `elem` fps)
        assertBool "no D" (("src" </> "B" </> "D.hs") `elem` fps)
        assertBool "no Main test" (("test" </> "Main.hs") `elem` fps)
        assertBool "no TestA" (("test" </> "TestA.hs") `elem` fps)
        ))

testSynchronizeDelete :: (APIFacade a)=> a -> Test
testSynchronizeDelete api= TestLabel "testSynchronizeDelete" (TestCase ( do
        root<-createTestProject
        ((fps0,dels0),_)<-synchronize api root False
        assertBool "no file path on creation" (not $ null fps0) 
        assertBool "deletions!" (null dels0) 
        let new=root </> ".dist-buildwrapper" </> "New.hs"
        writeFile new "module New where"
        ex1<-doesFileExist new
        assertBool "new does not exist" ex1
        ((_,dels),_)<-synchronize api root False
        assertBool "no deletions" (not $ null dels) 
        assertBool "no New.hs" ("New.hs" `elem` dels)
        ex2<-doesFileExist new
        assertBool "new does still exist" (not ex2)
        ))

testSynchronizeExtraFiles :: (APIFacade a)=> a -> Test
testSynchronizeExtraFiles api= TestLabel "testSynchronizeAll" (TestCase ( do
        root<-createTestProject
        let extra=root </> "src" -- need to be in hs-source-dirs
        writeFile (extra </> "a.txt") "contents"
        let new=root </> ".dist-buildwrapper" </> "src" </> "a.txt"
        ex1<-doesFileExist new
        assertBool "new does exist before synchronize" (not ex1)
        ((fps,_),_)<-synchronize api root False
        assertBool "no new in sync result" (("src" </> "a.txt") `elem` fps)
        ex2<-doesFileExist new
        assertBool "new does not exist after synchronize" ex2
        ))

testConfigureErrors :: (APIFacade a)=> a -> Test
testConfigureErrors api= TestLabel "testConfigureErrors" (TestCase ( do
        root<-createTestProject
        (boolNoCabal,nsNoCabal)<- configure api root Target
        assertBool "configure returned true on no cabal" (not boolNoCabal)
        --assertEqual "errors or warnings on no cabal (should be ignored)" 0 (length nsNoCabal)        
        let bw=head nsNoCabal
        assertEqual "wrong error on no cabal" BWError (bwnStatus bw)   
        
        synchronize api root False
        (boolOK,nsOK)<-configure api root Target
        assertBool "configure returned false" boolOK
        assertBool ("errors or warnings:"++show nsOK) (null nsOK)
        let cf=testCabalFile root
        let cfn=takeFileName cf
        writeFile cf $ unlines ["version:0.1",
                "build-type:     Simple"]
        synchronize api root False
        (bool1,nsErrors1)<-configure api root Target
        assertBool "bool1 returned true" (not bool1)
        assertEqual "no errors on no name" 2 (length nsErrors1)
        let (nsError1:nsError2:[])=nsErrors1
        assertEqual "not proper error 1" (BWNote BWError "No 'name' field.\n" (mkEmptySpan cfn 1 1)) nsError1
        assertEqual "not proper error 2" (BWNote BWError "No executables and no library found. Nothing to do.\n" (mkEmptySpan cfn 1 1)) nsError2
        writeFile cf $ unlines ["name: 4 P1",
                "version:0.1",
                "build-type:     Simple"]
        synchronize api root False
        (bool2,nsErrors2)<-configure api root Target
        assertBool "bool2 returned true" (not bool2)
        assertEqual "no errors on invalid name" 1 (length nsErrors2)
        let (nsError3:[])=nsErrors2
        assertEqual "not proper error 3" (BWNote BWError "Parse of field 'name' failed.\n" (mkEmptySpan cfn 1 1)) nsError3
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
        synchronize api root False
        (bool3,nsErrors3)<-configure api root Target
        assertBool "bool3 returned true" (not bool3)
        assertEqual "no errors on unknown dependency" 1 (length nsErrors3)
        let (nsError4:[])=nsErrors3
        assertEqual "not proper error 4" (BWNote BWError "At least the following dependencies are missing:\ntoto -any\n" (mkEmptySpan cfn 1 1)) nsError4
        
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
        synchronize api root False
        (bool4,nsErrors4)<-configure api root Target
        assertBool "bool4 returned true" (not bool4)
        assertEqual "no errors on unknown dependencies" 1 (length nsErrors4)
        let (nsError5:[])=nsErrors4
        assertEqual "not proper error 5" (BWNote BWError "At least the following dependencies are missing:\ntiti -any, toto -any\n" (mkEmptySpan cfn 1 1)) nsError5
        (BuildResult bool4b _,nsErrors4b)<-build api root False Source
        assertBool "bool4b returned true" (not bool4b)
        assertEqual "no errors on unknown dependencies" 1 (length nsErrors4b)
        let (nsError5b:[])=nsErrors4b
        assertEqual "not proper error 5b" (BWNote BWError "At least the following dependencies are missing:\ntiti -any, toto -any\n" (mkEmptySpan cfn 1 1)) nsError5b
        writeFile cf $ unlines ["name: "++testProjectName,
                "version:0.1",
                "cabal-version:  >= 1.8",
                "build-type:     Simple",
                "",
                "executable BWTest",
                "  hs-source-dirs:  src",
                "  other-modules:  B.D",
                "  build-depends:  base"]
        synchronize api root False
        (bool5,nsErrors5)<-configure api root Target
        assertBool "bool5 returned true" (not bool5)
        assertEqual "no errors on no main" 1 (length nsErrors5)
        let (nsError6:[])=nsErrors5
        assertEqual "not proper error 6" (BWNote BWError "No 'Main-Is' field found for executable BWTest\n" (mkEmptySpan cfn 1 1)) nsError6
        
        ))
        
testConfigureWarnings :: (APIFacade a)=> a -> Test
testConfigureWarnings api = TestLabel "testConfigureWarnings" (TestCase ( do
        root<-createTestProject
        synchronize api root False
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
        synchronize api root False
        (bool1,ns1)<- configure api root Target
        assertBool ("returned false 1 " ++ show ns1) bool1
        assertEqual ("didn't return 1 warning: " ++ show ns1) 1 (length ns1)
        let (nsWarning1:[])=ns1
        assertEqual "not proper warning 1" (BWNote BWWarning "Unknown fields: field1 (line 5)\nFields allowed in this section:\nname, version, cabal-version, build-type, license, license-file,\ncopyright, maintainer, build-depends, stability, homepage,\npackage-url, bug-reports, synopsis, description, category, author,\ntested-with, data-files, data-dir, extra-source-files,\nextra-tmp-files\n" (mkEmptySpan cfn 5 1)) nsWarning1
        writeFile cf $ unlines ["name: "++testProjectName,
                "version:0.1",
                "build-type:     Simple",
                "",
                "library",
                "  hs-source-dirs:  src",
                "  exposed-modules: A",
                "  other-modules:  B.C",
                "  build-depends:   base"]
        synchronize api root False
        (bool2,ns2)<- configure api root Target
        assertBool ("returned false 2 " ++ show ns2) bool2
        assertEqual ("didn't return 1 warning: " ++ show ns1) 1 (length ns2)
        let (nsWarning2:[])=ns2
        assertEqual "not proper warning 2" (BWNote BWWarning "A package using section syntax must specify at least\n'cabal-version: >= 1.2'.\n" (mkEmptySpan cfn 1 1)) nsWarning2
        writeFile cf $ unlines ["name: "++testProjectName,
                "version:0.1",
                "cabal-version:  >= 1.2",
                "",
                "library",
                "  hs-source-dirs:  src",
                "  exposed-modules: A",
                "  other-modules:  B.C",
                "  build-depends:   base"]
        writeFile (takeDirectory cf </> "Setup.hs") $ unlines ["import Distribution.Simple",
                        "main = defaultMain"]
        synchronize api root False
        (bool3,ns3)<- configure api root Target
        assertBool ("returned false 3 " ++ show ns3) bool3
        assertEqual ("didn't return 1 warning: " ++ show ns1) 1 (length ns3)
        let (nsWarning3:[])=ns3
        assertEqual "not proper warning 3" (BWNote BWWarning "No 'build-type' specified. If you do not need a custom Setup.hs or\n./configure script then use 'build-type: Simple'.\n" (mkEmptySpan cfn 1 1)) nsWarning3

        ))   
        
testBuildErrors :: (APIFacade a)=> a -> Test
testBuildErrors api = TestLabel "testBuildErrors" (TestCase ( do
        root<-createTestProject
        synchronize api root False
        (boolOKc,nsOKc)<-configure api root Target
        assertBool "returned false on configure" boolOKc
        assertBool ("errors or warnings on configure:"++show nsOKc) (null nsOKc)
        (BuildResult boolOK _,nsOK)<-build api root False Source
        assertBool "returned false on build" boolOK
        assertBool ("errors or warnings on build:"++show nsOK) (null nsOK)
        let rel="src"</>"A.hs"
        -- write source file
        
        writeFile (root </> rel) $ unlines ["module A where","import toto","fA=undefined"]
        synchronize1 api root True rel
        (bool11,nsErrors11)<-build1 api root rel
        assertBool "returned true on bool1_1" (isNothing bool11)
        assertBool "no errors or warnings on nsErrors11" (not $ null nsErrors11)
        let (nsError11:[])=nsErrors11
        assertEqualNotesWithoutSpaces "not proper error 1_1" (BWNote BWError "parse error on input `toto'\n" (mkEmptySpan rel 2 8)) nsError11
        (BuildResult bool1 _,nsErrors1)<-build api root False Source
        assertBool "returned true on bool1" (not bool1)
        assertBool "no errors or warnings on nsErrors1" (not $ null nsErrors1)
        let (nsError1:[])=nsErrors1
        assertEqualNotesWithoutSpaces "not proper error 1" (BWNote BWError "parse error on input `toto'\n" (mkEmptySpan rel 2 8)) nsError1
        
            -- write file and synchronize
        writeFile (root </> "src"</>"A.hs")$ unlines ["module A where","import Toto","fA=undefined"]
        mf2<-synchronize1 api root True rel
        assertBool "mf2 not just" (isJust mf2)
        (BuildResult bool2 _,nsErrors2)<-build api root False Source
        assertBool "returned true on bool2" (not bool2)
        assertBool "no errors or warnings on nsErrors2" (not $ null nsErrors2)
        let (nsError2:[])=nsErrors2
        assertEqualNotesWithoutSpaces "not proper error 2" (BWNote BWError "Could not find module `Toto':\n      Use -v to see a list of the files searched for.\n" (mkEmptySpan rel 2 8)) nsError2
        synchronize1 api root True rel
        (bool21,nsErrors21)<-build1 api root rel
        assertBool "returned true on bool21" (isNothing bool21)
        assertBool "no errors or warnings on nsErrors2_1" (not $ null nsErrors21)
        let (nsError21:[])=nsErrors21
        assertEqualNotesWithoutSpaces "not proper error 2_1" (BWNote BWError "Could not find module `Toto':\n      Use -v to see a list of the files searched for.\n" (mkEmptySpan rel 2 8)) nsError21
        (_,nsErrors3f)<- getBuildFlags api root ("src"</>"A.hs")
        assertBool ("errors or warnings on nsErrors3f:" ++ show nsErrors3f) (null nsErrors3f)
        (bool3,nsErrors3)<-build1 api root rel
        assertBool "returned true on bool3" (isNothing bool3)
        assertBool "no errors or warnings on nsErrors3" (not $ null nsErrors3)
        let (nsError3:[])=nsErrors3
        assertEqualNotesWithoutSpaces "not proper error 3" (BWNote BWError "Could not find module `Toto':\n  Use -v to see a list of the files searched for." (mkEmptySpan rel 2 8)) nsError3
        
        ))        
        
testBuildWarnings :: (APIFacade a)=> a -> Test
testBuildWarnings api = TestLabel "testBuildWarnings" (TestCase ( do
        root<-createTestProject
        synchronize api root False
        --let cf=testCabalFile root      
        writeFile (root </> (testProjectName <.> ".cabal")) $ unlines ["name: "++testProjectName,
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
        (boolOK,nsOK)<-configure api root Source
        assertBool "returned false" boolOK
        assertBool ("errors or warnings:"++show nsOK) (null nsOK)
        let rel="src"</>"A.hs"
        writeFile (root </> rel) $ unlines ["module A where","import Data.List","fA=undefined"] 
        mf2<-synchronize1 api root True rel
        assertBool "mf2 not just" (isJust mf2)
        (BuildResult bool1 fps1,nsErrors1)<-build api root False Source
        assertBool "returned false on bool1" bool1
        assertBool "no errors or warnings on nsErrors1" (not $ null nsErrors1)
        assertBool ("no rel in fps1: " ++ show fps1) (rel `elem` fps1)
        let (nsError1:nsError2:[])=nsErrors1
        assertEqualNotesWithoutSpaces "not proper error 1" (BWNote BWWarning "The import of `Data.List' is redundant\n               except perhaps to import instances from `Data.List'\n             To import instances alone, use: import Data.List()\n" (mkEmptySpan rel 2 1)) nsError1
        assertEqualNotesWithoutSpaces "not proper error 2" (BWNote BWWarning "Top-level binding with no type signature:\n               fA :: forall a. a\n" (mkEmptySpan rel 3 1)) nsError2
        (_,nsErrors3f)<-getBuildFlags api root rel
        assertBool "errors or warnings on nsErrors3f" (null nsErrors3f)
        (bool3,nsErrors3)<-build1 api root rel
        assertBool "returned false on bool3" (isJust bool3)
        assertEqual "not 2 errors or warnings on nsErrors3" 2 (length nsErrors3)
        let (nsError3:nsError4:[])=nsErrors3
        assertEqualNotesWithoutSpaces "not proper error 3" (BWNote BWWarning "The import of `Data.List' is redundant\n           except perhaps to import instances from `Data.List'\n         To import instances alone, use: import Data.List()" (mkEmptySpan rel 2 1)) nsError3
        assertEqualNotesWithoutSpaces "not proper error 4" (BWNote BWWarning "Top-level binding with no type signature:\n           fA :: forall a. a" (mkEmptySpan rel 3 1)) nsError4
        writeFile (root </> rel) $ unlines ["module A where","pats:: String -> String","pats a=reverse a","fB:: String -> Char","fB pats=head pats"] 
        mf3<-synchronize1 api root True rel
        assertBool "mf3 not just" (isJust mf3)
        (bool4,nsErrors4)<-build1 api root rel
        assertBool "returned false on bool4" (isJust bool4)
        assertBool "no errors or warnings on nsErrors4" (not $ null nsErrors4)
        let (nsError5:[])=nsErrors4
        assertEqualNotesWithoutSpaces "not proper error 5" (BWNote BWWarning ("This binding for `pats' shadows the existing binding\n           defined at "++rel++":3:1") (mkEmptySpan rel 4 5)) nsError5
        )) 
        
testBuildOutput :: (APIFacade a)=> a -> Test
testBuildOutput api = TestLabel "testBuildOutput" (TestCase ( do       
        root<-createTestProject
        synchronize api root False
        build api root True Source
        let exeN=case os of
                        "mingw32" -> addExtension testProjectName "exe"
                        _   -> testProjectName
        let exeF=root </> ".dist-buildwrapper" </> "dist" </> "build" </> testProjectName </> exeN
        exeE1<-doesFileExist exeF
        assertBool ("exe does not exist on build output: "++exeF) exeE1
        removeFile exeF
        exeE2<-doesFileExist exeF
        assertBool ("exe does still exist after deletion: "++exeF) (not exeE2)
        build api root False Source
        exeE3<-doesFileExist exeF
        assertBool ("exe exists after build no output: "++exeF) (not exeE3)
        ))
        
testModuleNotInCabal :: (APIFacade a)=> a -> Test
testModuleNotInCabal api = TestLabel "testModuleNotInCabal" (TestCase ( do
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"
        writeFile (root </> rel) $ unlines ["module A where","import Auto","fA=undefined"] 
        let rel2="src"</>"Auto.hs"
        writeFile (root </> rel2) $ unlines ["module Auto where","fAuto=undefined"] 
        synchronize api root False
        (BuildResult bool1 _,nsErrors1)<-build api root True Source
        assertBool "returned false on bool1" bool1
        assertBool "errors or warnings on nsErrors1" (null nsErrors1)
        (_,nsErrors2f)<-getBuildFlags api root rel
        assertBool "no errors or warnings on nsErrors2f" (null nsErrors2f)
        (bool2, nsErrors2)<-build1 api root rel
        assertBool ("returned false on bool2: " ++ show nsErrors2) (isJust bool2)
        assertBool ("errors or warnings on nsErrors2: " ++ show nsErrors2) (null nsErrors2)
        
        ))      
        
      
testOutline :: (APIFacade a)=> a -> Test
testOutline api= TestLabel "testOutline" (TestCase ( do
        root<-createTestProject
        synchronize api root False
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
        (_,nsErrors3f)<-getBuildFlags api root rel
        assertBool "errors or warnings on nsErrors3f" (null nsErrors3f)        
        (OutlineResult defs es is,nsErrors1)<-getOutline api root rel
        assertBool ("errors or warnings on getOutline:"++show nsErrors1) (null nsErrors1)
        let expected=[
                mkOutlineDefWithChildren "XList" [Data,Family] (InFileSpan (InFileLoc 8 1)(InFileLoc 8 20))  []
                ,mkOutlineDefWithChildren "XList Char" [Data,Instance] (InFileSpan (InFileLoc 11 1)(InFileLoc 11 60)) [
                        mkOutlineDef "XCons" [Constructor] (InFileSpan (InFileLoc 11 28)(InFileLoc 11 53))
                        ,mkOutlineDef "XNil" [Constructor] (InFileSpan (InFileLoc 11 56)(InFileLoc 11 60)) 
                        ]
                ,mkOutlineDef "Elem" [Type,Family] (InFileSpan (InFileLoc 13 1)(InFileLoc 13 19))
                ,mkOutlineDef "Elem [e]" [Type,Instance] (InFileSpan (InFileLoc 15 1)(InFileLoc 15 27)) 
                ,OutlineDef "testfunc1" [Function] (InFileSpan (InFileLoc 17 1)(InFileLoc 18 25)) [] (Just "[Char]") Nothing
                ,OutlineDef "testfunc1bis" [Function] (InFileSpan (InFileLoc 20 1)(InFileLoc 22 25)) [] (Just "String -> [Char]") Nothing      
                ,OutlineDef "testMethod" [Function] (InFileSpan (InFileLoc 24 1)(InFileLoc 27 13))  [] (Just "forall a . (Num a) => a -> a -> a") Nothing 
                ,mkOutlineDefWithChildren "ToString" [Class] (InFileSpan (InFileLoc 29 1)(InFileLoc 32 0))  [
                        mkOutlineDef "toString" [Function] (InFileSpan (InFileLoc 30 5)(InFileLoc 30 28))
                        ]          
                ,mkOutlineDefWithChildren "ToString String" [Instance] (InFileSpan (InFileLoc 32 1)(InFileLoc 35 0))  [
                        mkOutlineDef "toString" [Function] (InFileSpan (InFileLoc 33 5)(InFileLoc 33 18)) 
                        ]    
                ,OutlineDef "Str" [Type] (InFileSpan (InFileLoc 35 1)(InFileLoc 35 16)) [] (Just "String") Nothing              
                ,mkOutlineDefWithChildren "Type1" [Data] (InFileSpan (InFileLoc 37 1)(InFileLoc 41 10))  [
                         mkOutlineDef "MkType1_1" [Constructor] (InFileSpan (InFileLoc 37 12)(InFileLoc 37 25)) 
                        ,mkOutlineDefWithChildren "MkType1_2" [Constructor] (InFileSpan (InFileLoc 38 7)(InFileLoc 41 10)) [
                                mkOutlineDef "mkt2_s" [Field] (InFileSpan (InFileLoc 39 9)(InFileLoc 39 25)) 
                                ,mkOutlineDef "mkt2_i" [Field] (InFileSpan (InFileLoc 40 9)(InFileLoc 40 22)) 
                                
                                ]
                        
                        ]
                ]
        assertEqual "length" (length expected) (length defs)
        mapM_ (uncurry (assertEqual "outline")) (zip expected defs)
        assertEqual "exports" [] es
        assertEqual "imports" [ImportDef "Data.Char" Nothing (InFileSpan (InFileLoc 5 1)(InFileLoc 5 17)) False False "" Nothing] is
      ))   
        
testOutlineComments :: (APIFacade a)=> a -> Test
testOutlineComments api= TestLabel "testOutlineComments" (TestCase ( do
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"
        -- use api to write temp file
        write api root rel $ unlines [
                "{-# LANGUAGE RankNTypes, TypeSynonymInstances, TypeFamilies #-}",
                "",
                "module Module1 where",
                "",
                "import Data.Char",
                "",
                "-- testFunc1 comment",
                "testfunc1 :: [Char]",
                "testfunc1=reverse \"test\"",
                "",
                "-- | testFunc1bis haddock",
                "testfunc1bis :: String -> [Char]",
                "testfunc1bis []=\"nothing\"",
                "testfunc1bis s=reverse s",
                "",
                "-- | testMethod",
                "-- haddock",
                "testMethod :: forall a. (Num a) => a -> a -> a",
                "testMethod a b=",
                "    let e=a + (fromIntegral $ length testfunc1)",
                "    in e * 2",
                "",
                "class ToString a where",
                "    toString :: a -> String -- ^ toString comment",
                "",
                "-- | Str haddock", 
                "type Str=String",
                "",
                "-- | Type1 haddock",
                "data Type1=MkType1_1 Int -- ^ MkType1 comment",
                "    | MkType1_2 {",
                "        mkt2_s :: String,",
                "        mkt2_i :: Int",
                "        }" ]
        (_,nsErrors3f)<-getBuildFlags api root rel
        assertBool "errors or warnings on nsErrors3f" (null nsErrors3f)        
        (OutlineResult defs es is,nsErrors1)<-getOutline api root rel
        assertBool ("errors or warnings on getOutline:"++show nsErrors1) (null nsErrors1)
        let expected=[
                OutlineDef "testfunc1" [Function] (InFileSpan (InFileLoc 8 1)(InFileLoc 9 25)) [] (Just "[Char]") Nothing
                ,OutlineDef "testfunc1bis" [Function] (InFileSpan (InFileLoc 12 1)(InFileLoc 14 25)) [] (Just "String -> [Char]") (Just "testFunc1bis haddock")              
                ,OutlineDef "testMethod" [Function] (InFileSpan (InFileLoc 18 1)(InFileLoc 21 13))  [] (Just "forall a . (Num a) => a -> a -> a")   (Just "testMethod\n haddock") 
                ,mkOutlineDefWithChildren "ToString" [Class] (InFileSpan (InFileLoc 23 1)(InFileLoc 27 0))  [
                        OutlineDef "toString" [Function] (InFileSpan (InFileLoc 24 5)(InFileLoc 24 28)) [] Nothing (Just "toString comment")
                        ]          
                ,OutlineDef "Str" [Type] (InFileSpan (InFileLoc 27 1)(InFileLoc 27 16)) [] (Just "String") (Just "Str haddock")          
                ,OutlineDef "Type1" [Data] (InFileSpan (InFileLoc 30 1)(InFileLoc 34 10))  [
                         OutlineDef "MkType1_1" [Constructor] (InFileSpan (InFileLoc 30 12)(InFileLoc 30 25)) [] Nothing (Just "MkType1 comment") 
                        ,mkOutlineDefWithChildren "MkType1_2" [Constructor] (InFileSpan (InFileLoc 31 7)(InFileLoc 34 10)) [
                                mkOutlineDef "mkt2_s" [Field] (InFileSpan (InFileLoc 32 9)(InFileLoc 32 25)) 
                                ,mkOutlineDef "mkt2_i" [Field] (InFileSpan (InFileLoc 33 9)(InFileLoc 33 22)) 
                                
                                ]
                        
                        ] Nothing (Just "Type1 haddock")    
                ]
        assertEqual ("length:" ++ show defs) (length expected) (length defs)
        mapM_ (uncurry (assertEqual "outline")) (zip expected defs)
        assertEqual "exports" [] es
        assertEqual "imports" [ImportDef "Data.Char" Nothing (InFileSpan (InFileLoc 5 1)(InFileLoc 5 17)) False False "" Nothing] is
      ))           
        
testOutlinePreproc :: (APIFacade a)=> a -> Test
testOutlinePreproc api= TestLabel "testOutlinePreproc" (TestCase ( do
        root<-createTestProject
        synchronize api root False
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
        (_,nsErrors3f)<-getBuildFlags api root rel
        assertBool "errors or warnings on nsErrors3f" (null nsErrors3f)         
        (OutlineResult defs1 _ _,nsErrors1)<-getOutline api root rel
        assertBool ("errors or warnings on getOutlinePreproc 1:"++show nsErrors1) (null nsErrors1)
        let expected1=[
                mkOutlineDef "testfunc1" [Function] (InFileSpan (InFileLoc 6 1)(InFileLoc 6 25))
                ]
        assertEqual "length of expected1" (length expected1) (length defs1)
        mapM_ (uncurry (assertEqual "outline")) (zip expected1 defs1)
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
        (OutlineResult defs2 _ _,nsErrors2)<-getOutline api root rel
        assertBool ("errors or warnings on getOutlinePreproc:"++show nsErrors2) (null nsErrors2)
        let expected2=[
                mkOutlineDefWithChildren "Name" [Data] (InFileSpan (InFileLoc 5 1)(InFileLoc 9 38))  [
                  OutlineDef "Ident" [Constructor] (InFileSpan (InFileLoc 6 6)(InFileLoc 6 18)) [] Nothing (Just "/varid/ or /conid/."),
                  OutlineDef "Symbol" [Constructor] (InFileSpan (InFileLoc 7 6)(InFileLoc 7 19)) [] Nothing (Just "/varsym/ or /consym/")
                  ]
                ] 
        assertEqual "length of expected2" (length expected2) (length defs2)
        mapM_ (uncurry (assertEqual "outline")) (zip expected2 defs2)
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
        (OutlineResult defs3 _ _,nsErrors3)<-getOutline api root rel
        assertBool ("errors or warnings on getOutlinePreproc 3:"++show nsErrors3) (null nsErrors3)
        let expected3=[
                mkOutlineDef "testfunc1" [Function] (InFileSpan (InFileLoc 6 1)(InFileLoc 6 25)) 
                ]
        assertEqual "length of expected3" (length expected3) (length defs3)
        mapM_ (uncurry (assertEqual "outline")) (zip expected3 defs3)
      ))      
                       
       
testOutlineLiterate :: (APIFacade a)=> a -> Test
testOutlineLiterate api= TestLabel "testOutlineLiterate" (TestCase ( do
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.lhs"
        -- use api to write temp file
        write api root rel $ unlines [
                "comment 1",
                "",
                "> module Module1 where",
                "",
                "",
                "> testfunc1=reverse \"test\"",
                "",
                ""
                 ]
        (_,nsErrors3f)<-getBuildFlags api root rel
        assertBool "errors or warnings on nsErrors3f" (null nsErrors3f)
        (OutlineResult defs1 _ _,nsErrors1)<-getOutline api root rel
        assertBool ("errors or warnings on testOutlineLiterate 1:"++show nsErrors1) (null nsErrors1)
        let expected1=[
                mkOutlineDef "testfunc1" [Function] (InFileSpan (InFileLoc 6 3)(InFileLoc 6 27)) 
                ]
        assertEqual "length of expected1" (length expected1) (length defs1)
        mapM_ (uncurry (assertEqual "outline")) (zip expected1 defs1)
        ))       
       
testOutlineImportExport :: (APIFacade a)=> a -> Test
testOutlineImportExport api= TestLabel "testOutlineImportExport" (TestCase ( do
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"
        -- use api to write temp file
        write api root rel $ unlines [
                "module Module1 (dummy,module Data.Char,MkTest(..)) where",
                 "",
                "import Data.Char",
                "import Data.Map as DM (empty) ",
                "import Data.List hiding (orderBy,groupBy)",
                "import qualified Data.Maybe (Maybe(Just))"
                ]
        (_,nsErrors3f)<-getBuildFlags api root rel
        assertBool "errors or warnings on nsErrors3f" (null nsErrors3f)
        (OutlineResult _ es is,nsErrors1)<-getOutline api root rel
        assertBool ("errors or warnings on getOutline:"++show nsErrors1) (null nsErrors1)
        let exps=[
                ExportDef "dummy" IEVar (InFileSpan (InFileLoc 1 17)(InFileLoc 1 22)) [],
                ExportDef "Data.Char" IEModule (InFileSpan (InFileLoc 1 23)(InFileLoc 1 39)) [],
                ExportDef "MkTest" IEThingAll (InFileSpan (InFileLoc 1 40)(InFileLoc 1 50)) []
                ]
        mapM_ (uncurry (assertEqual "exports")) (zip exps es)
        let imps=[
                ImportDef "Data.Char" Nothing (InFileSpan (InFileLoc 3 1)(InFileLoc 3 17)) False False "" Nothing,
                ImportDef "Data.Map" Nothing (InFileSpan (InFileLoc 4 1)(InFileLoc 4 30)) False False "DM" (Just [ImportSpecDef "empty" IEVar (InFileSpan (InFileLoc 4 24)(InFileLoc 4 29)) []]),
                ImportDef "Data.List" Nothing (InFileSpan (InFileLoc 5 1)(InFileLoc 5 42)) False True "" (Just [ImportSpecDef "orderBy" IEVar (InFileSpan (InFileLoc 5 26)(InFileLoc 5 33)) [],ImportSpecDef "groupBy" IEVar (InFileSpan (InFileLoc 5 34)(InFileLoc 5 41)) []]),
                ImportDef "Data.Maybe" Nothing (InFileSpan (InFileLoc 6 1)(InFileLoc 6 42)) True False "" (Just [ImportSpecDef "Maybe" IEThingWith (InFileSpan (InFileLoc 6 30)(InFileLoc 6 41)) ["Just"]])
                ] 
        mapM_ (uncurry (assertEqual "imports")) (zip imps is)
        
        ))
            
testOutlineMultiParam  :: (APIFacade a)=> a -> Test
testOutlineMultiParam api= TestLabel "testOutlineMultiParam" (TestCase ( do
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"      
        write api root rel $ unlines [
                "{-# LANGUAGE MultiParamTypeClasses #-}",
                "module A where",
                "",
                "class C a b",
                "       where",
                "       c :: a -> b"
                ]
        let rel2="src"</>"B"</>"C.hs"      
        write api root rel2 $ unlines [
                "module B.C where",
                "",
                "import A",
                "myC ::", 
                "    (C a b)", 
                "    => ",
                "    a -> b", 
                "myC = c"
                ] 
        (_,nsErrors3f)<-getBuildFlags api root rel2
        assertBool "errors or warnings on nsErrors3f" (null nsErrors3f)
        (OutlineResult ors _ _,nsErrors1)<-getOutline api root rel2
        assertBool ("errors or warnings on getOutline:"++show nsErrors1) (null nsErrors1)
        assertBool "no outline" (not $ null ors)
        ))
    
testOutlineOperator  :: (APIFacade a)=> a -> Test
testOutlineOperator api= TestLabel "testOutlineMultiParam" (TestCase ( do
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"      
        write api root rel $ unlines [
                "{-# LANGUAGE MultiParamTypeClasses #-}",
                "module A           (  Collection (",
                "                     (>-)",
                "                    )",
                "   )where",
                "    infixl 5  >-",
                "    class Collection a where",  
                "        (>-)     :: Eq b => a b -> a b -> a b"
                ]
        (_,nsErrors3f)<-getBuildFlags api root rel
        assertBool "errors or warnings on nsErrors3f" (null nsErrors3f)
        (OutlineResult ors _ _,nsErrors1)<-getOutline api root rel
        assertBool ("errors or warnings on getOutline:"++show nsErrors1) (null nsErrors1)
        assertBool "no outline" (not $ null ors)
        ))    


testOutlinePatternGuards  :: (APIFacade a)=> a -> Test
testOutlinePatternGuards api= TestLabel "testOutlinePatternGuards" (TestCase ( do
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"      
        write api root rel $ unlines [
                "module A",
                "  where",
                "toto o | Just s<-o=s",
                "toto _=\"\"   "
                ]
        (_,nsErrors3f)<-getBuildFlags api root rel
        assertBool "errors or warnings on nsErrors3f" (null nsErrors3f)
        (bool3,nsErrors3)<-build1 api root rel
        assertBool "returned false on bool3" (isJust bool3)
        assertBool ("errors on nsErrors3"++ show nsErrors3) (not (any (\ x -> BWError == bwnStatus x) nsErrors3))
        (OutlineResult ors _ _,nsErrors1)<-getOutline api root rel
        assertBool ("errors or warnings on getOutline:"++show nsErrors1) (null nsErrors1)
        assertBool "no outline" (not $ null ors)
        )) 
                
testOutlineExtension     :: (APIFacade a)=> a -> Test
testOutlineExtension api= TestLabel "testOutlineExtension" (TestCase ( do
        root<-createTestProject
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
                "  extensions: EmptyDataDecls"]
        let rel="src"</>"A.hs"      
        write api root rel $ unlines [
                "module A",
                "  where",
                "data B"
                ]
        synchronize api root False        
        configure api root Source
        (_,nsErrors3f)<-getBuildFlags api root rel
        assertBool "errors or warnings on nsErrors3f" (null nsErrors3f)
        (bool3,nsErrors3)<-build1 api root rel
        assertBool "returned false on bool3" (isJust bool3)
        assertBool ("errors on nsErrors3"++ show nsErrors3) (not (any (\ x -> BWError == bwnStatus x) nsErrors3))
        (OutlineResult ors _ _,nsErrors1)<-getOutline api root rel
        assertBool ("errors or warnings on getOutline:"++show nsErrors1) (null nsErrors1)
        assertBool "no outline" (not $ null ors)
        ))             
              
testOutlineOptions :: (APIFacade a)=> a -> Test
testOutlineOptions api= TestLabel "testOutlineOptions" (TestCase ( do
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"      
        write api root rel $ unlines [
                "{-# OPTIONS -XMultiParamTypeClasses -XTupleSections -XRank2Types -XScopedTypeVariables -XTypeOperators #-}",
                "module A where",
                "-- MultiParamTypeClasses",
                "class C a b", 
                "-- TupleSections",
                "test1 = ((),)", 
                "-- Rank2Types",
                "test2 :: (forall a . a) -> b",
                "test2 a = undefined",
                "-- TypeOperators",
                "type a :-: b = (a,b)",
                "-- ScopedTypeVariables",
                "test3 :: forall a . a -> a",
                "test3 x = x :: a"
                ]
        (_,nsErrors3f)<-getBuildFlags api root rel
        assertBool "errors or warnings on nsErrors3f" (null nsErrors3f)
        (bool3,nsErrors3)<-build1 api root rel
        assertBool "returned false on bool3" (isJust bool3)
        assertBool ("errors on nsErrors3"++ show nsErrors3) (not (any (\ x -> BWError == bwnStatus x) nsErrors3))
        (OutlineResult ors _ _,nsErrors1)<-getOutline api root rel
        assertBool ("errors or warnings on getOutline:"++show nsErrors1) (null nsErrors1)
        assertBool "no outline" (not $ null ors)
        ))            
                
testPreviewTokenTypes :: (APIFacade a)=> a -> Test
testPreviewTokenTypes api= TestLabel "testPreviewTokenTypes" (TestCase ( do
        root<-createTestProject
        synchronize api root False
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
        let expectedS="[{\"D\":[1,1,37]},{\"D\":[2,1,13]},{\"K\":[3,1,7]},{\"IC\":[3,8,12]},{\"K\":[3,13,18]},{\"IV\":[5,1,5]},{\"S\":[5,6,8]},{\"IC\":[5,9,11]},{\"SS\":[5,12]},{\"IC\":[5,13,16]},{\"SS\":[5,16]},{\"IV\":[6,1,5]},{\"S\":[6,6]},{\"K\":[6,8,10]},{\"IV\":[7,9,15]},{\"SS\":[7,16]},{\"LC\":[7,17,20]},{\"S\":[7,20]},{\"LS\":[7,21,34]},{\"SS\":[7,34]},{\"IV\":[8,9,15]},{\"SS\":[8,16]},{\"LI\":[8,17]},{\"IV\":[8,19]},{\"LI\":[8,21]},{\"SS\":[8,22]},{\"PP\":[10,1,11]},{\"TH\":[11,1,3]},{\"IV\":[11,4,10]},{\"IV\":[11,11,23]},{\"TH\":[11,24,26]},{\"IC\":[11,26,35]},{\"SS\":[11,36]},{\"PP\":[12,1,7]}]"
        assertEqual "" expectedS (encode $ toJSON tts)
        ))
        
testThingAtPoint :: (APIFacade a)=> a -> Test
testThingAtPoint api= TestLabel "testThingAtPoint" (TestCase ( do
        root<-createTestProject
        synchronize api root False
        configure api root Target        
        let rel="src"</>"Main.hs"
        write api root rel $ unlines [  
                  "module Main where",
                  "main=return $ map id \"toto\"",
                  "",
                  "data DataT=MkData {name :: String}",
                  "",
                  "data Toot=Toot {toot :: String}",
                  "",
                  "fun1=let",
                  "    l2=reverse \"toto\"",
                  "    in head l2"
                  ] 
        (_,nsErrors3f)<-getBuildFlags api root rel
        assertBool "errors or warnings on nsErrors3f" (null nsErrors3f)          
        (tap1,nsErrors1)<-getThingAtPoint api root rel 2 16
        assertBool ("errors or warnings on getThingAtPoint1:"++show nsErrors1) (null nsErrors1)
        assertBool "not just tap1" (isJust tap1)
        assertEqual "not map" "map" (tapName $ fromJust tap1)
        assertEqual "not GHC.Base" (Just "GHC.Base") (tapModule $ fromJust tap1)
        assertEqual "not qtype"  (Just "(GHC.Types.Char -> GHC.Types.Char) -> [GHC.Types.Char] -> [GHC.Types.Char]") (tapQType $ fromJust tap1)
        assertEqual "not type"  (Just "(Char -> Char) -> [Char] -> [Char]") (tapType $ fromJust tap1)
        assertEqual "not htype"  (Just "v") (tapHType $ fromJust tap1)
        assertEqual "not gtype"  (Just "Var") (tapGType $ fromJust tap1)
        
        
        (tap2,nsErrors2)<-getThingAtPoint api root rel 2 20
        assertBool ("errors or warnings on getThingAtPoint2:"++show nsErrors2) (null nsErrors2)
        assertBool "not just tap2" (isJust tap2)
        assertEqual "not id" "id" (tapName $ fromJust tap2)
        assertEqual "not GHC.Base" (Just "GHC.Base") (tapModule $ fromJust tap2)
        assertEqual "not htype2"  (Just "v") (tapHType $ fromJust tap2)
        assertEqual "not qtype" (Just "GHC.Types.Char -> GHC.Types.Char") (tapQType $ fromJust tap2)
       
        (tap3,nsErrors3)<-getThingAtPoint api root rel 4 7
        assertBool ("errors or warnings on getThingAtPoint3:"++show nsErrors3) (null nsErrors3)
        assertBool "not just tap3" (isJust tap3)
        assertEqual "not DataT" "DataT" (tapName $ fromJust tap3)
        assertEqual "not Main" (Just "Main") (tapModule $ fromJust tap3)
        assertEqual "not htype3"  (Just "t") (tapHType $ fromJust tap3)
        assertEqual "qtype DataT" Nothing (tapQType $ fromJust tap3)
        
#if __GLASGOW_HASKELL__ < 704
        -- type information for constructors at the declaration is not supported by ghc 7.4       
        (tap4,nsErrors4)<-getThingAtPoint api root rel 4 14
        assertBool ("errors or warnings on getThingAtPoint4:"++show nsErrors4) (null nsErrors4)
        assertBool "not just tap4" (isJust tap4)
        assertEqual "not MkData" "MkData" (tapName $ fromJust tap4)
        assertEqual "not Main" (Just "Main") (tapModule $ fromJust tap4)
        assertEqual "not htype4"  (Just "v") (tapHType $ fromJust tap4)
        assertEqual "gtype MkData"  (Just "DataCon") (tapGType $ fromJust tap4)
        assertEqual "type MkData" (Just "String -> DataT") (tapType $ fromJust tap4)
        assertEqual "qtype MkData" (Just "GHC.Base.String -> Main.DataT") (tapQType $ fromJust tap4)
#endif

        (tap5,nsErrors5)<-getThingAtPoint api root rel 4 22
        assertBool ("errors or warnings on getThingAtPoint5:"++show nsErrors5) (null nsErrors5)
        assertBool "not just tap5" (isJust tap5)
        assertEqual "not name" "name" (tapName $ fromJust tap5)
        assertEqual "not Main" (Just "Main") (tapModule $ fromJust tap5)
        assertEqual "not htype5"  (Just "v") (tapHType $ fromJust tap5)
        assertEqual "qtype name" (Just "Main.DataT -> GHC.Base.String") (tapQType $ fromJust tap5)
        
        (tap6,nsErrors6)<-getThingAtPoint api root rel 6 7
        assertBool ("errors or warnings on getThingAtPoint6:"++show nsErrors6) (null nsErrors6)
        assertBool "not just tap6" (isJust tap6)
        assertEqual "not Toot" "Toot" (tapName $ fromJust tap6)
        assertEqual "not Main" (Just "Main") (tapModule $ fromJust tap6)
        assertEqual "not htype6"  (Just "t") (tapHType $ fromJust tap6)
        assertEqual "qtype Toot" Nothing (tapQType $ fromJust tap6)

#if __GLASGOW_HASKELL__ < 704
        -- type information for constructors at the declaration is not supported by ghc 7.4       
        (tap7,nsErrors7)<-getThingAtPoint api root rel 6 14
        assertBool ("errors or warnings on getThingAtPoint7:"++show nsErrors7) (null nsErrors7)
        assertBool "not just tap7" (isJust tap7)
        assertEqual "not Toot" "Toot" (tapName $ fromJust tap7)
        assertEqual "not Main" (Just "Main") (tapModule $ fromJust tap7)
        assertEqual "not htype7"  (Just "v") (tapHType $ fromJust tap7)
        assertEqual "qtype Toot" (Just "GHC.Base.String -> Main.Toot") (tapQType $ fromJust tap7)
        
        -- type information for field names at the declaration is not supported by ghc 7.4       
        (tap8,nsErrors8)<-getThingAtPoint api root rel 6 19
        assertBool ("errors or warnings on getThingAtPoint8:"++show nsErrors8) (null nsErrors8)
        assertBool "not just tap8" (isJust tap8)
        assertEqual "not toot" "toot" (tapName $ fromJust tap8)
        assertEqual "not Main" (Just "Main") (tapModule $ fromJust tap8)
        assertEqual "not htype8"  (Just "v") (tapHType $ fromJust tap8)
        assertEqual "qtype toot" (Just "Main.Toot -> GHC.Base.String") (tapQType $ fromJust tap8)
#endif        
                
        (tap9,nsErrors9)<-getThingAtPoint api root rel 9 5
        assertBool ("errors or warnings on getThingAtPoint9:"++show nsErrors9) (null nsErrors9)
        assertBool "not just tap9" (isJust tap9)
        assertEqual "not l2" "l2" (tapName $ fromJust tap9)
        assertEqual "not empty module" (Just "") (tapModule $ fromJust tap9)
        assertEqual "not htype9"  (Just "v") (tapHType $ fromJust tap9)
        assertEqual "qtype l2" (Just "[GHC.Types.Char]") (tapQType $ fromJust tap9)
      ))

testThingAtPointTypeReduction :: (APIFacade a)=> a -> Test
testThingAtPointTypeReduction api= TestLabel "testThingAtPointTypeReduction" (TestCase ( do
        root<-createTestProject
        let cf=testCabalFile root
        writeFile cf $ unlines ["name: "++testProjectName,
                "version:0.1",
                "cabal-version:  >= 1.8",
                "build-type:     Simple",
                "",
                "executable BWTest",
                "  hs-source-dirs:  src",
                "  main-is:         Main.hs",
                "  build-depends:  base, containers"]
        let rel="src"</>"Main.hs"
        synchronize api root False
        configure api root Source   
        write api root rel $ unlines [  
                  "module Main where",
                  "import qualified Data.Map as M",
                  "main=putStrLn \"M\"",
                  "",
                  "fun1 :: M.Map String Int",
                  "fun1 = M.insert \"key\" 1 M.empty"
                  ] 
        (_,nsErrorsMf)<-getBuildFlags api root rel
        assertBool "errors or warnings on nsErrorsMf" (null nsErrorsMf)          
        (tapM,nsErrorsM)<-getThingAtPoint api root rel 6 13
        assertBool ("errors or warnings on getThingAtPointM:"++show nsErrorsM) (null nsErrorsM)
        assertBool "not just tapM" (isJust tapM)
        assertEqual "not insert" "insert" (tapName $ fromJust tapM)
        assertEqual "not Data.Map module" (Just "Data.Map") (tapModule $ fromJust tapM)
        assertEqual "not htypeM"  (Just "v") (tapHType $ fromJust tapM)
        assertEqual "qtype insert" (Just "GHC.Base.String -> GHC.Types.Int -> Data.Map.Map GHC.Base.String GHC.Types.Int -> Data.Map.Map GHC.Base.String GHC.Types.Int") (tapQType $ fromJust tapM)
        )) 

testThingAtPointNotInCabal :: (APIFacade a)=> a -> Test
testThingAtPointNotInCabal api= TestLabel "testThingAtPointNotInCabal" (TestCase ( do
        root<-createTestProject
        synchronize api root False
        configure api root Target        
        let rel2="src"</>"Auto.hs"
        writeFile (root </> rel2) $ unlines ["module Auto where","fAuto=head [2,3,4]"] 
        synchronize api root False
        (_,nsErrors3f)<-getBuildFlags api root rel2
        assertBool "errors or warnings on nsErrors3f" (null nsErrors3f)
        (tap1,nsErrors1)<-getThingAtPoint api root rel2 2 8
        assertBool ("errors or warnings on getThingAtPoint1:"++show nsErrors1) (null nsErrors1)
        assertBool "not just tap1" (isJust tap1)
        assertEqual "not head" "head" (tapName $ fromJust tap1)
        assertEqual "not GHC.List" (Just "GHC.List") (tapModule $ fromJust tap1)
        assertEqual "not qtype"  (Just "[GHC.Integer.Type.Integer] -> GHC.Integer.Type.Integer") (tapQType $ fromJust tap1)
       )) 

testThingAtPointMain :: (APIFacade a)=> a -> Test
testThingAtPointMain api= TestLabel "testThingAtPointMain" (TestCase ( do
        root<-createTestProject
        let cf=testCabalFile root
        writeFile cf $ unlines ["name: "++testProjectName,
                "version:0.1",
                "cabal-version:  >= 1.8",
                "build-type:     Simple",
                "",
                "executable BWTest",
                "  hs-source-dirs:  src",
                "  main-is:         A.hs",
                "  other-modules:  B.D",
                "  build-depends:  base"]
        let rel="src"</>"A.hs"        
        writeFile (root </> rel) $ unlines [  
                  "module Main where",
                  "import B.D",
                  "main=return $ head [2,3,4]"
                  ]
        synchronize api root False
        configure api root Target     
        (bf3,nsErrors3f)<-getBuildFlags api root rel
        assertBool "errors or warnings on nsErrors3f" (null nsErrors3f)
        assertEqual "not main module" (Just "Main") (bfModName bf3)
        (tap1,nsErrors1)<-getThingAtPoint api root rel 3 16
        assertBool ("errors or warnings on getThingAtPoint1:"++show nsErrors1) (null nsErrors1)
        assertBool "not just tap1" (isJust tap1)
        assertEqual "not head" "head" (tapName $ fromJust tap1)
        assertEqual "not GHC.List" (Just "GHC.List") (tapModule $ fromJust tap1)
        assertEqual "not qtype"  (Just "[GHC.Integer.Type.Integer] -> GHC.Integer.Type.Integer") (tapQType $ fromJust tap1)
       
--        (tap2,nsErrors2)<-getThingAtPoint api root rel 2 8
--        assertBool ("errors or warnings on getThingAtPoint2:"++show nsErrors2) (null nsErrors2)
--        assertBool "not just tap2" (isJust tap2)
--        assertEqual "not B.D" "B.D" (tapName $ fromJust tap2)
--        assertEqual "not ModuleName" (Just "ModuleName") (tapGType $ fromJust tap2)
--        assertEqual "not m"  (Just "m") (tapHType $ fromJust tap2)
--       
       
       )) 
        
testThingAtPointMainSubFolder :: (APIFacade a)=> a -> Test
testThingAtPointMainSubFolder api= TestLabel "testThingAtPointMainSubFolder" (TestCase ( do
        root<-createTestProject
        let cf=testCabalFile root
        writeFile cf $ unlines ["name: "++testProjectName,
                "version:0.1",
                "cabal-version:  >= 1.8",
                "build-type:     Simple",
                "",
                "executable BWTest",
                "  hs-source-dirs:  src",
                "  main-is:        src2/A.hs",
                "  other-modules:  B.D",
                "  build-depends:  base"]
        let sf=root </> "src" </> "src2"
        createDirectory sf        
        let rel="src" </> "src2" </> "A.hs"        
        writeFile (root </> rel) $ unlines [  
                  "module Main where",
                  "import B.D",
                  "main=return $ head [2,3,4]"
                  ]
        synchronize api root False
        configure api root Target          
        (bf3,nsErrors3f)<-getBuildFlags api root rel
        assertBool "errors or warnings on nsErrors3f" (null nsErrors3f)
        assertEqual "not main module" (Just "Main") (bfModName bf3)
        (tap1,nsErrors1)<-getThingAtPoint api root rel 3 16
        assertBool ("errors or warnings on getThingAtPoint1:"++show nsErrors1) (null nsErrors1)
        assertBool "not just tap1" (isJust tap1)
        assertEqual "not head" "head" (tapName $ fromJust tap1)
        assertEqual "not GHC.List" (Just "GHC.List") (tapModule $ fromJust tap1)
        assertEqual "not qtype"  (Just "[GHC.Integer.Type.Integer] -> GHC.Integer.Type.Integer") (tapQType $ fromJust tap1)
     
        ))           

testNamesInScope :: (APIFacade a)=> a -> Test
testNamesInScope api= TestLabel "testNamesInScope" (TestCase ( do
        root<-createTestProject
        synchronize api root False
        configure api root Source        
        let rel="src"</>"Main.hs"
        writeFile (root </> rel) $ unlines [  
                  "module Main where",
                  "import B.D",
                  "main=return $ map id \"toto\""
                  ] 
        build api root True Source
        synchronize api root False
        --c1<-getClockTime
        (mtts,nsErrors1)<-getNamesInScope api root rel
        --c2<-getClockTime
        --putStrLn ("testNamesInScope: " ++ (timeDiffToString  $ diffClockTimes c2 c1))
        assertBool ("errors or warnings on getNamesInScope:"++show nsErrors1) (null nsErrors1)
        assertBool "getNamesInScope not just" (isJust mtts)
        let tts=fromJust mtts
        assertBool "does not contain Main.main" ("Main.main" `elem` tts)
        assertBool "does not contain B.D.fD" ("B.D.fD" `elem` tts)
        assertBool "does not contain GHC.Types.Char" ("GHC.Types.Char" `elem` tts)
        )) 
     
testNameDefsInScope :: (APIFacade a)=> a -> Test
testNameDefsInScope api= TestLabel "testNameDefsInScope" (TestCase ( do
        root<-createTestProject
        synchronize api root False
        configure api root Source        
        let rel="src"</>"Main.hs"
        writeFile (root </> rel) $ unlines [  
                  "module Main where",
                  "import B.D",
                  "main=return $ map id \"toto\"",
                  "data Type1=MkType1_1 Int"
                  ] 
        build api root True Source
        synchronize api root False
        (mtts,_)<-build1 api root rel
        assertBool "getNameDefsInScope not just" (isJust mtts)
        let tts=fromJust mtts
        assertBool "does not contain Main.main" (NameDef "Main.main" [Function] (Just "IO [Char]") `elem` tts)
        assertBool "does not contain B.D.fD" (NameDef "B.D.fD" [Function] (Just "forall a. a") `elem` tts)
        assertBool "does not contain Main.Type1" (NameDef "Main.Type1" [Type] Nothing `elem` tts)
        assertBool "does not contain Main.MkType1_1" (NameDef "Main.MkType1_1" [Constructor] (Just "Int -> Type1") `elem` tts)
        assertBool "does not contain GHC.Types.Char" (NameDef "GHC.Types.Char" [Type] Nothing `elem` tts)
        )) 
          
        
testInPlaceReference  :: (APIFacade a)=> a -> Test
testInPlaceReference api= TestLabel "testInPlaceReference" (TestCase ( do
        root<-createTestProject
        synchronize api root False
        writeFile (root </> (testProjectName <.> ".cabal")) $ unlines ["name: "++testProjectName,
                "version:0.1",
                "cabal-version:  >= 1.8",
                "build-type:     Simple",
                "",
                "library",
                "  hs-source-dirs:  src",
                "  exposed-modules: A,B.C",
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
        let rel="src"</>"Main.hs"          
        writeFile (root </> rel) $ unlines [  
                  "module Main where",
                  "import B.C",
                  "main=return $ map id \"toto\""
                  ]         
        let rel3="test"</>"TestA.hs"          
        writeFile (root </> rel3) $ unlines ["module TestA where","import A","fTA=undefined"]
        let rel2="src"</>"A.hs"
        writeFile (root </> rel2) $ unlines ["module A where","import B.C","fA=undefined"]          
        (boolOKc,nsOKc)<-configure api root Source
        assertBool ("returned false on configure:"++show nsOKc) boolOKc
        assertBool ("errors or warnings on configure:"++show nsOKc) (null nsOKc)
        (BuildResult boolOK _,nsOK)<-build api root True Source
        assertBool "returned false on build" boolOK
        assertBool ("errors or warnings on build:"++show nsOK) (null nsOK)
        synchronize api root True
        (mtts,nsErrors1)<-getNamesInScope api root rel
        assertBool ("errors or warnings on getNamesInScope in place:"++show nsErrors1) (null nsErrors1)
        assertBool "getNamesInScope in place not just" (isJust mtts)
        let tts=fromJust mtts
        assertBool "getNamesInScope in place 1 does not contain Main.main" ("Main.main" `elem` tts)
        assertBool "getNamesInScope in place 1 does not contain B.C.fC" ("B.C.fC" `elem` tts)
        (_,nsErrors3f)<-getBuildFlags api root rel
        assertBool "errors or warnings on nsErrors3f" (null nsErrors3f)
        (tap1,nsErrorsTap1)<-getThingAtPoint api root rel 3 16
        assertBool ("errors or warnings on getThingAtPoint1 in place:"++show nsErrorsTap1) (null nsErrorsTap1)
        assertBool "not just tap1" (isJust tap1)
        assertEqual "not map" "map" (tapName $ fromJust tap1)
        assertEqual "not GHC.Base" (Just "GHC.Base") (tapModule $ fromJust tap1)
        assertEqual "not qtype"  (Just "(GHC.Types.Char -> GHC.Types.Char) -> [GHC.Types.Char] -> [GHC.Types.Char]") (tapQType $ fromJust tap1)
        (mtts2,nsErrors2)<-getNamesInScope api root rel2
        assertBool ("errors or warnings on getNamesInScope in place 2:"++show nsErrors2) (null nsErrors2)
        assertBool "getNamesInScope in place 2 not just" (isJust mtts2)
        let tts2=fromJust mtts2
        assertBool "getNamesInScope in place 2 does not contain A.fA" ("A.fA" `elem` tts2)     
        assertBool "getNamesInScope in place 2 does not contain B.C.fC" ("B.C.fC" `elem` tts2) 
        
        (mtts3,nsErrors3)<-getNamesInScope api root rel3
        assertBool ("errors or warnings on getNamesInScope in place 3:"++show nsErrors3) (null nsErrors3)
        assertBool "getNamesInScope in place 3 not just" (isJust mtts3)
        let tts3=fromJust mtts3
        assertBool "getNamesInScope in place 3 does not contain A.fA" ("A.fA" `elem` tts3)       
        ))

testCabalComponents  :: (APIFacade a)=> a -> Test
testCabalComponents api= TestLabel "testCabalComponents" (TestCase ( do
        root<-createTestProject
        synchronize api root False
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
        configure api root Source
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
        synchronize api root False
        (cps,nsOK)<-getCabalDependencies api root
        assertBool ("errors or warnings on getCabalDependencies:"++show nsOK) (null nsOK)
        assertEqual "not two databases" 2 (length cps)
        let [(_,pkgs1),(_,pkgs2)] = cps -- One is global and one is local, but the order depends on the paths, 
            pkgs = pkgs1 ++ pkgs2       -- so we concatenate the two.
        let base=filter (\pkg->cpName pkg == "base") pkgs
        assertEqual "not 1 base" 1 (length base)
        let (l:ex:ts:[])=cpDependent $ head base
        assertEqual "not library true" (CCLibrary True) l
        assertEqual "not executable true" (CCExecutable "BWTest" True) ex
        assertEqual "not test suite true" (CCTestSuite "BWTest-test" True) ts
                ))


testNoSourceDir :: (APIFacade a)=> a -> Test
testNoSourceDir api=TestLabel "testNoSourceDir" (TestCase (do
        root<-createTestProject
        let cf=testCabalFile root
        writeFile cf $ unlines ["name: "++testProjectName,
                "version:0.1",
                "cabal-version:  >= 1.8",
                "build-type:     Simple",
                "",
                "library",
                "  exposed-modules: A",
                "  other-modules:  B.C",
                "  build-depends:   base",
                 "",
                "executable BWTest",
                "  main-is:        src/Main.hs",
                "  other-modules:  B.D",
                "  build-depends:  base",
                ""]
        let git=root </> ".git"
        createDirectoryIfMissing False git
        let gitInBW=root </> ".dist-buildwrapper" </> ".git"
        writeFile (git </> "testfile") "test"
        d1<-doesDirectoryExist gitInBW
        assertBool "git exists before synchronize" (not d1)
        
        let bwInBw=root </> ".dist-buildwrapper" </> ".dist-buildwrapper"
        d1b<-doesDirectoryExist bwInBw
        assertBool ".dist-buildwrapper exists before synchronize" (not d1b)
        
        synchronize api root False
        d2<-doesDirectoryExist gitInBW
        assertBool "git exists after synchronize" (not d2)
        
        d2b<-doesDirectoryExist bwInBw
        assertBool ".dist-buildwrapper exists after synchronize" (not d2b)
        
        ))

testFlags  :: (APIFacade a)=> a -> Test
testFlags api=TestLabel "testFlags" (TestCase (do
        root<-createTestProject
        let cf=testCabalFile root
        writeFile cf $ unlines ["name: "++testProjectName,
                "version:0.1",
                "cabal-version:  >= 1.8",
                "build-type:     Simple",
                "",
                "flag server",
                "  description: Install the scion-server.",
                "  default: False",
                "",
                "library",
                "  hs-source-dirs:  src",
                "  exposed-modules: A",
                "  other-modules:  B.C",
                "  build-depends:  base",
                "",
                "executable BWTest",
                "  if !flag(server)",
                "    buildable: False",
                "  hs-source-dirs:  src",
                "  main-is:         Main.hs",
                "  other-modules:  B.D",
                "  build-depends:  base",
                ""
                ]
        configure api root Source
        build api root True Source
        let exePath=root </> ".dist-buildwrapper" </> "dist" </> "build" </> testProjectName </> "BWTest" <.> exeExtension
        ex1<-doesFileExist exePath
        assertBool "exe exists!" (not ex1)
        
        configureWithFlags api root Source "server"
        build api root True Source
        ex2<-doesFileExist exePath
        assertBool "exe doesn't exist!" ex2
        
        ))    

testBuildFlags :: (APIFacade a)=> a -> Test
testBuildFlags api=TestLabel "testFlags" (TestCase (do
        root<-createTestProject
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
                "  extensions: OverlappingInstances"
                ]
        configure api root Source 
        let rel="src"</>"A.hs"  
        (flgs,nsErrors3f)<-getBuildFlags api root rel
        print flgs
        assertBool "errors or warnings on nsErrors3f" (null nsErrors3f)
        let ast=bfAst flgs   
        assertBool "no package-name" ("-package-name" `elem` ast)
        assertBool "no package name" ("BWTest-0.1" `elem` ast)
        assertBool "no -XOverlappingInstances" ("-XOverlappingInstances" `elem` ast)
        assertBool "OverlappingInstances" ("OverlappingInstances" `notElem` ast)
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
        
testSetupContents ::String
testSetupContents = unlines ["#!/usr/bin/env runhaskell",
        "import Distribution.Simple",
        "main :: IO ()",
        "main = defaultMain"]        
        
createTestProject :: IO FilePath
createTestProject = do
        temp<-getTemporaryDirectory
        let root=temp </> testProjectName
        ex<-doesDirectoryExist root
        when ex (removeDirectoryRecursive root)
        createDirectory root
        writeFile (testCabalFile root) testCabalContents
        writeFile (root </> "Setup.hs") testSetupContents
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
        
removeSpaces :: String -> String
removeSpaces = filter (/= ':') . filter (not . isSpace)

assertEqualNotesWithoutSpaces :: String -> BWNote -> BWNote -> IO()
assertEqualNotesWithoutSpaces msg n1 n2=do
        let
                n1'=n1{bwnTitle=removeSpaces $ bwnTitle n1}
                n2'=n1{bwnTitle=removeSpaces $ bwnTitle n2}
        assertEqual msg n1' n2'