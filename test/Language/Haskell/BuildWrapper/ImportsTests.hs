{-# LANGUAGE CPP,OverloadedStrings #-} 
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- |
-- Module      : Language.Haskell.BuildWrapper.ImportsTests
-- Copyright   : (c) JP Moresmau 2013
-- License     : BSD3
-- 
-- Maintainer  : jpmoresmau@gmail.com
-- Stability   : beta
-- Portability : portable
-- 
-- Test import cleaning and such
module Language.Haskell.BuildWrapper.ImportsTests where

import Language.Haskell.BuildWrapper.Base hiding (writeFile)
import Language.Haskell.BuildWrapper.CMDTests

import System.FilePath

import Test.Framework
import Test.HUnit (Assertion)

test_CleanImportsNothing :: Assertion
test_CleanImportsNothing = do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"
        -- use api to write temp file
        write api root rel $ unlines [
                "module A where",
                "",
                "f :: IO()",
                "f=putStrLn \"hello\""
                ]
        (ics,ns)<-cleanImports api root rel False
        assertBool $ null ns
        assertBool $ null ics
        
test_CleanImportsFunction :: Assertion
test_CleanImportsFunction = do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"
        -- use api to write temp file
        write api root rel $ unlines [
                "module A where",
                "",
                "import Data.Unique",
                "f = newUnique"
                ]
        (ics,ns)<-cleanImports api root rel False
        assertBool $ null ns
        assertEqual [ImportClean (InFileSpan (InFileLoc 3 1) (InFileLoc 3 19)) "import Data.Unique (newUnique)"] ics

test_CleanImportsRemove :: Assertion
test_CleanImportsRemove = do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"
        -- use api to write temp file
        write api root rel $ unlines [
                "module A where",
                "",
                "import Data.Unique",
                "f = putStrLn \"hello\""
                ]
        (ics,ns)<-cleanImports api root rel False
        assertBool $ null ns
        assertEqual [ImportClean (InFileSpan (InFileLoc 3 1) (InFileLoc 3 19)) ""] ics


test_CleanImportsFunctionType :: Assertion
test_CleanImportsFunctionType = do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"
        -- use api to write temp file
        write api root rel $ unlines [
                "module A where",
                "",
                "import Data.Unique",
                "f :: IO Unique",
                "f = newUnique"
                ]
        (ics,ns)<-cleanImports api root rel False
        assertBool $ null ns
        assertEqual [ImportClean (InFileSpan (InFileLoc 3 1) (InFileLoc 3 19)) "import Data.Unique (newUnique, Unique)"] ics
        
        
test_CleanImportsConstructor :: Assertion
test_CleanImportsConstructor = do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"
        -- use api to write temp file
        write api root rel $ unlines [
                "module A where",
                "",
                "data MyData=MyCons Int"
                ]
        let rel2="src"</>"B"</>"C.hs"      
        write api root rel2 $ unlines [
                "module B.C where",
                "",
                "import A",
                "f = MyCons 2"
                ]        
        (ics,ns)<-cleanImports api root rel2 False
        assertBool $ null ns
        assertEqual [ImportClean (InFileSpan (InFileLoc 3 1) (InFileLoc 3 9)) "import A (MyData (MyCons))"] ics

test_CleanImportsConstructorTyped :: Assertion
test_CleanImportsConstructorTyped = do
        let api=cabalAPI
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
                "  build-depends:   base"]
        synchronize api root False
        configure api root Target
        let rel2="src"</>"B"</>"C.hs"      
        write api root rel2 $ unlines [
                "module B.C where",
                "",
                "import System.Exit",
                "f r= case r of",
                "  ExitFailure _ ->True",
                "  _->False"
                ]        
        (ics,ns)<-cleanImports api root rel2 False
        assertBool $ null ns
#if __GLASGOW_HASKELL__ != 704
        assertEqual [ImportClean (InFileSpan (InFileLoc 3 1) (InFileLoc 3 19)) "import System.Exit (ExitCode (ExitFailure))"] ics
#endif

test_CleanImportsFunctionInExport :: Assertion
test_CleanImportsFunctionInExport = do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"
        -- use api to write temp file
        write api root rel $ unlines [
                "module A where",
                "",
                "data MyData=MyCons Int",
                "myUnCons (MyCons i)=i"
                ]
        let rel2="src"</>"B"</>"C.hs"      
        write api root rel2 $ unlines [
                "module B.C (f,myUnCons) where",
                "",
                "import A",
                "f = MyCons 2"
                ]        
        (ics,ns)<-cleanImports api root rel2 False
        assertBool $ null ns
        assertEqual [ImportClean (InFileSpan (InFileLoc 3 1) (InFileLoc 3 9)) "import A (MyData (MyCons), myUnCons)"] ics

        
test_CleanImportsConstructorQualified :: Assertion
test_CleanImportsConstructorQualified = do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"
        -- use api to write temp file
        write api root rel $ unlines [
                "module A where",
                "",
                "data MyData=MyCons Int"
                ]
        let rel2="src"</>"B"</>"C.hs"      
        write api root rel2 $ unlines [
                "module B.C where",
                "",
                "import qualified A as MyA",
                "f = MyA.MyCons 2"
                ]        
        (ics,ns)<-cleanImports api root rel2 False
        assertBool $ null ns
        assertEqual [ImportClean (InFileSpan (InFileLoc 3 1) (InFileLoc 3 26)) "import qualified A as MyA (MyData (MyCons))"] ics

test_CleanImportsFunctionReExported :: Assertion
test_CleanImportsFunctionReExported = do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"
        -- use api to write temp file
        write api root rel $ unlines [
                "module A where",
                "",
                "import Data.Char",
                "f = toUpper"
                ]    
        (ics,ns)<-cleanImports api root rel False
        assertBool $ null ns
        assertEqual [ImportClean (InFileSpan (InFileLoc 3 1) (InFileLoc 3 17)) "import Data.Char (toUpper)"] ics

test_CleanImportsFunctionReExportedInExport :: Assertion
test_CleanImportsFunctionReExportedInExport = do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"
        -- use api to write temp file
        write api root rel $ unlines [
                "module A (f,toLower) where",
                "",
                "import Data.Char",
                "f = toUpper"
                ]    
        (ics,ns)<-cleanImports api root rel False
        assertBool $ null ns
        assertEqual [ImportClean (InFileSpan (InFileLoc 3 1) (InFileLoc 3 17)) "import Data.Char (toLower, toUpper)"] ics
        
test_CleanImportsFunctionReExportedWithOthers :: Assertion
test_CleanImportsFunctionReExportedWithOthers = do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"
        -- use api to write temp file
        write api root rel $ unlines [
                "module A where",
                "",
                "import Data.Char",
                "import Data.List",
                "f = toUpper",
                "f2 = unzip5",
                "f3 = isSeparator"
                ]    
        (ics,ns)<-cleanImports api root rel False
        assertBool $ null ns
        assertEqual [ImportClean (InFileSpan (InFileLoc 3 1) (InFileLoc 3 17)) 
                        "import Data.Char (isSeparator, toUpper)",
                ImportClean (InFileSpan (InFileLoc 4 1) (InFileLoc 4 17)) 
                        "import Data.List (unzip5)"] 
                ics

test_CleanImportsFormat :: Assertion
test_CleanImportsFormat = do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"
        -- use api to write temp file
        write api root rel $ unlines [
                "module A where",
                "",
                "import Data.Char",
                "import qualified Data.Unique as U",
                "f = toUpper",
                "f2 = U.newUnique",
                "f3 = isSeparator"
                ]            
        (ics,ns)<-cleanImports api root rel True
        assertBool $ null ns
        assertEqual [ImportClean (InFileSpan (InFileLoc 3 1) (InFileLoc 3 17)) 
                        "import           Data.Char        (isSeparator, toUpper)",
                ImportClean (InFileSpan (InFileLoc 4 1) (InFileLoc 4 34)) 
                        "import qualified Data.Unique as U (newUnique)"] 
                ics    
                
test_CleanImportsInfix :: Assertion
test_CleanImportsInfix = do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"
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
                "  build-depends:   base, filepath"]
        synchronize api root False
        configure api root Target
        -- use api to write temp file
        write api root rel $ unlines [
                "module A where",
                "",
                "import System.FilePath",
                "f = \"dir\" </> \"file\""
                ]            
        (ics,ns)<-cleanImports api root rel True
        assertBool $ null ns
        assertEqual [ImportClean (InFileSpan (InFileLoc 3 1) (InFileLoc 3 23)) 
                        "import System.FilePath ((</>))"
                ] 
                ics   
                
test_CleanImportsHiding :: Assertion
test_CleanImportsHiding = do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"
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
                "  build-depends:   base, bytestring"]
        synchronize api root False
        configure api root Target
        -- use api to write temp file
        write api root rel $ unlines [
                "module A where",
                "",
                "import Data.ByteString.Lazy.Char8 hiding (writeFile)",
                "f s= writeFile \"file\" s"
                ]            
        (ics,ns)<-cleanImports api root rel True
        assertBool $ null ns
        assertEqual [ImportClean (InFileSpan (InFileLoc 3 1) (InFileLoc 3 53)) 
                        ""
                ] 
                ics  
                
test_CleanImportsPrelude :: Assertion
test_CleanImportsPrelude = do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"
        -- use api to write temp file
        write api root rel $ unlines [
                "module A where",
                "",
                "import Data.List",
                "f = map id"
                ]            
        (ics,ns)<-cleanImports api root rel True
        assertBool $ null ns
        assertEqual [ImportClean (InFileSpan (InFileLoc 3 1) (InFileLoc 3 17)) 
                        ""]
                ics    
                                