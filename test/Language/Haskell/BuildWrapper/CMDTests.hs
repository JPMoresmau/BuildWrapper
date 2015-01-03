{-# LANGUAGE CPP, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- |
-- Module      : Language.Haskell.BuildWrapper.CMDTests
-- Copyright   : (c) JP Moresmau 2011
-- License     : BSD3
-- 
-- Maintainer  : jpmoresmau@gmail.com
-- Stability   : beta
-- Portability : portable
-- 
-- Testing via the executable interface
module Language.Haskell.BuildWrapper.CMDTests where

import Language.Haskell.BuildWrapper.Base hiding (writeFile,readFile)

import Data.ByteString.Lazy ()
import Data.ByteString.Lazy.Char8()

import Data.Maybe
import Data.Char

import System.Directory
import System.FilePath
import System.Info

import Control.Applicative ((<$>))
import Control.Monad


import Data.Attoparsec.ByteString
import Data.Aeson
import Data.Aeson.Parser
import qualified Data.ByteString.Char8 as BS
import Data.List
import System.Exit
import System.Process

import Test.Framework
import Test.HUnit (Assertion)
import System.IO (Handle, hPutStrLn, hFlush)

class APIFacade a where
        synchronize :: a -> FilePath -> Bool -> IO (OpResult ([FilePath],[FilePath]))
        synchronize1 :: a  -> FilePath -> Bool -> FilePath -> IO (Maybe FilePath)
        write :: a -> FilePath -> FilePath -> String -> IO ()
        configure :: a -> FilePath -> WhichCabal -> IO (OpResult Bool)
        configureWithFlags :: a -> FilePath -> WhichCabal -> String -> IO (OpResult Bool)
        build :: a -> FilePath -> Bool -> WhichCabal -> IO (OpResult BuildResult)
        build1 :: a -> FilePath -> FilePath -> IO (OpResult (Maybe [NameDef]))
        build1c :: a -> FilePath -> FilePath -> String -> IO (OpResult (Maybe [NameDef]))
        getBuildFlags :: a -> FilePath -> FilePath -> IO (OpResult BuildFlags)
        getOutline :: a -> FilePath ->  FilePath -> IO (OpResult OutlineResult)
        getOutlineNC :: a -> FilePath ->  FilePath -> IO (OpResult OutlineResult)
        getTokenTypes :: a -> FilePath -> FilePath -> IO (OpResult [TokenDef])
        getOccurrences :: a -> FilePath -> FilePath -> String -> IO (OpResult [TokenDef])
        getThingAtPoint :: a -> FilePath -> FilePath -> Int -> Int -> IO (OpResult (Maybe ThingAtPoint))
        getLocals :: a -> FilePath -> FilePath -> Int -> Int -> Int -> Int -> IO (OpResult [ThingAtPoint])
        eval :: a -> FilePath -> FilePath -> String-> IO (OpResult [EvalResult])
        getNamesInScope :: a -> FilePath -> FilePath-> IO (OpResult (Maybe [String]))
        getCabalDependencies :: a -> FilePath -> Maybe FilePath -> IO (OpResult [(FilePath,[CabalPackage])])
        getCabalComponents :: a -> FilePath -> IO (OpResult [CabalComponent])
        generateUsage :: a -> FilePath -> Bool -> CabalComponent -> IO (OpResult (Maybe [FilePath]))
        cleanImports :: a -> FilePath -> FilePath -> Bool -> IO (OpResult [ImportClean])
        clean :: a -> FilePath -> Bool -> IO Bool


data CMDAPI=CMDAPI {
        cabalExe :: String,
        cabalOpts :: [String]
        }



instance APIFacade CMDAPI where
        synchronize (CMDAPI c o) r ff= runAPI c r "synchronize" (("--force="++ show ff) : cmdOpts o)
        synchronize1 (CMDAPI c o) r ff fp= runAPI c r "synchronize1" (["--force="++show ff,"--file="++fp]++ cmdOpts o)
        write (CMDAPI c _) r fp s= runAPI c r "write" ["--file="++fp,"--contents="++s]
        configure (CMDAPI c o) r t= runAPI c r "configure" (("--cabaltarget="++ show t) : cmdOpts o)
        configureWithFlags (CMDAPI c o) r t fgs= runAPI c r "configure" (["--cabaltarget="++ show t,"--cabalflags="++ fgs]++ cmdOpts o)
        build (CMDAPI c o) r b wc= runAPI c r "build" (["--output="++ show b,"--cabaltarget="++ show wc]++ cmdOpts o)
        build1 (CMDAPI c _) r fp= runAPI c r "build1" ["--file="++fp]
        build1c (CMDAPI c _) r fp ccn= runAPI c r "build1" ["--file="++fp,"--component="++ccn]
        getBuildFlags (CMDAPI c _) r fp= runAPI c r "getbuildflags" ["--file="++fp]
        getOutline (CMDAPI c _) r fp= runAPI c r "outline" ["--file="++fp]
        getOutlineNC (CMDAPI c _) r fp= runAPI' False c r "outline" ["--file="++fp]
        getTokenTypes (CMDAPI c _) r fp= runAPI c r "tokentypes" ["--file="++fp]
        getOccurrences (CMDAPI c _) r fp s= runAPI c r "occurrences" ["--file="++fp,"--token="++s]
        getThingAtPoint (CMDAPI c _) r fp l cl= removeLayoutTAP <$> runAPI c r "thingatpoint" ["--file="++fp,"--line="++ show l,"--column="++ show cl]
        getLocals (CMDAPI c _) r fp sl sc el ec= runAPI c r "locals" ["--file="++fp,"--sline="++ show sl,"--scolumn="++ show sc,"--eline="++ show el,"--ecolumn="++ show ec]
        eval (CMDAPI c _) r fp ex= runAPI c r "eval" ["--file="++fp,"--expression="++ ex]
        getNamesInScope (CMDAPI c _) r fp= runAPI c r "namesinscope" ["--file="++fp]
        getCabalDependencies (CMDAPI c o) r mfp= runAPI c r "dependencies" (maybe [] (\x->["--sandbox="++x]) mfp ++ cmdOpts o)
        getCabalComponents (CMDAPI c _) r= runAPI c r "components" []
        generateUsage (CMDAPI c _) r retAll cc=runAPI c r "generateusage" ["--returnall="++ show retAll,"--cabalcomponent="++ cabalComponentName cc]
        cleanImports (CMDAPI c _) r fp fo= runAPI c r "cleanimports" ["--file="++fp,"--format="++ show fo]
        clean (CMDAPI c _) r e=runAPI c r "clean" ["--everything="++show e]
        
build1lr :: FilePath
                       -> String -> IO (Handle, Handle, Handle, ProcessHandle)
build1lr r fp= startAPIProcess r "build1" ["--file="++fp,"--longrunning=true"]

cabalAPI :: CMDAPI
cabalAPI= CMDAPI "cabal" []     

exeExtension :: String
#ifdef mingw32_HOST_OS
exeExtension = "exe"
#else
exeExtension = ""
#endif        

test_SynchronizeAll :: Assertion
test_SynchronizeAll = do
        let api=cabalAPI
        root<-createTestProject
        ((fps,dels),_)<-synchronize api root False
        assertBool (not $ null fps) 
        assertBool (null dels) 
        assertEqual (testProjectName <.> ".cabal") (head fps)
        assertBool (("src" </> "A.hs") `elem` fps)
        assertBool (("src" </> "B" </> "C.hs") `elem` fps)
        assertBool (("src" </> "Main.hs") `elem` fps)
        assertBool (("src" </> "B" </> "D.hs") `elem` fps)
        assertBool (("test" </> "Main.hs") `elem` fps)
        assertBool (("test" </> "TestA.hs") `elem` fps)
        
test_SynchronizeDelete :: Assertion
test_SynchronizeDelete = do
        let api=cabalAPI
        root<-createTestProject
        ((fps0,dels0),_)<-synchronize api root False
        assertBool (not $ null fps0) 
        assertBool (null dels0) 
        let new=root </> ".dist-buildwrapper" </> "New.hs"
        writeFile new "module New where"
        ex1<-doesFileExist new
        assertBool  ex1
        ((_,dels),_)<-synchronize api root False
        assertBool (not $ null dels) 
        assertBool ("New.hs" `elem` dels)
        ex2<-doesFileExist new
        assertBool (not ex2)


test_SynchronizeExtraFiles :: Assertion
test_SynchronizeExtraFiles = do
        let api=cabalAPI
        root<-createTestProject
        let extra=root </> "src" -- need to be in hs-source-dirs
        writeFile (extra </> "a.txt") "contents"
        let new=root </> ".dist-buildwrapper" </> "src" </> "a.txt"
        ex1<-doesFileExist new
        assertBool (not ex1)
        ((fps,_),_)<-synchronize api root False
        assertBool (("src" </> "a.txt") `elem` fps)
        ex2<-doesFileExist new
        assertBool ex2


test_ConfigureErrors :: Assertion
test_ConfigureErrors = do
        let api=cabalAPI
        root<-createTestProject
        (boolNoCabal,nsNoCabal)<- configure api root Target
        assertBool (not boolNoCabal)
        --assertEqual 0 (length nsNoCabal)        
        let bw=head nsNoCabal
        assertEqual BWError (bwnStatus bw)   
        
        synchronize api root False
        (boolOK,nsOK)<-configure api root Target
        assertBool boolOK
        assertBool (null nsOK)
        let cf=testCabalFile root
        let cfn=takeFileName cf
        writeFile cf $ unlines ["version:0.1",
                "build-type:     Simple"]
        synchronize api root False
        (bool1,nsErrors1)<-configure api root Target
        assertBool (not bool1)
        assertEqual 2 (length nsErrors1)
        let (nsError1:nsError2:[])=nsErrors1
        assertEqual (BWNote BWError "No 'name' field.\n" (mkEmptySpan cfn 1 1)) nsError1
        assertEqual (BWNote BWError "No executables and no library found. Nothing to do.\n" (mkEmptySpan cfn 1 1)) nsError2
        writeFile cf $ unlines ["name: 4 P1",
                "version:0.1",
                "build-type:     Simple"]
        synchronize api root False
        (bool2,nsErrors2)<-configure api root Target
        assertBool (not bool2)
        assertEqual 1 (length nsErrors2)
        let (nsError3:[])=nsErrors2
        assertEqual (BWNote BWError "Parse of field 'name' failed.\n" (mkEmptySpan cfn 1 1)) nsError3
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
        assertBool (not bool3)
        assertEqual 1 (length nsErrors3)
        let (nsError4:[])=nsErrors3
        assertEqual (BWNote BWError "At least the following dependencies are missing:\ntoto -any\n" (mkEmptySpan cfn 1 1)) nsError4
        
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
        assertBool (not bool4)
        assertEqual 1 (length nsErrors4)
        let (nsError5:[])=nsErrors4
        assertEqual (BWNote BWError "At least the following dependencies are missing:\ntiti -any, toto -any\n" (mkEmptySpan cfn 1 1)) nsError5
        (BuildResult bool4b _,nsErrors4b)<-build api root False Source
        assertBool (not bool4b)
        assertEqual 1 (length nsErrors4b)
        let (nsError5b:[])=nsErrors4b
        assertEqual (BWNote BWError "At least the following dependencies are missing:\ntiti -any, toto -any\n" (mkEmptySpan cfn 1 1)) nsError5b
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
        assertBool (not bool5)
        assertEqual 1 (length nsErrors5)
        let (nsError6:[])=nsErrors5
        assertEqual (BWNote BWError "no 'main-is' field found for executable bwtest\n" (mkEmptySpan cfn 1 1)) (nsError6{bwnTitle=map toLower $ bwnTitle nsError6})
        writeFile cf $ unlines ["name: "++testProjectName,
                "version:0.1",
                "cabal-version:  >= 1.2",
                "build-type:     Simple",
                "",
                "library",
                "  hs-source-dirs:  src",
                "  exposed-modules: A",
                "  other-modules:  B.C",
                "",
                "executable BWTest",
                "  hs-source-dirs: src",
                "  main-is:        Main.hs",
                "  other-modules:  B.D",
                "  build-depends:  base," ++ testProjectName]
        synchronize api root False
        (bool6,nsErrors6)<-configure api root Target
        assertBool (not bool6)
        assertEqual 1 (length nsErrors6)
        let (nsError7:[])=nsErrors6
        assertEqual (BWNote BWError ("The field 'build-depends: "++ testProjectName ++"' refers to a library which is defined\nwithin the same package. To use this feature the package must specify at least\n'cabal-version: >= 1.8'.\n") (mkEmptySpan cfn 1 1)) nsError7
            
        
test_ConfigureWarnings :: Assertion
test_ConfigureWarnings  = do
        let api=cabalAPI
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
        assertBool bool1
        assertEqual 1 (length ns1)
        let (nsWarning1:[])=ns1
        assertEqualNotesStart  (BWNote BWWarning "Unknown fields: field1 (line 5)" (mkEmptySpan cfn 5 1)) nsWarning1
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
        assertBool bool2
        assertEqual 1 (length ns2)
        let (nsWarning2:[])=ns2
        assertEqual (BWNote BWWarning "A package using section syntax must specify at least\n'cabal-version: >= 1.2'.\n" (mkEmptySpan cfn 1 1)) nsWarning2
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
        assertBool bool3
        assertEqual 1 (length ns3)
        let (nsWarning3:[])=ns3
        assertEqual (BWNote BWWarning "No 'build-type' specified. If you do not need a custom Setup.hs or\n./configure script then use 'build-type: Simple'.\n" (mkEmptySpan cfn 1 1)) nsWarning3

        
test_BuildErrors :: Assertion
test_BuildErrors  = do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        (boolOKc,nsOKc)<-configure api root Target
        assertBool boolOKc
        assertBool (null nsOKc)
        (BuildResult boolOK _,nsOK)<-build api root False Source
        assertBool boolOK
        assertBool (null nsOK)
        let rel="src"</>"A.hs"
        -- write source file
        
        writeFile (root </> rel) $ unlines ["module A where","import toto","fA=undefined"]
        synchronize1 api root True rel
        (bool11,nsErrors11)<-build1 api root rel
        assertBool (isNothing bool11)
        assertBool (not $ null nsErrors11)
        let (nsError11:[])=nsErrors11
        assertEqualNotesWithoutSpaces "not proper error 1_1" (BWNote BWError "parse error on input `toto'\n" (mkEmptySpan rel 2 8)) nsError11
        (BuildResult bool1 _,nsErrors1)<-build api root False Source
        assertBool (not bool1)
        assertBool (not $ null nsErrors1)
        let (nsError1:[])=nsErrors1
        assertEqualNotesWithoutSpaces "not proper error 1" (BWNote BWError "parse error on input `toto'\n" (mkEmptySpan rel 2 8)) nsError1
        
            -- write file and synchronize
        writeFile (root </> "src"</>"A.hs")$ unlines ["module A where","import Toto","fA=undefined"]
        mf2<-synchronize1 api root True rel
        assertBool (isJust mf2)
        (BuildResult bool2 _,nsErrors2)<-build api root False Source
        assertBool (not bool2)
        assertBool (not $ null nsErrors2)
        let (nsError2:[])=nsErrors2
        assertEqualNotesWithoutSpaces "not proper error 2" (BWNote BWError "Could not find module `Toto':\n      Use -v to see a list of the files searched for.\n" (mkEmptySpan rel 2 8)) nsError2
        synchronize1 api root True rel
        (bool21,nsErrors21)<-build1 api root rel
        assertBool (isNothing bool21)
        assertBool (not $ null nsErrors21)
        let (nsError21:[])=nsErrors21
        assertEqualNotesWithoutSpaces "not proper error 2_1" (BWNote BWError "Could not find module `Toto':\n      Use -v to see a list of the files searched for.\n" (mkEmptySpan rel 2 8)) nsError21
        (_,nsErrors3f)<- getBuildFlags api root ("src"</>"A.hs")
        assertBool  (null nsErrors3f)
        (bool3,nsErrors3)<-build1 api root rel
        assertBool  (isNothing bool3)
        assertBool  (not $ null nsErrors3)
        let (nsError3:[])=nsErrors3
        assertEqualNotesWithoutSpaces "not proper error 3" (BWNote BWError "Could not find module `Toto':\n  Use -v to see a list of the files searched for." (mkEmptySpan rel 2 8)) nsError3
        
     
        
test_BuildWarnings :: Assertion
test_BuildWarnings  = do
        let api=cabalAPI
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
        assertBool  boolOK
        assertBool  (null nsOK)
        let rel="src"</>"A.hs"
        writeFile (root </> rel) $ unlines ["module A where","import Data.List","fA=undefined"] 
        mf2<-synchronize1 api root True rel
        assertBool (isJust mf2)
        (BuildResult bool1 fps1,nsErrors1)<-build api root False Source
        assertBool bool1
        assertBool (not $ null nsErrors1)
        assertBool (rel `elem` fps1)
        let (nsError1:nsError2:[])=nsErrors1
#if __GLASGOW_HASKELL__ >=708
        let notype="Top-level binding with no type signature:\n               fA :: forall t. t"
#else
        let notype="Top-level binding with no type signature:\n               fA :: forall a. a"
#endif        
        assertEqualNotesWithoutSpaces "not proper error 1" (BWNote BWWarning "The import of `Data.List' is redundant\n               except perhaps to import instances from `Data.List'\n             To import instances alone, use: import Data.List()\n" (mkEmptySpan rel 2 1)) nsError1
        assertEqualNotesWithoutSpaces "not proper error 2" (BWNote BWWarning notype (mkEmptySpan rel 3 1)) nsError2
        (_,nsErrors3f)<-getBuildFlags api root rel
        assertBool  (null nsErrors3f)
        (bool3,nsErrors3)<-build1 api root rel
        assertBool (isJust bool3)
        assertEqual  2 (length nsErrors3)
        let (nsError3:nsError4:[])=nsErrors3
        assertEqualNotesWithoutSpaces "not proper error 3" (BWNote BWWarning "The import of `Data.List' is redundant\n           except perhaps to import instances from `Data.List'\n         To import instances alone, use: import Data.List()" (mkEmptySpan rel 2 1)) nsError3
        assertEqualNotesWithoutSpaces "not proper error 4" (BWNote BWWarning notype (mkEmptySpan rel 3 1)) nsError4
        writeFile (root </> rel) $ unlines ["module A where","pats:: String -> String","pats a=reverse a","fB:: String -> Char","fB pats=head pats"] 
        mf3<-synchronize1 api root True rel
        assertBool (isJust mf3)
        (bool4,nsErrors4)<-build1 api root rel
        assertBool (isJust bool4)
        assertBool (not $ null nsErrors4)
        let (nsError5:[])=nsErrors4
        assertEqualNotesWithoutSpaces "not proper error 5" (BWNote BWWarning ("This binding for `pats' shadows the existing binding\n           defined at "++rel++":3:1") (mkEmptySpan rel 4 5)) nsError5

        
test_BuildOutput :: Assertion
test_BuildOutput  = do      
        let api=cabalAPI 
        root<-createTestProject
        synchronize api root False
        build api root True Source
        let exeN=case os of
                        "mingw32" -> addExtension testProjectName "exe"
                        _   -> testProjectName
        let exeF=root </> ".dist-buildwrapper" </> "dist" </> "build" </> testProjectName </> exeN
        exeE1<-doesFileExist exeF
        assertBool  exeE1
        removeFile exeF
        exeE2<-doesFileExist exeF
        assertBool  (not exeE2)
        build api root False Source
        exeE3<-doesFileExist exeF
        assertBool (not exeE3)
        
-- |  http://hackage.haskell.org/trac/ghc/ticket/7380#comment:1     : -O2 is removed from the options  
test_BuildO2 :: Assertion
test_BuildO2  = do   
        let api=cabalAPI    
        root<-createTestProject
        write api root (testProjectName <.> ".cabal") $  unlines ["name: "++testProjectName,
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
                "  ghc-options:    -O2",
                "",
                "test-suite BWTest-test",
                "  type:            exitcode-stdio-1.0",
                "  hs-source-dirs:  test",
                "  main-is:         Main.hs",
                "  other-modules:  TestA",
                "  build-depends:  base",
                ""
                ]
        configure api root Target
        let rel="src"</>"Main.hs"
        writeFile (root </> rel) $ unlines ["module Main where","main :: IO()","main= putStrLn \"Hello World\""] 
        synchronize api root False
        (ns, _)<-build1 api root rel 
        assertBool (isJust ns)

        
test_ModuleNotInCabal :: Assertion
test_ModuleNotInCabal  = do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"
        writeFile (root </> rel) $ unlines ["module A where","import Auto","fA=undefined"] 
        let rel2="src"</>"Auto.hs"
        writeFile (root </> rel2) $ unlines ["module Auto where","fAuto=undefined"] 
        synchronize api root False
        (BuildResult bool1 _,nsErrors1)<-build api root True Source
        assertBool  bool1
        assertBool  (null nsErrors1)
        (_,nsErrors2f)<-getBuildFlags api root rel
        assertBool  (null nsErrors2f)
        (bool2, nsErrors2)<-build1 api root rel
        assertBool  (isJust bool2)
        assertBool  (null nsErrors2)
        
      
        
      
test_Outline :: Assertion
test_Outline = doTestOutline cabalAPI getOutline

test_OutlineNoCabal :: Assertion
test_OutlineNoCabal = doTestOutline cabalAPI getOutlineNC

doTestOutline :: APIFacade a => a -> (a -> FilePath ->  FilePath -> IO (OpResult OutlineResult)) -> Assertion
doTestOutline api f = do
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
        assertBool (null nsErrors3f)        
        (OutlineResult defs es is,nsErrors1)<-f api root rel
        assertBool  (null nsErrors1)
        let expected=[
                mkOutlineDefWithChildren "XList" [Data,Family] (InFileSpan (InFileLoc 8 1)(InFileLoc 8 20))  []
                ,mkOutlineDefWithChildren "XList Char" [Data,Instance] (InFileSpan (InFileLoc 11 1)(InFileLoc 11 60)) [
                        mkOutlineDef "XCons" [Constructor] (InFileSpan (InFileLoc 11 28)(InFileLoc 11 53))
                        ,mkOutlineDef "XNil" [Constructor] (InFileSpan (InFileLoc 11 56)(InFileLoc 11 60)) 
                        ]
                ,mkOutlineDef "Elem" [Type,Family] (InFileSpan (InFileLoc 13 1)(InFileLoc 13 19))
                ,mkOutlineDef "Elem [e]" [Type,Instance] (InFileSpan (InFileLoc 15 1)(InFileLoc 15 27)) 
                ,OutlineDef "testfunc1" [Function] (InFileSpan (InFileLoc 17 1)(InFileLoc 18 25)) [] (Just "[Char]") Nothing Nothing
                ,OutlineDef "testfunc1bis" [Function] (InFileSpan (InFileLoc 20 1)(InFileLoc 22 25)) [] (Just "String -> [Char]") Nothing Nothing      
                ,OutlineDef "testMethod" [Function] (InFileSpan (InFileLoc 24 1)(InFileLoc 27 13))  [] (Just "forall a . (Num a) => a -> a -> a") Nothing Nothing 
                ,mkOutlineDefWithChildren "ToString" [Class] (InFileSpan (InFileLoc 29 1)(InFileLoc 30 28))  [
                        mkOutlineDef "toString" [Function] (InFileSpan (InFileLoc 30 5)(InFileLoc 30 28))
                        ]          
                ,mkOutlineDefWithChildren "ToString String" [Instance] (InFileSpan (InFileLoc 32 1)(InFileLoc 35 0))  [
                        mkOutlineDef "toString" [Function] (InFileSpan (InFileLoc 33 5)(InFileLoc 33 18)) 
                        ]    
                ,OutlineDef "Str" [Type] (InFileSpan (InFileLoc 35 1)(InFileLoc 35 16)) [] (Just "String") Nothing Nothing             
                ,mkOutlineDefWithChildren "Type1" [Data] (InFileSpan (InFileLoc 37 1)(InFileLoc 41 10))  [
                         mkOutlineDef "MkType1_1" [Constructor] (InFileSpan (InFileLoc 37 12)(InFileLoc 37 25)) 
                        ,mkOutlineDefWithChildren "MkType1_2" [Constructor] (InFileSpan (InFileLoc 38 7)(InFileLoc 41 10)) [
                                mkOutlineDef "mkt2_s" [Field] (InFileSpan (InFileLoc 39 9)(InFileLoc 39 25)) 
                                ,mkOutlineDef "mkt2_i" [Field] (InFileSpan (InFileLoc 40 9)(InFileLoc 40 22)) 
                                
                                ]
                        
                        ]
                ]
        assertEqual  (length expected) (length defs)
        mapM_ (uncurry assertEqual) (zip expected defs)
        assertEqual  [] es
        assertEqual  [ImportDef "Data.Char" Nothing (InFileSpan (InFileLoc 5 1)(InFileLoc 5 17)) False False "" Nothing] is
  
        
test_OutlineComments :: Assertion
test_OutlineComments= do
        let api=cabalAPI
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
        assertBool (null nsErrors3f)        
        (OutlineResult defs es is,nsErrors1)<-getOutline api root rel
        assertBool (null nsErrors1)
        let expected=[
                OutlineDef "testfunc1" [Function] (InFileSpan (InFileLoc 8 1)(InFileLoc 9 25)) [] (Just "[Char]") Nothing Nothing
                ,OutlineDef "testfunc1bis" [Function] (InFileSpan (InFileLoc 12 1)(InFileLoc 14 25)) [] (Just "String -> [Char]") (Just "testFunc1bis haddock")  (Just 11)           
                ,OutlineDef "testMethod" [Function] (InFileSpan (InFileLoc 18 1)(InFileLoc 21 13))  [] (Just "forall a . (Num a) => a -> a -> a")   (Just "testMethod\n haddock") (Just 16)     
                ,mkOutlineDefWithChildren "ToString" [Class] (InFileSpan (InFileLoc 23 1)(InFileLoc 24 28))  [
                        OutlineDef "toString" [Function] (InFileSpan (InFileLoc 24 5)(InFileLoc 24 28)) [] Nothing (Just "toString comment") Nothing
                        ]          
                ,OutlineDef "Str" [Type] (InFileSpan (InFileLoc 27 1)(InFileLoc 27 16)) [] (Just "String") (Just "Str haddock") (Just 26)             
                ,OutlineDef "Type1" [Data] (InFileSpan (InFileLoc 30 1)(InFileLoc 34 10))  [
                         OutlineDef "MkType1_1" [Constructor] (InFileSpan (InFileLoc 30 12)(InFileLoc 30 25)) [] Nothing (Just "MkType1 comment") Nothing
                        ,mkOutlineDefWithChildren "MkType1_2" [Constructor] (InFileSpan (InFileLoc 31 7)(InFileLoc 34 10)) [
                                mkOutlineDef "mkt2_s" [Field] (InFileSpan (InFileLoc 32 9)(InFileLoc 32 25)) 
                                ,mkOutlineDef "mkt2_i" [Field] (InFileSpan (InFileLoc 33 9)(InFileLoc 33 22)) 
                                
                                ]
                        
                        ] Nothing (Just "Type1 haddock")  (Just 29)       
                ]
        assertEqual (length expected) (length defs)
        mapM_ (uncurry assertEqual) (zip expected defs)
        assertEqual [] es
        assertEqual [ImportDef "Data.Char" Nothing (InFileSpan (InFileLoc 5 1)(InFileLoc 5 17)) False False "" Nothing] is
         
test_OutlineComments142 :: Assertion
test_OutlineComments142= do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"
        -- use api to write temp file
        write api root rel $ unlines [
                "module Module1 where",
                "",        
                "-- | Type for an entry in file",
                "data CblEntry = ",
                "  -- | This is the documentation for the 1. constructor",
                "  CblCondition { cblCondition :: String }",
                "  -- | This is the documentation for the FV",
                "  | CblFieldValue { ",
                "    cblFldName :: String, -- ^ name of the field",
                "    cblFldValues :: [String] -- ^ values of the field",
                "    } -- ^ field-values pair"                ]
        (_,nsErrors3f)<-getBuildFlags api root rel
        assertBool (null nsErrors3f)        
        (OutlineResult defs es is,nsErrors1)<-getOutline api root rel
        assertBool (null nsErrors1)
        assertEqual [] es
        assertEqual [] is
        let expected=[
                OutlineDef "CblEntry" [Data] (InFileSpan (InFileLoc 4 1)(InFileLoc 11 6)) [
                    OutlineDef "CblCondition" [Constructor] (InFileSpan (InFileLoc 6 3) (InFileLoc 6 42))
                        [
                            OutlineDef "cblCondition" [Field] (InFileSpan (InFileLoc 6 18) (InFileLoc 6 40)) [] Nothing Nothing Nothing
                        ] Nothing (Just "This is the documentation for the 1. constructor") (Just 5) 
                    , OutlineDef "CblFieldValue" [Constructor] (InFileSpan (InFileLoc 8 5) (InFileLoc 11 6))
                        [
                            OutlineDef "cblFldName" [Field] (InFileSpan (InFileLoc 9 5) (InFileLoc 9 25)) [] Nothing (Just "name of the field") Nothing
                            ,OutlineDef "cblFldValues" [Field] (InFileSpan (InFileLoc 10 5) (InFileLoc 10 29)) [] Nothing (Just "values of the field") Nothing
                        ] Nothing (Just "This is the documentation for the FV") (Just 7) 
                ] Nothing (Just "Type for an entry in file") (Just 3)]
        assertEqual (length expected) (length defs)
        mapM_ (uncurry assertEqual) (zip expected defs)
        
test_OutlineCommentsNextLine :: Assertion
test_OutlineCommentsNextLine= do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        let rel="src"</>"A.hs"
        -- use api to write temp file
        write api root rel $ unlines [
                "module Module1 where",
                "",        
                "-- | Type for an entry in file",
                "data CblEntry = ",
                "  -- | This is the documentation for the 1. constructor",
                "  CblCondition { cblCondition :: String }",
                "  -- | This is the documentation for the FV",
                "  | CblFieldValue { ",
                "    cblFldName :: String",
                "       -- ^ name of the field",
                "       --   continued on a second line",
                "    ,cblFldValues :: [String]",
                "       -- ^ values of the field",
                "    } -- ^ field-values pair"                ]
        (_,nsErrors3f)<-getBuildFlags api root rel
        assertBool (null nsErrors3f)        
        (OutlineResult defs es is,nsErrors1)<-getOutline api root rel
        assertBool (null nsErrors1)
        assertEqual [] es
        assertEqual [] is
        let expected=[
                OutlineDef "CblEntry" [Data] (InFileSpan (InFileLoc 4 1)(InFileLoc 14 6)) [
                    OutlineDef "CblCondition" [Constructor] (InFileSpan (InFileLoc 6 3) (InFileLoc 6 42))
                        [
                            OutlineDef "cblCondition" [Field] (InFileSpan (InFileLoc 6 18) (InFileLoc 6 40)) [] Nothing Nothing Nothing
                        ] Nothing (Just "This is the documentation for the 1. constructor") (Just 5) 
                    , OutlineDef "CblFieldValue" [Constructor] (InFileSpan (InFileLoc 8 5) (InFileLoc 14 6))
                        [
                            OutlineDef "cblFldName" [Field] (InFileSpan (InFileLoc 9 5) (InFileLoc 9 25)) [] Nothing (Just "name of the field\n   continued on a second line") Nothing
                            ,OutlineDef "cblFldValues" [Field] (InFileSpan (InFileLoc 12 6) (InFileLoc 12 30)) [] Nothing (Just "values of the field") Nothing
                        ] Nothing (Just "This is the documentation for the FV") (Just 7) 
                ] Nothing (Just "Type for an entry in file") (Just 3)]
        assertEqual (length expected) (length defs)
        mapM_ (uncurry assertEqual) (zip expected defs)
        -- use api to write temp file
        write api root rel $ unlines [
                "module Module1 where",
                "",        
                "f1",
                "  :: [a]",
                "  -> [a]",
                "     -- ^ The returned list",
                "f1 a=let",
                "  f2 = reverse a",
                "    in reverse f2"                ]
        (_,nsErrors4f)<-getBuildFlags api root rel
        assertBool (null nsErrors4f)        
        (OutlineResult defs2 es2 is2,nsErrors2)<-getOutline api root rel
        assertBool (null nsErrors2)
        assertEqual [] es2
        assertEqual [] is2
        let expected2=[
                OutlineDef "f1" [Function] (InFileSpan (InFileLoc 3 1)(InFileLoc 9 18)) [] 
                  (Just "[a] -> [a]") Nothing Nothing]
        assertEqual (length expected2) (length defs2)
        mapM_ (uncurry assertEqual) (zip expected2 defs2)
        
test_OutlinePreproc :: Assertion
test_OutlinePreproc =  do
        let api=cabalAPI
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
        assertBool (null nsErrors3f)         
        (OutlineResult defs1 _ _,nsErrors1)<-getOutline api root rel
        assertBool (null nsErrors1)
        let expected1=[
                mkOutlineDef "testfunc1" [Function] (InFileSpan (InFileLoc 6 1)(InFileLoc 6 25))
                ]
        assertEqual (length expected1) (length defs1)
        mapM_ (uncurry assertEqual) (zip expected1 defs1)
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
        assertBool (null nsErrors2)
        let expected2=[
                mkOutlineDefWithChildren "Name" [Data] (InFileSpan (InFileLoc 5 1)(InFileLoc 9 38))  [
                  OutlineDef "Ident" [Constructor] (InFileSpan (InFileLoc 6 6)(InFileLoc 6 18)) [] Nothing (Just "/varid/ or /conid/.") Nothing,
                  OutlineDef "Symbol" [Constructor] (InFileSpan (InFileLoc 7 6)(InFileLoc 7 19)) [] Nothing (Just "/varsym/ or /consym/") Nothing
                  ]
                ] 
        assertEqual (length expected2) (length defs2)
        mapM_ (uncurry assertEqual) (zip expected2 defs2)
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
        assertBool (null nsErrors3)
        let expected3=[
                mkOutlineDef "testfunc1" [Function] (InFileSpan (InFileLoc 6 1)(InFileLoc 6 25)) 
                ]
        assertEqual (length expected3) (length defs3)
        mapM_ (uncurry assertEqual) (zip expected3 defs3)
  
                       
       
test_OutlineLiterate :: Assertion
test_OutlineLiterate = do
        let api=cabalAPI
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
        assertBool  (null nsErrors3f)
        (OutlineResult defs1 _ _,nsErrors1)<-getOutline api root rel
        assertBool (null nsErrors1)
        let expected1=[
                mkOutlineDef "testfunc1" [Function] (InFileSpan (InFileLoc 6 3)(InFileLoc 6 27)) 
                ]
        assertEqual (length expected1) (length defs1)
        mapM_ (uncurry assertEqual) (zip expected1 defs1)
   
       
test_OutlineImportExport :: Assertion
test_OutlineImportExport = do
        let api=cabalAPI
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
        assertBool (null nsErrors3f)
        (OutlineResult _ es is,nsErrors1)<-getOutline api root rel
        assertBool  (null nsErrors1)
        let exps=[
                ExportDef "dummy" IEVar (InFileSpan (InFileLoc 1 17)(InFileLoc 1 22)) [],
                ExportDef "Data.Char" IEModule (InFileSpan (InFileLoc 1 23)(InFileLoc 1 39)) [],
                ExportDef "MkTest" IEThingAll (InFileSpan (InFileLoc 1 40)(InFileLoc 1 50)) []
                ]
        mapM_ (uncurry assertEqual) (zip exps es)
        let imps=[
                ImportDef "Data.Char" Nothing (InFileSpan (InFileLoc 3 1)(InFileLoc 3 17)) False False "" Nothing,
                ImportDef "Data.Map" Nothing (InFileSpan (InFileLoc 4 1)(InFileLoc 4 30)) False False "DM" (Just [ImportSpecDef "empty" IEVar (InFileSpan (InFileLoc 4 24)(InFileLoc 4 29)) []]),
                ImportDef "Data.List" Nothing (InFileSpan (InFileLoc 5 1)(InFileLoc 5 42)) False True "" (Just [ImportSpecDef "orderBy" IEVar (InFileSpan (InFileLoc 5 26)(InFileLoc 5 33)) [],ImportSpecDef "groupBy" IEVar (InFileSpan (InFileLoc 5 34)(InFileLoc 5 41)) []]),
                ImportDef "Data.Maybe" Nothing (InFileSpan (InFileLoc 6 1)(InFileLoc 6 42)) True False "" (Just [ImportSpecDef "Maybe" IEThingWith (InFileSpan (InFileLoc 6 30)(InFileLoc 6 41)) ["Just"]])
                ] 
        mapM_ (uncurry assertEqual) (zip imps is)
        
    
            
test_OutlineMultiParam  :: Assertion
test_OutlineMultiParam = do
        let api=cabalAPI
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
        assertBool  (null nsErrors3f)
        (OutlineResult ors _ _,nsErrors1)<-getOutline api root rel2
        assertBool (null nsErrors1)
        assertBool (not $ null ors)

    
test_OutlineOperator  :: Assertion
test_OutlineOperator = do
        let api=cabalAPI
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
        assertBool (null nsErrors3f)
        (OutlineResult ors _ _,nsErrors1)<-getOutline api root rel
        assertBool (null nsErrors1)
        assertBool (not $ null ors)

test_OutlinePatternGuards  :: Assertion
test_OutlinePatternGuards = do
        let api=cabalAPI
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
        assertBool  (null nsErrors3f)
        (bool3,nsErrors3)<-build1 api root rel
        assertBool (isJust bool3)
        assertBool (not (any (\ x -> BWError == bwnStatus x) nsErrors3))
        (OutlineResult ors _ _,nsErrors1)<-getOutline api root rel
        assertBool (null nsErrors1)
        assertBool (not $ null ors)

                
test_OutlineExtension     :: Assertion
test_OutlineExtension = do
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
        assertBool (null nsErrors3f)
        (bool3,nsErrors3)<-build1 api root rel
        assertBool (isJust bool3)
        assertBool (not (any (\ x -> BWError == bwnStatus x) nsErrors3))
        (OutlineResult ors _ _,nsErrors1)<-getOutline api root rel
        assertBool (null nsErrors1)
        assertBool (not $ null ors)
             
              
test_OutlineOptions :: Assertion
test_OutlineOptions = do
        let api=cabalAPI
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
        assertBool (null nsErrors3f)
        (bool3,nsErrors3)<-build1 api root rel
        assertBool (isJust bool3)
        assertBool (not (any (\ x -> BWError == bwnStatus x) nsErrors3))
        (OutlineResult ors _ _,nsErrors1)<-getOutline api root rel
        assertBool (null nsErrors1)
        assertBool (not $ null ors)
      
                
test_PreviewTokenTypes :: Assertion
test_PreviewTokenTypes = do
        let api=cabalAPI
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
        assertBool (null nsErrors1)
        let expectedS="[{\"P\":[1,1,37]},{\"C\":[2,1,13]},{\"K\":[3,1,7]},{\"IC\":[3,8,12]},{\"K\":[3,13,18]},{\"IV\":[5,1,5]},{\"S\":[5,6,8]},{\"IC\":[5,9,11]},{\"SS\":[5,12]},{\"IC\":[5,13,16]},{\"SS\":[5,16]},{\"IV\":[6,1,5]},{\"S\":[6,6]},{\"K\":[6,8,10]},{\"IV\":[7,9,15]},{\"SS\":[7,16]},{\"LC\":[7,17,20]},{\"S\":[7,20]},{\"LS\":[7,21,34]},{\"SS\":[7,34]},{\"IV\":[8,9,15]},{\"SS\":[8,16]},{\"LI\":[8,17]},{\"VS\":[8,19]},{\"LI\":[8,21]},{\"SS\":[8,22]},{\"PP\":[10,1,11]},{\"TH\":[11,1,3]},{\"IV\":[11,4,10]},{\"IV\":[11,11,23]},{\"TH\":[11,24,26]},{\"IC\":[11,26,35]},{\"SS\":[11,36]},{\"PP\":[12,1,7]}]"
        assertEqual expectedS (encode $ toJSON tts)

test_PreviewTokenTypesLine :: Assertion
test_PreviewTokenTypesLine = do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        configure api root Target        
        let rel="src"</>"Main.hs"
        write api root rel $ unlines [
                "{-# LINE 42 \"Foo.vhs\" #-}",
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
        assertBool (null nsErrors1)
        let expectedS="[{\"P\":[1,1,26]},{\"C\":[2,1,13]},{\"K\":[3,1,7]},{\"IC\":[3,8,12]},{\"K\":[3,13,18]},{\"IV\":[5,1,5]},{\"S\":[5,6,8]},{\"IC\":[5,9,11]},{\"SS\":[5,12]},{\"IC\":[5,13,16]},{\"SS\":[5,16]},{\"IV\":[6,1,5]},{\"S\":[6,6]},{\"K\":[6,8,10]},{\"IV\":[7,9,15]},{\"SS\":[7,16]},{\"LC\":[7,17,20]},{\"S\":[7,20]},{\"LS\":[7,21,34]},{\"SS\":[7,34]},{\"IV\":[8,9,15]},{\"SS\":[8,16]},{\"LI\":[8,17]},{\"VS\":[8,19]},{\"LI\":[8,21]},{\"SS\":[8,22]},{\"PP\":[10,1,11]},{\"TH\":[11,1,3]},{\"IV\":[11,4,10]},{\"IV\":[11,11,23]},{\"TH\":[11,24,26]},{\"IC\":[11,26,35]},{\"SS\":[11,36]},{\"PP\":[12,1,7]}]"
        assertEqual expectedS (encode $ toJSON tts)
        write api root rel $ unlines [
                "-- a comment",
                "module Main where", 
                "{-# LINE 2 \"d:\\\\toot\\\\titi\\\\Foo.vhs\" #-}",
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
        (tts2,nsErrors2)<-getTokenTypes api root rel
        assertBool (null nsErrors2)
        let expectedS2="[{\"C\":[1,1,13]},{\"K\":[2,1,7]},{\"IC\":[2,8,12]},{\"K\":[2,13,18]},{\"P\":[3,1,41]},{\"IV\":[5,1,5]},{\"S\":[5,6,8]},{\"IC\":[5,9,11]},{\"SS\":[5,12]},{\"IC\":[5,13,16]},{\"SS\":[5,16]},{\"IV\":[6,1,5]},{\"S\":[6,6]},{\"K\":[6,8,10]},{\"IV\":[7,9,15]},{\"SS\":[7,16]},{\"LC\":[7,17,20]},{\"S\":[7,20]},{\"LS\":[7,21,34]},{\"SS\":[7,34]},{\"IV\":[8,9,15]},{\"SS\":[8,16]},{\"LI\":[8,17]},{\"VS\":[8,19]},{\"LI\":[8,21]},{\"SS\":[8,22]},{\"PP\":[10,1,11]},{\"TH\":[11,1,3]},{\"IV\":[11,4,10]},{\"IV\":[11,11,23]},{\"TH\":[11,24,26]},{\"IC\":[11,26,35]},{\"SS\":[11,36]},{\"PP\":[12,1,7]}]"
        assertEqual expectedS2 (encode $ toJSON tts2)
        write api root rel $ unlines [
                "-- a comment",
                "module Main ({-# LINE 24 \"d:\\\\toot\\\\titi\\\\Foo.vhs\" #-})", 
                "where",
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
        (tts3,nsErrors3)<-getTokenTypes api root rel
        assertBool (null nsErrors3)
        let expectedS3="[{\"C\":[1,1,13]},{\"K\":[2,1,7]},{\"IC\":[2,8,12]},{\"SS\":[2,13]},{\"P\":[2,14,55]},{\"SS\":[2,55]},{\"K\":[3,1,6]},{\"IV\":[5,1,5]},{\"S\":[5,6,8]},{\"IC\":[5,9,11]},{\"SS\":[5,12]},{\"IC\":[5,13,16]},{\"SS\":[5,16]},{\"IV\":[6,1,5]},{\"S\":[6,6]},{\"K\":[6,8,10]},{\"IV\":[7,9,15]},{\"SS\":[7,16]},{\"LC\":[7,17,20]},{\"S\":[7,20]},{\"LS\":[7,21,34]},{\"SS\":[7,34]},{\"IV\":[8,9,15]},{\"SS\":[8,16]},{\"LI\":[8,17]},{\"VS\":[8,19]},{\"LI\":[8,21]},{\"SS\":[8,22]},{\"PP\":[10,1,11]},{\"TH\":[11,1,3]},{\"IV\":[11,4,10]},{\"IV\":[11,11,23]},{\"TH\":[11,24,26]},{\"IC\":[11,26,35]},{\"SS\":[11,36]},{\"PP\":[12,1,7]}]"
        assertEqual expectedS3 (encode $ toJSON tts3)
   
test_PreviewTokenTypesHaddock :: Assertion
test_PreviewTokenTypesHaddock = do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        configure api root Target        
        let rel="src"</>"Main.hs"
        write api root rel $ unlines [
                "{-# LINE 42 \"Foo.vhs\" #-}",
                "-- | a comment",
                "module Main where", 
                "",
                "main :: IO (Int)",
                "main = do" ,
                "        putStr ('h':\"ello Prefs!\")",
                "        return (2 + 2)",
                ""
                ]
        (tts,nsErrors1)<-getTokenTypes api root rel
        assertBool (null nsErrors1)
        let expectedS="[{\"P\":[1,1,26]},{\"D\":[2,1,15]},{\"K\":[3,1,7]},{\"IC\":[3,8,12]},{\"K\":[3,13,18]},{\"IV\":[5,1,5]},{\"S\":[5,6,8]},{\"IC\":[5,9,11]},{\"SS\":[5,12]},{\"IC\":[5,13,16]},{\"SS\":[5,16]},{\"IV\":[6,1,5]},{\"S\":[6,6]},{\"K\":[6,8,10]},{\"IV\":[7,9,15]},{\"SS\":[7,16]},{\"LC\":[7,17,20]},{\"S\":[7,20]},{\"LS\":[7,21,34]},{\"SS\":[7,34]},{\"IV\":[8,9,15]},{\"SS\":[8,16]},{\"LI\":[8,17]},{\"VS\":[8,19]},{\"LI\":[8,21]},{\"SS\":[8,22]}]"
        assertEqual expectedS (encode $ toJSON tts)   
        
test_ThingAtPoint :: Assertion
test_ThingAtPoint = do
        let api=cabalAPI
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
        assertBool (null nsErrors3f)          
        (tap1,nsErrors1)<-getThingAtPoint api root rel 2 16
        assertBool (null nsErrors1)
        assertBool (isJust tap1)
        assertEqual "map" (tapName $ fromJust tap1)
        assertEqual (Just "GHC.Base") (tapModule $ fromJust tap1)
        assertEqual (Just "(GHC.Types.Char -> GHC.Types.Char) -> [GHC.Types.Char] -> [GHC.Types.Char]") (tapQType $ fromJust tap1)
        assertEqual (Just "(Char -> Char) -> [Char] -> [Char]") (tapType $ fromJust tap1)
        assertEqual (Just "v") (tapHType $ fromJust tap1)
        assertEqual (Just "Var") (tapGType $ fromJust tap1)
        
        
        (tap2,nsErrors2)<-getThingAtPoint api root rel 2 20
        assertBool  (null nsErrors2)
        assertBool (isJust tap2)
        assertEqual "id" (tapName $ fromJust tap2)
        assertEqual (Just "GHC.Base") (tapModule $ fromJust tap2)
        assertEqual (Just "v") (tapHType $ fromJust tap2)
        assertEqual (Just "GHC.Types.Char -> GHC.Types.Char") (tapQType $ fromJust tap2)
       
        (tap3,nsErrors3)<-getThingAtPoint api root rel 4 7
        assertBool (null nsErrors3)
        assertBool (isJust tap3)
        assertEqual "DataT" (tapName $ fromJust tap3)
        assertEqual (Just "Main") (tapModule $ fromJust tap3)
        assertEqual (Just "t") (tapHType $ fromJust tap3)
        assertEqual Nothing (tapQType $ fromJust tap3)
        
#if __GLASGOW_HASKELL__ != 704
        -- type information for constructors at the declaration is not supported by ghc 7.4       
        (tap4,nsErrors4)<-getThingAtPoint api root rel 4 14
        assertBool (null nsErrors4)
        assertBool  (isJust tap4)
        assertEqual "MkData" (tapName $ fromJust tap4)
        assertEqual (Just "Main") (tapModule $ fromJust tap4)
        assertEqual (Just "v") (tapHType $ fromJust tap4)
        assertEqual (Just "DataCon") (tapGType $ fromJust tap4)
        assertEqual (Just "String -> DataT") (tapType $ fromJust tap4)
        assertEqual (Just "GHC.Base.String -> Main.DataT") (tapQType $ fromJust tap4)
#endif

        (tap5,nsErrors5)<-getThingAtPoint api root rel 4 22
        assertBool (null nsErrors5)
        assertBool (isJust tap5)
        assertEqual  "name" (tapName $ fromJust tap5)
        assertEqual  (Just "Main") (tapModule $ fromJust tap5)
        assertEqual   (Just "v") (tapHType $ fromJust tap5)
        assertEqual  (Just "Main.DataT -> GHC.Base.String") (tapQType $ fromJust tap5)
        
        (tap6,nsErrors6)<-getThingAtPoint api root rel 6 7
        assertBool (null nsErrors6)
        assertBool  (isJust tap6)
        assertEqual "Toot" (tapName $ fromJust tap6)
        assertEqual  (Just "Main") (tapModule $ fromJust tap6)
        assertEqual   (Just "t") (tapHType $ fromJust tap6)
        assertEqual  Nothing (tapQType $ fromJust tap6)

#if __GLASGOW_HASKELL__ != 704
        -- type information for constructors at the declaration is not supported by ghc 7.4       
        (tap7,nsErrors7)<-getThingAtPoint api root rel 6 14
        assertBool  (null nsErrors7)
        assertBool  (isJust tap7)
        assertEqual "Toot" (tapName $ fromJust tap7)
        assertEqual (Just "Main") (tapModule $ fromJust tap7)
        assertEqual (Just "v") (tapHType $ fromJust tap7)
        assertEqual (Just "GHC.Base.String -> Main.Toot") (tapQType $ fromJust tap7)
        
        -- type information for field names at the declaration is not supported by ghc 7.4       
        (tap8,nsErrors8)<-getThingAtPoint api root rel 6 19
        assertBool (null nsErrors8)
        assertBool (isJust tap8)
        assertEqual  "toot" (tapName $ fromJust tap8)
        assertEqual  (Just "Main") (tapModule $ fromJust tap8)
        assertEqual  (Just "v") (tapHType $ fromJust tap8)
        assertEqual  (Just "Main.Toot -> GHC.Base.String") (tapQType $ fromJust tap8)
#endif        
                
        (tap9,nsErrors9)<-getThingAtPoint api root rel 9 5
        assertBool (null nsErrors9)
        assertBool (isJust tap9)
        assertEqual "l2" (tapName $ fromJust tap9)
        assertEqual (Just "") (tapModule $ fromJust tap9)
        assertEqual (Just "v") (tapHType $ fromJust tap9)
        assertEqual (Just "[GHC.Types.Char]") (tapQType $ fromJust tap9)


test_ThingAtPointTypeReduction :: Assertion
test_ThingAtPointTypeReduction = do
        let api=cabalAPI
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
        assertBool (null nsErrorsMf)          
        (tapM,nsErrorsM)<-getThingAtPoint api root rel 6 13
        assertBool (null nsErrorsM)
        assertBool (isJust tapM)
        assertEqual "insert" (tapName $ fromJust tapM)
#if __GLASGOW_HASKELL__ >= 706
        assertEqual (Just "Data.Map.Base") (tapModule $ fromJust tapM)
        assertEqual (Just "v") (tapHType $ fromJust tapM)
        assertEqual (Just "GHC.Base.String -> GHC.Types.Int -> Data.Map.Base.Map GHC.Base.String GHC.Types.Int -> Data.Map.Base.Map GHC.Base.String GHC.Types.Int") (tapQType $ fromJust tapM)
#else
        assertEqual (Just "Data.Map") (tapModule $ fromJust tapM)
        assertEqual (Just "v") (tapHType $ fromJust tapM)
        assertEqual (Just "GHC.Base.String -> GHC.Types.Int -> Data.Map.Map GHC.Base.String GHC.Types.Int -> Data.Map.Map GHC.Base.String GHC.Types.Int") (tapQType $ fromJust tapM)
#endif


test_ThingAtPointNotInCabal :: Assertion
test_ThingAtPointNotInCabal = do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        configure api root Target        
        let rel2="src"</>"Auto.hs"
        writeFile (root </> rel2) $ unlines ["module Auto where","fAuto=head [2,3,4]"] 
        synchronize api root False
        (_,nsErrors3f)<-getBuildFlags api root rel2
        assertBool (null nsErrors3f)
        (tap1,nsErrors1)<-getThingAtPoint api root rel2 2 8
        assertBool (null nsErrors1)
        assertBool  (isJust tap1)
        assertEqual "head" (tapName $ fromJust tap1)
        assertEqual (Just "GHC.List") (tapModule $ fromJust tap1)
        assertEqual (Just "[GHC.Integer.Type.Integer] -> GHC.Integer.Type.Integer") (tapQType $ fromJust tap1)


test_ThingAtPointMain :: Assertion
test_ThingAtPointMain = do
        let api=cabalAPI
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
        assertBool (null nsErrors3f)
        assertEqual (Just "Main") (bfModName bf3)
        (tap1,nsErrors1)<-getThingAtPoint api root rel 3 16
        assertBool (null nsErrors1)
        assertBool (isJust tap1)
        assertEqual "head" (tapName $ fromJust tap1)
        assertEqual (Just "GHC.List") (tapModule $ fromJust tap1)
        assertEqual (Just "[GHC.Integer.Type.Integer] -> GHC.Integer.Type.Integer") (tapQType $ fromJust tap1)
       
--        (tap2,nsErrors2)<-getThingAtPoint api root rel 2 8
--        assertBool ("errors or warnings on getThingAtPoint2:"++show nsErrors2) (null nsErrors2)
--        assertBool "not just tap2" (isJust tap2)
--        assertEqual "not B.D" "B.D" (tapName $ fromJust tap2)
--        assertEqual "not ModuleName" (Just "ModuleName") (tapGType $ fromJust tap2)
--        assertEqual "not m"  (Just "m") (tapHType $ fromJust tap2)
--       

test_ThingAtPointUnit :: Assertion
test_ThingAtPointUnit = do
        let api=cabalAPI
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
                  "import Control.Monad (when)",
                  "main=when True $ print \"5\" >> return ()"
                  ]
        synchronize api root False
        configure api root Target     
        (bf3,nsErrors3f)<-getBuildFlags api root rel
        assertBool (null nsErrors3f)
        assertEqual (Just "Main") (bfModName bf3)
        (tap1,nsErrors1)<-getThingAtPoint api root rel 3 35
        assertBool (null nsErrors1)
        assertBool (isJust tap1)
        assertEqual "return" (tapName $ fromJust tap1)
        --assertEqual (Just "forall a. a -> IO ()") (tapType $ fromJust tap1)
        assertEqual (Just "() -> IO ()") (tapType $ fromJust tap1)
        (tap2,nsErrors2)<-getThingAtPoint api root rel 3 30
        assertBool (null nsErrors2)
        assertBool (isJust tap2)
        assertEqual ">>" (tapName $ fromJust tap2)
        --assertEqual (Just "forall a b. IO () -> IO () -> IO ()") (tapType $ fromJust tap2)
        assertEqual (Just "IO () -> IO () -> IO ()") (tapType $ fromJust tap2)
        (tap3,nsErrors3)<-getThingAtPoint api root rel 3 7
        assertBool (null nsErrors3)
        assertBool (isJust tap3)
        assertEqual "when" (tapName $ fromJust tap3)
        assertEqual (Just "Bool -> IO () -> IO ()") (tapType $ fromJust tap3)
        
test_ThingAtPointMainSubFolder :: Assertion
test_ThingAtPointMainSubFolder = do
        let api=cabalAPI
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
        assertBool (null nsErrors3f)
        assertEqual (Just "Main") (bfModName bf3)
        (tap1,nsErrors1)<-getThingAtPoint api root rel 3 16
        assertBool (null nsErrors1)
        assertBool (isJust tap1)
        assertEqual "head" (tapName $ fromJust tap1)
        assertEqual (Just "GHC.List") (tapModule $ fromJust tap1)
        assertEqual (Just "[GHC.Integer.Type.Integer] -> GHC.Integer.Type.Integer") (tapQType $ fromJust tap1)
      
test_Locals :: Assertion
test_Locals = do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        configure api root Target        
        let rel="src"</>"Main.hs"
        write api root rel $ unlines [  
                  "module Main where",
                  "main=return $ map id \"toto\"",
                  "",
                  "fun1 l1=let",
                  "    l2=reverse \"toto\"",
                  "    in head l2"
                  ] 
        (_,nsErrors)<-getBuildFlags api root rel
        assertBool (null nsErrors)      
        (loc1,nsErrors1)<-getLocals api root rel 4 1 6 15
        assertBool (null nsErrors1)
        assertBool (not $ null loc1)
        let names=map tapName loc1
        assertBool ("l2" `elem` names)
        assertBool ("l1" `elem` names)
        write api root rel $ unlines [  
                  "module Main where",
                  "main=return $ map id \"toto\"",
                  "",
                  "fun1 l1=do",
                  "    let l2=reverse \"toto\"",
                  "    reverse head l2"
                  ] 
        (_,nsErrorsM)<-getBuildFlags api root rel
        assertBool (null nsErrorsM)      
        (loc2,nsErrors2)<-getLocals api root rel 4 1 6 15
        assertBool (null nsErrors2)
        assertBool (not $ null loc2)
        let namesM=map tapName loc2
        assertBool ("l2" `elem` namesM)
        assertBool ("l1" `elem` namesM)
        return ()

test_Eval :: Assertion
test_Eval = do
        let api=cabalAPI
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
        (_,nsErrors)<-getBuildFlags api root rel
        assertBool (null nsErrors)   

        (s1,_)<-eval api root rel "reverse \"toto\"" 
        assertEqual [EvalResult (Just "[GHC.Types.Char]") (Just "\"otot\"") Nothing] s1
        (s2,_)<-eval api root  rel "main" 
        assertEqual [EvalResult (Just "[GHC.Types.Char]") (Just "\"toto\"") Nothing] s2     
        (s3,_)<-eval api root rel "MkType1_1"
        assertBool $ isPrefixOf "No instance for" $ (\(EvalResult _ _ (Just err))->err) $ head s3     
        return ()

test_NamesInScope :: Assertion
test_NamesInScope = do
        let api=cabalAPI
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
        assertBool (null nsErrors1)
        assertBool (isJust mtts)
        let tts=fromJust mtts
        assertBool ("Main.main" `elem` tts)
        assertBool ("B.D.fD" `elem` tts)
        assertBool ("GHC.Types.Char" `elem` tts)

     
test_NameDefsInScope :: Assertion
test_NameDefsInScope = do
        let api=cabalAPI
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
        assertBool (isJust mtts)
        let tts=fromJust mtts
#if __GLASGOW_HASKELL__ >=708
        let functype="t"
#else
        let functype="forall a. a"
#endif            
        assertBool (NameDef "Main.main" [Function] (Just "IO [Char]") `elem` tts)
        assertBool (NameDef "B.D.fD" [Function] (Just functype) `elem` tts)
        assertBool (NameDef "Main.Type1" [Type] Nothing `elem` tts)
        assertBool (NameDef "Main.MkType1_1" [Constructor] (Just "Int -> Type1") `elem` tts)
        assertBool (NameDef "GHC.Types.Char" [Type] Nothing `elem` tts)


                
test_InPlaceReference  :: Assertion
test_InPlaceReference = do
        let api=cabalAPI
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
        assertBool boolOKc
        assertBool (null nsOKc)
        (BuildResult boolOK _,nsOK)<-build api root True Source
        assertBool boolOK
        assertBool (null nsOK)
        synchronize api root True
        (mtts,nsErrors1)<-getNamesInScope api root rel
        assertBool (null nsErrors1)
        assertBool (isJust mtts)
        let tts=fromJust mtts
        assertBool ("Main.main" `elem` tts)
        assertBool ("B.C.fC" `elem` tts)
        (_,nsErrors3f)<-getBuildFlags api root rel
        assertBool (null nsErrors3f)
        (tap1,nsErrorsTap1)<-getThingAtPoint api root rel 3 16
        assertBool (null nsErrorsTap1)
        assertBool (isJust tap1)
        assertEqual "map" (tapName $ fromJust tap1)
        assertEqual (Just "GHC.Base") (tapModule $ fromJust tap1)
        assertEqual (Just "(GHC.Types.Char -> GHC.Types.Char) -> [GHC.Types.Char] -> [GHC.Types.Char]") (tapQType $ fromJust tap1)
        (mtts2,nsErrors2)<-getNamesInScope api root rel2
        assertBool (null nsErrors2)
        assertBool (isJust mtts2)
        let tts2=fromJust mtts2
        assertBool ("A.fA" `elem` tts2)     
        assertBool ("B.C.fC" `elem` tts2) 
        
        (mtts3,nsErrors3)<-getNamesInScope api root rel3
        assertBool (null nsErrors3)
        assertBool (isJust mtts3)
        let tts3=fromJust mtts3
        assertBool ("A.fA" `elem` tts3)       


test_CabalComponents  :: Assertion
test_CabalComponents= do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        (cps,nsOK)<-getCabalComponents api root
        assertBool (null nsOK)
        assertEqual 3 (length cps)
        let (l:ex:ts:[])=cps
        assertEqual (CCLibrary True) l
        assertEqual (CCExecutable "BWTest" True) ex
        assertEqual (CCTestSuite "BWTest-test" True) ts
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
        assertBool (null nsOK2)
        assertEqual 3 (length cps2)
        let (l2:ex2:ts2:[])=cps2
        assertEqual (CCLibrary False) l2
        assertEqual (CCExecutable "BWTest" False) ex2
        assertEqual (CCTestSuite "BWTest-test" False) ts2          


test_CabalBenchmark  :: Assertion
test_CabalBenchmark= do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
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
                "",
                "benchmark BWTest-bench",
                "  type:            exitcode-stdio-1.0",
                "  hs-source-dirs:  test",
                "  main-is:         Main.hs",
                "  other-modules:  TestA",
                "  build-depends:  base",
                ""
                ]
        configure api root Source
        (cps2,nsOK2)<-getCabalComponents api root
        assertBool (null nsOK2)
        assertEqual 4 (length cps2)
        let (l2:ex2:ts2:b2:[])=cps2
        assertEqual (CCLibrary True) l2
        assertEqual (CCExecutable "BWTest" True) ex2
        assertEqual (CCTestSuite "BWTest-test" True) ts2   
        assertEqual (CCBenchmark "BWTest-bench" True) b2   
        
test_CabalDependencies  :: Assertion
test_CabalDependencies = do
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
                        "  build-depends:  base,filepath",
                        ""]
        synchronize api root False
        (cps,nsOK)<-getCabalDependencies api root Nothing
        assertBool (null nsOK)
        assertEqual 2 (length cps)
        let [(_,pkgs1),(_,pkgs2)] = cps -- One is global and one is local, but the order depends on the paths, 
            pkgs = pkgs1 ++ pkgs2       -- so we concatenate the two.
        let base=filter (\pkg->cpName pkg == "base") pkgs
        assertEqual 1 (length base)
        let (l:ex:ts:[])=cpDependent $ head base
        assertEqual (CCLibrary True) l
        assertEqual (CCExecutable "BWTest" True) ex
        assertEqual (CCTestSuite "BWTest-test" True) ts
        let fp=filter (\pkg->cpName pkg == "filepath") pkgs
        assertEqual 1 (length fp)
        let (lfp:[])=cpDependent $ head fp
        assertEqual (CCTestSuite "BWTest-test" True) lfp


test_NoSourceDir :: Assertion
test_NoSourceDir = do
        let api=cabalAPI
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
        assertBool (not d1)
        
        let bwInBw=root </> ".dist-buildwrapper" </> ".dist-buildwrapper"
        d1b<-doesDirectoryExist bwInBw
        assertBool (not d1b)
        
        synchronize api root False
        d2<-doesDirectoryExist gitInBW
        assertBool (not d2)
        
        d2b<-doesDirectoryExist bwInBw
        assertBool (not d2b)
        


test_Flags  :: Assertion
test_Flags = do
        let api=cabalAPI
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
        assertBool (not ex1)
        
        configureWithFlags api root Source "server"
        build api root True Source
        ex2<-doesFileExist exePath
        assertBool ex2
        
   

test_BuildFlags :: Assertion
test_BuildFlags =do
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
                "  build-depends:  base",
                "  extensions: OverlappingInstances"
                ]
        configure api root Source 
        let rel="src"</>"A.hs"  
        (flgs,nsErrors3f)<-getBuildFlags api root rel
        -- print flgs
        assertBool (null nsErrors3f)
        let ast=bfAst flgs   
        assertBool ("-package-name" `elem` ast)
        assertBool ("BWTest-0.1" `elem` ast)
        assertBool ("-XOverlappingInstances" `elem` ast)
        assertBool ("OverlappingInstances" `notElem` ast)
       
        
test_ExplicitComponent :: Assertion
test_ExplicitComponent =do
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
                "  other-modules:  B",
                "  build-depends:  base,containers",
                "executable BWTest",
                "  hs-source-dirs:  src",
                "  main-is:         Main.hs",
                "  other-modules:  B",
                "  build-depends:  base,containers",
                "",
                "executable BWTest2",
                "  hs-source-dirs:  src",
                "  main-is:         Main2.hs",
                "  other-modules:  B",
                "  build-depends:  base"
                ]
        let rel="src"</>"B.hs"  
        writeFile (root </> rel) $ unlines [
                "module B where",
                "import qualified Data.Map as M",
                "ins=M.insert 'k' 0 M.empty"
                ]
        configure api root Source 
        
        synchronize1 api root True rel
        --build1 api root rel
        (names1,nsErrors1)<-build1c api root rel ""
        assertBool (isJust names1)
        assertBool (null nsErrors1)
        (names2,nsErrors2)<-build1c api root rel "BWTest"
        assertBool (isJust names2)
        assertBool (null nsErrors2)
        (names3,nsErrors3)<-build1c api root rel "BWTest2"
        assertBool (isNothing names3)
        assertBool (not $ null nsErrors3)
   
   
test_ExplicitComponentUnRef :: Assertion
test_ExplicitComponentUnRef =do
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
                "  build-depends:  base,containers",
                "executable BWTest",
                "  hs-source-dirs:  src",
                "  main-is:         Main.hs",
                "  build-depends:  base,containers",
                "",
                "executable BWTest2",
                "  hs-source-dirs:  src",
                "  main-is:         Main2.hs",
                "  build-depends:  base"
                ]
        let rel="src"</>"B.hs"  
        writeFile (root </> rel) $ unlines [
                "module B where",
                "import qualified Data.Map as M",
                "ins=M.insert 'k' 0 M.empty"
                ]
        configure api root Source 
        
        synchronize1 api root True rel
        --build1 api root rel
        (names1,nsErrors1)<-build1c api root rel ""
        assertBool (isJust names1)
        assertBool (null nsErrors1)
        (names2,nsErrors2)<-build1c api root rel "BWTest"
        assertBool (isJust names2)
        assertBool (null nsErrors2)
        (names3,nsErrors3)<-build1c api root rel "BWTest2"
        assertBool (isNothing names3)
        assertBool (not $ null nsErrors3)
 
test_CleanFiles :: Assertion
test_CleanFiles =do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        let fldr=root </> ".dist-buildwrapper"
        let file=fldr </> "src" </> ".A.hs.bwinfo"
        exf1<-doesFileExist file
        assertBool $ not exf1
        exd1<-doesDirectoryExist fldr
        assertBool exd1
        let rel="src"</> "A.hs"
        build1 api root rel
        getThingAtPoint api root rel 1 1
        exf2<-doesFileExist file
        assertBool exf2
        clean api root False
        exf3<-doesFileExist file
        assertBool $ not exf3
        exd2<-doesDirectoryExist fldr
        assertBool exd2
        build1 api root rel
        getThingAtPoint api root rel 1 1
        exf4<-doesFileExist file
        assertBool exf4
        clean api root True
        exf5<-doesFileExist file
        assertBool $ not exf5
        exd3<-doesDirectoryExist fldr
        assertBool $ not exd3
        
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
        "  ghc-options: -dynamic",
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
testCabalFile root =root </> (last (splitDirectories root) <.> ".cabal") 
     
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
removeSpaces = filter (not . flip elem [':','\'','`','\CAN','\EM']) . filter (not . isSpace)

assertEqualNotesStart :: BWNote -> BWNote -> IO()
assertEqualNotesStart n1 n2=do
        let n2'=n1{bwnTitle=Data.List.take (length $ bwnTitle n1) $ bwnTitle n2}
        assertEqual n1 n2'

assertEqualNotesWithoutSpaces :: String -> BWNote -> BWNote -> IO()
assertEqualNotesWithoutSpaces msg n1 n2=do
        let
                n1'=n1{bwnTitle=removeSpaces $ bwnTitle n1}
                n2'=n1{bwnTitle=removeSpaces $ bwnTitle n2}
        assertEqualVerbose msg n1' n2'

removeLayoutTAP :: OpResult (Maybe ThingAtPoint) -> OpResult (Maybe ThingAtPoint) 
removeLayoutTAP res = case res of
                        (Just tap@ThingAtPoint{tapType=tp,tapQType=qtp},xs) ->
                          (Just tap{tapType=removeLayout tp, tapQType=removeLayout qtp},xs)
                        _ -> res
 where removeLayout (Just tp) = Just $ unwords . concatMap words . lines $ tp -- replace sequences of spaces and newlines by single space
       removeLayout Nothing   = Nothing

  
runAPI:: (FromJSON a,Show a) => FilePath ->  FilePath -> String -> [String] -> IO a
runAPI= runAPI' True

runAPI':: (FromJSON a,Show a) => Bool -> FilePath ->  FilePath -> String -> [String] -> IO a
runAPI' withCabal cabal root command args= do
        cd<-getCurrentDirectory
        let fullargs=[command,"--tempfolder=.dist-buildwrapper","--cabalpath=" ++ cabal] ++ (if withCabal then ["--cabalfile="++ testCabalFile root] else []) ++ args
        exePath<-filterM doesFileExist [".dist-buildwrapper/dist/build/buildwrapper/buildwrapper" <.> exeExtension,"dist/build/buildwrapper/buildwrapper" <.> exeExtension]
        assertBool (0<length exePath)
        setCurrentDirectory root
        (ex,out,err)<-readProcessWithExitCode (cd </> head exePath) fullargs ""
        setCurrentDirectory cd
        putStrLn ("out:"++out)
        putStrLn ("err:"++err)
        assertEqual ExitSuccess ex
        let res=map (drop $ length ("build-wrapper-json:"::String)) $ filter (isPrefixOf "build-wrapper-json:") $ lines out
        assertEqual 1 (length res)
        let r=parse value $ BS.pack (head res)
        case r of
                Done _ js->do 
                        let r1= fromJSON js
                        case r1 of 
                                Data.Aeson.Success fin->return fin
                                a->do
                                        assertFailure (show a) 
                                        error ""
                a-> assertFailure (show a)  
                        
startAPIProcess :: FilePath -> String -> [String] -> IO (Handle, Handle, Handle, ProcessHandle)
startAPIProcess  root command args= do
        cd<-getCurrentDirectory
        let fullargs=[command,"--tempfolder=.dist-buildwrapper","--cabalpath=cabal","--cabalfile="++ testCabalFile root] ++ args
        exePath<-filterM doesFileExist [".dist-buildwrapper/dist/build/buildwrapper/buildwrapper" <.> exeExtension,"dist/build/buildwrapper/buildwrapper" <.> exeExtension]
        assertBool (0<length exePath)
        runInteractiveProcess (cd </> head exePath) fullargs (Just root) Nothing      
               
readResult :: (FromJSON a,Show a) => Handle -> IO a    
readResult h= do
        l<-BS.hGetLine h
        BS.putStrLn l
        if "build-wrapper-json:" `BS.isPrefixOf` l
           then do
              let r=parse value $ BS.drop (BS.length "build-wrapper-json:") l
              -- print r
              case r of
                Done _ js->do 
                        let r1= fromJSON js
                        case r1 of 
                                Data.Aeson.Success fin->return fin
                                a-> assertFailure (show a) 
                a-> assertFailure (show a) 
           else
                readResult h    
         
continue :: Handle -> IO ()
continue h=do
        hPutStrLn h "."
        hFlush h

evalLR :: Handle -> String -> IO ()
evalLR h expr=do
        hPutStrLn h ("e "++expr)
        hFlush h

end :: Handle -> IO ()
end h =do
        hPutStrLn h "q"  
        hFlush h    
  
tokenTypesLR :: Handle -> IO ()
tokenTypesLR h =do
        hPutStrLn h "t"  
        hFlush h     
  
tapLR :: Handle -> Int -> Int -> IO()
tapLR h l c=do
        hPutStrLn h ('p':show (l,c))  
        hFlush h    
  
localsLR::  Handle -> Int -> Int -> Int -> Int -> IO()
localsLR h l c el ec=do
        hPutStrLn h ('l':show (l,c,el,ec))  
        hFlush h      
  
cmdOpts :: [String] -> [String] 
cmdOpts =map ("--cabaloption=" ++)   
   
notesInError :: [BWNote] -> Bool
notesInError = any (\ x -> BWError == bwnStatus x)

changeLR :: Handle -> String -> String -> IO()
changeLR h fp m=do
        hPutStrLn h ('c':show (fp,m))  
        hFlush h   
