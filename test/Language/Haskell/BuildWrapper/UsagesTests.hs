{-# LANGUAGE CPP,OverloadedStrings,PatternGuards #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- |
-- Module      : Language.Haskell.BuildWrapper.UsagesTests
-- Copyright   : (c) JP Moresmau 2013
-- License     : BSD3
-- 
-- Maintainer  : jpmoresmau@gmail.com
-- Stability   : beta
-- Portability : portable
-- 
-- Test usage generation
module Language.Haskell.BuildWrapper.UsagesTests where

import Language.Haskell.BuildWrapper.Base hiding (readFile,writeFile)

import Language.Haskell.BuildWrapper.CMDTests
import Test.Framework hiding (Success)
import Test.HUnit (Assertion)

import System.Directory
import System.FilePath

--import System.Time

import qualified Data.ByteString.Lazy as BS
-- import qualified Data.ByteString.Lazy.Char8 as BSC (putStrLn)
import qualified Data.ByteString as BSS
import Data.Aeson
import Data.Maybe

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import qualified Data.Aeson.Types (parse)


test_GenerateBWUsage :: Assertion
test_GenerateBWUsage = do
        let api=cabalAPI
        root<-createTestProject
        ((fps,dels),_)<-synchronize api root False
        assertBool (not $ null fps)
        assertBool (null dels)  
        assertEqual (testProjectName <.> ".cabal") (head fps)
        let rel="src" </> "A.hs"
        assertBoolVerbose "no A" (rel `elem` fps)
        let bwI1=getUsageFile (root </> ".dist-buildwrapper" </>  rel)
        ef1<-doesFileExist bwI1
        assertBoolVerbose (bwI1 ++ "  file exists before build") (not ef1)
        (BuildResult bool1 fps1,nsErrors1)<-build api root False Source
        assertBool bool1
        assertBool (null nsErrors1)
        assertBool (rel `elem` fps1)
        assertBoolVerbose (bwI1 ++ "  file exists after build") (not ef1)
        (comps,_)<-getCabalComponents api root
        --c1<-getClockTime
        gar<-mapM (generateUsage api root False) comps
        let fs=concat $ mapMaybe fst gar
        assertBool (rel `elem` fs)
        --c2<-getClockTime
        -- putStrLn ("generateUsage: " ++ timeDiffToString (diffClockTimes c2 c1))
        ef2<-doesFileExist bwI1
        assertBoolVerbose (bwI1 ++ " file doesn't exist after generateAST") ef2
        gar2<-mapM (generateUsage api root  False) comps
        let fs2=concat $ mapMaybe fst gar2
        assertBool (rel `notElem` fs2)
        gar3<-mapM (generateUsage api root  True) comps
        let fs3=concat $ mapMaybe fst gar3
        assertBool (rel `elem` fs3)


test_GenerateReferencesSimple :: Assertion
test_GenerateReferencesSimple = do
        let api=cabalAPI
        root<-createTestProject
        let relMain="src"</>"Main.hs"
        writeFile (root</> relMain) $ unlines [  
                  "module Main where",
                  "import A",
                  "main=print $ reset $ Cons2 1"
                  ] 
        let rel="src" </> "A.hs"
        writeFile (root</> rel) $ unlines [  
                  "module A where",
                  "data MyData=Cons1", 
                  "      { mdS::MyString}", 
                  "      | Cons2 Int",
                  "     deriving Show",
                  "",
                  "type MyString=String",
                  "",
                  "reset :: MyData -> MyData",
                  "reset (Cons1 _)=Cons1 \"\"",
                  "reset (Cons2 _)=Cons2 0",
                  "",
                  "resetAll=map reset",
                  "",
                  "getString :: MyData -> Maybe MyString",
                  "getString (Cons1 s)=Just s",
                  "getString _= Nothing"
                  ]  
        _<-synchronize api root True          
        (BuildResult bool1 _,nsErrors1)<-build api root False Source
        assertBoolVerbose ("returned false on bool1:" ++ show nsErrors1)  bool1
        assertBool (null nsErrors1)
        (lib:exe:_,_)<-getCabalComponents api root    
        --mapM_ (generateUsage api root False) comps
        generateUsage api root False lib
        let uf=getUsageFile $ root </> ".dist-buildwrapper" </> relMain
        euf1<-doesFileExist uf
        assertBool (not euf1)
        generateUsage api root False exe
        euf2<-doesFileExist uf
        assertBool euf2
        --sI<-fmap formatJSON (readFile  $ getInfoFile(root </> ".dist-buildwrapper" </>  rel))
        --putStrLn sI
        v<-readStoredUsage (root </> ".dist-buildwrapper" </>  rel)
        -- sU<-fmap formatJSON (readFile  $ getUsageFile(root </> ".dist-buildwrapper" </>  rel))
        -- putStrLn sU
      
        assertPackageModule "BWTest-0.1" "A" [1,8,1,9] v
      
        assertVarUsage "BWTest-0.1" "A" "Cons1" [("MyData",True,[2,13,2,18]),("reset",False,[10,8,10,13]),("reset",False,[10,17,10,22]),("getString",False,[16,12,16,17])] v
        assertVarUsage "BWTest-0.1" "A" "Cons2" [("MyData",True,[4,9,4,14]),("reset",False,[11,8,11,13]),("reset",False,[11,17,11,22])] v
        assertVarUsage "BWTest-0.1" "A" "mdS" [("MyData",True,[3,9,3,12])] v
        assertVarUsage "BWTest-0.1" "A" "reset" [("reset",True,[9,1,9,6]),("reset",True,[10,1,10,25]),("reset",True,[11,1,11,24]),("resetAll",False,[13,14,13,19])] v
        assertVarUsage "BWTest-0.1" "A" "resetAll" [("resetAll",True,[13,1,13,19])] v
        assertVarUsage "BWTest-0.1" "A" "getString" [("getString",True,[15,1,15,10]),("getString",True,[16,1,16,27]),("getString",True,[17,1,17,21])] v
        assertVarUsage "base" "Data.Maybe" "Nothing" [("getString",False,[17,14,17,21])] v
        assertVarUsage "base" "Data.Maybe" "Just" [("getString",False,[16,21,16,25])] v
        assertVarUsage "base" "GHC.Base" "map" [("resetAll",False,[13,10,13,13])] v
        assertVarUsage "base" "GHC.Num" "fromInteger" [("reset",False,[11,23,11,24])] v
        
        assertTypeUsage "BWTest-0.1" "A" "MyData" [("MyData",True,[2,6,2,12]),("reset",False,[9,10,9,16]),("reset",False,[9,20,9,26]),("getString",False,[15,14,15,20])] v
        assertTypeUsage "BWTest-0.1" "A" "MyString" [("MyData",False,[3,14,3,22]),("MyString",True,[7,6,7,14]),("getString",False,[15,30,15,38])] v
        assertTypeUsage "base" "Data.Maybe" "Maybe" [("getString",False,[15,24,15,29])] v
        assertTypeUsage "base" "GHC.Base" "String" [("MyString",False,[7,15,7,21])] v
        assertTypeUsage "base" "GHC.Show" "Show" [("MyData",False,[5,15,5,19])] v
        assertTypeUsage "ghc-prim" "GHC.Types" "Int" [("MyData",False,[4,15,4,18])] v
        
        vMain<-readStoredUsage (root </> ".dist-buildwrapper" </>  relMain)
        --sUMain<-fmap formatJSON (readFile  $ getUsageFile(root </> ".dist-buildwrapper" </>  relMain))
        --putStrLn sUMain
        assertPackageModule "BWTest-0.1" "Main" [1,8,1,12] vMain
        
        assertVarUsage "BWTest-0.1" "A" "" [("import",False,[2,8,2,9])] vMain
        assertVarUsage "BWTest-0.1" "A" "Cons2" [("main",False,[3,22,3,27])] vMain
        assertVarUsage "BWTest-0.1" "A" "reset" [("main",False,[3,14,3,19])] vMain
        assertVarUsage "BWTest-0.1" "Main" "main" [("main",True,[3,1,3,29])] vMain
        assertVarUsage "base" "System.IO" "print" [("main",False,[3,6,3,11])] vMain
        assertVarUsage "base" "GHC.Base" "$" [("main",False,[3,12,3,13]),("main",False,[3,20,3,21])] vMain
        return ()


test_GenerateReferencesImports :: Assertion
test_GenerateReferencesImports = do
        let api=cabalAPI
        root<-createTestProject
        let relMain="src"</>"Main.hs"
        writeFile (root</> relMain) $ unlines [
                  "module Main where",
                  "import Data.Ord",
                  "import Data.Maybe (Maybe(..))",
                  "import Data.Complex (Complex((:+)))",
                  "",
                  "main=undefined"
                  ] 
        _<-synchronize api root True          
        (BuildResult bool1 _,nsErrors1)<-build api root False Source
        assertBoolVerbose ("returned false on bool1:" ++ show nsErrors1)  bool1
        assertBool (null nsErrors1)
        (comps,_)<-getCabalComponents api root    
        mapM_ (generateUsage api root False) comps
        vMain<-readStoredUsage (root </> ".dist-buildwrapper" </>  relMain)
        -- sUMain<-fmap formatJSON (readFile  $ getUsageFile(root </> ".dist-buildwrapper" </>  relMain))
        -- putStrLn sUMain
        assertVarUsage "base" "Data.Ord" "" [("import",False,[2,8,2,16])] vMain
        assertVarUsage "base" "Data.Maybe" "" [("import",False,[3,8,3,18])] vMain
        assertVarUsage "base" "Data.Complex" "" [("import",False,[4,8,4,20])] vMain
        assertTypeUsage "base" "Data.Maybe" "Maybe" [("import",False,[3,20,3,29])] vMain
        assertTypeUsage "base" "Data.Complex" "Complex" [("import",False,[4,22,4,35])] vMain
        assertVarUsage "base" "Data.Complex" ":+" [("import",False,[4,22,4,35])] vMain


test_GenerateReferencesExports :: Assertion
test_GenerateReferencesExports =  do
        let api=cabalAPI
        root<-createTestProject
        let rel="src" </> "A.hs"
        writeFile (root</> rel) $ unlines [  
                  "module A (",
                  "    MyData,",
                  "    MyData2(..),",
                  "    MyData3(Cons31),",
                  "    reset,",
                  "    MyString,",
                  "    module Data.Ord) where",
                  "import Data.Ord",
                  "data MyData=Cons1", 
                  "      { mdS::MyString}", 
                  "      | Cons2 Int",
                  "     deriving Show",
                  "",
                  "type MyString=String",
                  "",
                  "reset :: MyData -> MyData",
                  "reset (Cons1 _)=Cons1 \"\"",
                  "reset (Cons2 _)=Cons2 0",
                  "",
                  "data MyData2=Cons21", 
                  "      { mdS2::MyString}", 
                  "      | Cons22 Int",
                  "     deriving Show",
                  "data MyData3=Cons31", 
                  "      { mdS3::MyString}", 
                  "      | Cons32 Int",
                  "     deriving Show"
                  ]  
        _<-synchronize api root True          
        (BuildResult bool1 _,nsErrors1)<-build api root False Source
        assertBoolVerbose ("returned false on bool1:" ++ show nsErrors1)  bool1
        assertBool (null nsErrors1)
        (comps,_)<-getCabalComponents api root    
        mapM_ (generateUsage api root False) comps
        v<-readStoredUsage (root </> ".dist-buildwrapper" </>  rel)
        --sU<-fmap formatJSON (readFile  $ getUsageFile(root </> ".dist-buildwrapper" </>  rel))
        --putStrLn sU
        
        assertVarUsage "BWTest-0.1" "A" "Cons1" [("MyData",True,[9,13,9,18]),("reset",False,[17,8,17,13]),("reset",False,[17,17,17,22])] v
        assertVarUsage "BWTest-0.1" "A" "Cons2" [("MyData",True,[11,9,11,14]),("reset",False,[18,8,18,13]),("reset",False,[18,17,18,22])] v
        assertVarUsage "BWTest-0.1" "A" "Cons21" [("MyData2",True,[20,14,20,20])] v
        assertVarUsage "BWTest-0.1" "A" "Cons22" [("MyData2",True,[22,9,22,15])] v
        assertVarUsage "BWTest-0.1" "A" "Cons31" [("export",False,[4,5,4,20]),("MyData3",True,[24,14,24,20])] v
        assertVarUsage "BWTest-0.1" "A" "Cons32" [("MyData3",True,[26,9,26,15])] v
        assertVarUsage "BWTest-0.1" "A" "mdS" [("MyData",True,[10,9,10,12])] v
        assertVarUsage "BWTest-0.1" "A" "mdS2" [("MyData2",True,[21,9,21,13])] v
        assertVarUsage "BWTest-0.1" "A" "mdS3" [("MyData3",True,[25,9,25,13])] v
        assertVarUsage "BWTest-0.1" "A" "reset" [("export",False,[5,5,5,10]),("reset",True,[16,1,16,6]),("reset",True,[17,1,17,25]),("reset",True,[18,1,18,24])] v
        assertVarUsage "base" "GHC.Num" "fromInteger" [("reset",False,[18,23,18,24])] v
        
        assertVarUsage "base" "Data.Ord" "" [("export",False,[7,5,7,20]),("import",False,[8,8,8,16])] v
        
        assertTypeUsage "BWTest-0.1" "A" "MyData" [("export",False,[2,5,2,11]),("MyData",True,[9,6,9,12]),("reset",False,[16,10,16,16]),("reset",False,[16,20,16,26])] v
        assertTypeUsage "BWTest-0.1" "A" "MyString" [("export",False,[6,5,6,13]),("MyData",False,[10,14,10,22]),("MyString",True,[14,6,14,14]),("MyData2",False,[21,15,21,23]),("MyData3",False,[25,15,25,23])] v
        assertTypeUsage "base" "GHC.Base" "String" [("MyString",False,[14,15,14,21])] v
        assertTypeUsage "base" "GHC.Show" "Show" [("MyData",False,[12,15,12,19]),("MyData2",False,[23,15,23,19]),("MyData3",False,[27,15,27,19])] v
        assertTypeUsage "BWTest-0.1" "A" "MyData2" [("export",False,[3,5,3,16]),("MyData2",True,[20,6,20,13])] v
        assertTypeUsage "BWTest-0.1" "A" "MyData3" [("export",False,[4,5,4,20]),("MyData3",True,[24,6,24,13])] v
        assertTypeUsage "ghc-prim" "GHC.Types" "Int" [("MyData",False,[11,15,11,18]),("MyData2",False,[22,16,22,19]),("MyData3",False,[26,16,26,19])] v

 
test_GenerateReferencesExportAlias :: Assertion
test_GenerateReferencesExportAlias =  do
        let api=cabalAPI
        root<-createTestProject
        let relMain="src"</>"Main.hs"
        writeFile (root</> relMain) $ unlines [
                  "module Main (module X,main) where",
                  "import Data.Ord as X",
                  "",
                  "main=undefined"
                  ] 
        _<-synchronize api root True          
        (BuildResult bool1 _,nsErrors1)<-build api root False Source
        assertBoolVerbose ("returned false on bool1:" ++ show nsErrors1)  bool1
        assertBool (null nsErrors1)
        (comps,_)<-getCabalComponents api root    
        mapM_ (generateUsage api root False) comps
        vMain<-readStoredUsage (root </> ".dist-buildwrapper" </>  relMain)
        -- sUMain<-fmap formatJSON (readFile  $ getUsageFile(root </> ".dist-buildwrapper" </>  relMain))
        -- putStrLn sUMain
        assertVarUsage "base" "Data.Ord" "" [("export",False,[1,14,1,22]),("import",False,[2,8,2,16])] vMain

test_GenerateReferencesPattern :: Assertion
test_GenerateReferencesPattern =  do
        let api=cabalAPI
        root<-createTestProject
        let relMain="src"</>"Main.hs"
        writeFile (root</> relMain) $ unlines [
                  "module Main where",
                  "",
                  "main=undefined",
                  "f :: Maybe String -> String",
                  "f mf=case mf of",
                  "  Just x->x",
                  "  _->\"\"" 
                  ] 
        _<-synchronize api root True          
        (BuildResult bool1 _,nsErrors1)<-build api root False Source
        assertBoolVerbose ("returned false on bool1:" ++ show nsErrors1)  bool1
        assertBool (null nsErrors1)
        (comps,_)<-getCabalComponents api root    
        mapM_ (generateUsage api root False) comps
        vMain<-readStoredUsage (root </> ".dist-buildwrapper" </>  relMain)
        -- sUMain<-fmap formatJSON (readFile  $ getUsageFile(root </> ".dist-buildwrapper" </>  relMain))
        -- putStrLn sUMain
        assertVarUsage "base" "Data.Maybe" "Just" [("f",False,[6,3,6,7])] vMain
       
    
test_IncorrectModuleFileName :: Assertion
test_IncorrectModuleFileName = do
        let api=cabalAPI
        root<-createTestProject
        let relMain="src"</>"Main.hs"
        let relMain2="src"</>"Main-exe.hs"
        renameFile (root </> relMain) (root </> relMain2)
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
                "  main-is:         Main-exe.hs",
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
        synchronize api root False  
        configure api root Target  
        (comps,_)<-getCabalComponents api root    
        gar<-mapM (generateUsage api root False) comps       
        let fs=concat $ mapMaybe fst gar
        assertBool (relMain2 `elem` fs)

getUsageFile :: FilePath -- ^ the source file
        -> FilePath
getUsageFile fp= let 
        (dir,file)=splitFileName fp
        in combine dir ('.' : addExtension file ".bwusage")      
        
readStoredUsage :: FilePath  -- ^ the source file
        -> IO Value
readStoredUsage =readJSONFile . getUsageFile

  
readJSONFile :: FilePath -> IO Value
readJSONFile f= do
       ex<-doesFileExist f
       mv<-if ex
                then do
                       bs<-BSS.readFile f
                       return $ decode' $ BS.fromChunks [bs]
                else return Nothing
       return $ fromMaybe (object []) mv
       
getInfoFile :: FilePath -- ^ the source file
        -> FilePath
getInfoFile fp= let 
        (dir,file)=splitFileName fp
        in combine dir ('.' : addExtension file ".bwinfo")      
        
-- | read the top JSON value containing all the information
readStoredInfo :: FilePath  -- ^ the source file
        -> IO Value
readStoredInfo =readJSONFile . getInfoFile 

extractNameValue :: Value -> T.Text
extractNameValue (Object m) |Just (String s)<-HM.lookup "Name" m=s
extractNameValue _ = error "no name in value"

assertVarUsage :: T.Text -> T.Text -> T.Text -> [(T.Text,Bool,[Int])] -> Value -> IO() 
assertVarUsage = assertUsage "vars"

assertTypeUsage :: T.Text -> T.Text -> T.Text -> [(T.Text,Bool,[Int])] -> Value -> IO() 
assertTypeUsage = assertUsage "types"


assertUsage :: T.Text -> T.Text -> T.Text -> T.Text -> [(T.Text,Bool,[Int])] -> Value -> IO()
assertUsage tp pkg modu name lins (Array v) |
        V.length v==5,
        (Object m) <-v V.! 3,
        Just (Object m2)<-HM.lookup pkg m,
        Just (Object m3)<-HM.lookup modu m2,
        Just (Object m4)<-HM.lookup tp m3,
        Just (Array arr)<-HM.lookup name m4=   do
                let expected=S.fromList $ map (\(section,def,[sl,sc,el,ec])->(section,def,InFileSpan (InFileLoc sl sc) (InFileLoc el ec))) lins
                let actual=S.fromList $ map (\v2->
                        let (Success r)=Data.Aeson.Types.parse (\(Object o) ->do 
                                --Success (Object o)<-fromJSON v 
                                (String section)<-o .: "s"
                                (Bool def)<-o .: "d"
                                arr2<-o .: "l"
                                let (Success ifl)=fromJSON arr2
                                return (section,def,ifl::InFileSpan)) v2
                        in r) $ V.toList arr
                assertEqualVerbose (T.unpack modu ++ "." ++ T.unpack name ++ ": " ++ show lins) expected actual  
        --V.elem (Number (I line)) arr=return ()
assertUsage _ _ modu name line _=assertFailure (T.unpack modu ++ "." ++ T.unpack name ++ ": " ++ show line)

assertPackageModule :: T.Text -> T.Text -> [Int] -> Value -> IO()
assertPackageModule pkg modu [sl,sc,el,ec] (Array v) |
         V.length v==5,
        (String s0) <-v V.! 0,
        (String s1) <-v V.! 1,
        arr<- v V.! 2= do
                assertEqualVerbose (T.unpack pkg) pkg s0
                assertEqualVerbose (T.unpack modu) modu s1  
                let (Success ifs)=fromJSON arr
                assertEqualVerbose (show ifs) (InFileSpan (InFileLoc sl sc) (InFileLoc el ec)) ifs  
assertPackageModule pkg modu _ _=  assertFailure (T.unpack pkg ++ "." ++ T.unpack modu)
