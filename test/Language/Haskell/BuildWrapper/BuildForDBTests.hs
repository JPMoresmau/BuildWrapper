{-# LANGUAGE CPP,OverloadedStrings,PatternGuards #-}
module Language.Haskell.BuildWrapper.BuildForDBTests where

import Language.Haskell.BuildWrapper.Base

import Language.Haskell.BuildWrapper.Tests
import Language.Haskell.BuildWrapper.CMDTests
import Test.HUnit

import System.Directory
import System.FilePath

import System.Time

import qualified Data.ByteString.Lazy as BS
-- import qualified Data.ByteString.Lazy.Char8 as BSC (putStrLn)
import qualified Data.ByteString as BSS
import Data.Aeson
import Data.Maybe

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import Data.Attoparsec.Number (Number(I))

buildForDBTests::[Test]
buildForDBTests= map (\f->f CMDAPI) bfdbtests

bfdbtests :: (APIFacade a)=> [a -> Test]
bfdbtests= [ testGenerateASTCreatesBWUsage,
        testGenerateReferencesSimple]

testGenerateASTCreatesBWUsage :: (APIFacade a)=> a -> Test
testGenerateASTCreatesBWUsage api= TestLabel "testGenerateASTCreatesBWUsage" (TestCase ( do
        root<-createTestProject
        (fps,_)<-synchronize api root False
        assertBool "no file path on creation" (not $ null fps) 
        assertEqual "no cabal file" (testProjectName <.> ".cabal") (head fps)
        let rel="src" </> "A.hs"
        assertBool "no A" (rel `elem` fps)
        let bwI1=getUsageFile (root </> ".dist-buildwrapper" </>  rel)
        ef1<-doesFileExist bwI1
        assertBool (bwI1 ++ "  file exists before build") (not ef1)
        (BuildResult bool1 fps1,nsErrors1)<-build api root False Source
        assertBool "returned false on bool1" bool1
        assertBool "no errors or warnings on nsErrors1" (null nsErrors1)
        assertBool ("no rel in fps1: " ++ show fps1) (rel `elem` fps1)
        assertBool (bwI1 ++ "  file exists after build") (not ef1)
        (comps,_)<-getCabalComponents api root
        c1<-getClockTime
        mapM_ (generateAST api root) comps
        c2<-getClockTime
        putStrLn ("generateAST: " ++ timeDiffToString (diffClockTimes c2 c1))
        ef2<-doesFileExist bwI1
        assertBool (bwI1 ++ " file doesn't exist after generateAST") ef2
        ))

testGenerateReferencesSimple :: (APIFacade a)=> a -> Test
testGenerateReferencesSimple api= TestLabel "testGenerateReferencesSimple" (TestCase ( do
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
        assertBool ("returned false on bool1:" ++ show nsErrors1)  bool1
        assertBool "no errors or warnings on nsErrors1" (null nsErrors1)
        (comps,_)<-getCabalComponents api root    
        mapM_ (generateAST api root) comps
        --sI<-fmap formatJSON (readFile  $ getInfoFile(root </> ".dist-buildwrapper" </>  rel))
        --putStrLn sI
        v<-readStoredUsage (root </> ".dist-buildwrapper" </>  rel)
        --sU<-fmap formatJSON (readFile  $ getUsageFile(root </> ".dist-buildwrapper" </>  rel))
        --putStrLn sU
      
        assertVarUsage "BWTest-0.1" "A" "Cons1" [[2,13,2,18],[10,8,10,13],[10,17,10,22],[16,12,16,17]] v
        assertVarUsage "BWTest-0.1" "A" "Cons2" [[4,9,4,14],[11,8,11,13],[11,17,11,22]] v
        assertVarUsage "BWTest-0.1" "A" "mdS" [[3,9,3,12]] v
        assertVarUsage "BWTest-0.1" "A" "reset" [[9,1,9,6],[10,1,10,25],[11,1,11,24],[13,14,13,19]] v
        assertVarUsage "BWTest-0.1" "A" "resetAll" [[13,1,13,19]] v
        assertVarUsage "BWTest-0.1" "A" "getString" [[15,1,15,10],[16,1,16,27],[17,1,17,21]] v
        assertVarUsage "base" "Data.Maybe" "Nothing" [[17,14,17,21]] v
        assertVarUsage "base" "Data.Maybe" "Just" [[16,21,16,25]] v
        assertVarUsage "base" "GHC.Base" "map" [[13,10,13,13]] v
        assertVarUsage "base" "GHC.Num" "fromInteger" [[11,23,11,24]] v
        
        assertTypeUsage "BWTest-0.1" "A" "MyData" [[2,6,2,12],[9,10,9,16],[9,20,9,26],[15,14,15,20]] v
        assertTypeUsage "BWTest-0.1" "A" "MyString" [[3,14,3,22],[7,6,7,14],[15,30,15,38]] v
        assertTypeUsage "base" "Data.Maybe" "Maybe" [[15,24,15,29]] v
        assertTypeUsage "base" "GHC.Base" "String" [[7,15,7,21]] v
        assertTypeUsage "base" "GHC.Show" "Show" [[5,15,5,19]] v
        
        vMain<-readStoredUsage (root </> ".dist-buildwrapper" </>  relMain)
        --sUMain<-fmap formatJSON (readFile  $ getUsageFile(root </> ".dist-buildwrapper" </>  relMain))
        --putStrLn sUMain
        
        assertVarUsage "BWTest-0.1" "A" "" [[2,1,2,9]] vMain
        assertVarUsage "BWTest-0.1" "A" "Cons2" [[3,22,3,27]] vMain
        assertVarUsage "BWTest-0.1" "A" "reset" [[3,14,3,19]] vMain
        assertVarUsage "BWTest-0.1" "Main" "main" [[3,1,3,29]] vMain
        assertVarUsage "base" "System.IO" "print" [[3,6,3,11]] vMain
        assertVarUsage "base" "GHC.Base" "$" [[3,12,3,13],[3,20,3,21]] vMain
        return ()
        ))


        
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

assertVarUsage :: T.Text -> T.Text -> T.Text -> [[Int]] -> Value -> IO() 
assertVarUsage = assertUsage "vars"

assertTypeUsage :: T.Text -> T.Text -> T.Text -> [[Int]] -> Value -> IO() 
assertTypeUsage = assertUsage "types"


assertUsage :: T.Text -> T.Text -> T.Text -> T.Text -> [[Int]] -> Value -> IO()
assertUsage tp pkg modu name lins (Object m) |
        Just (Object m2)<-HM.lookup pkg m,
        Just (Object m3)<-HM.lookup modu m2,
        Just (Object m4)<-HM.lookup tp m3,
        Just (Array arr)<-HM.lookup name m4=   do
                let expected=S.fromList $ map (\[sl,sc,el,ec]->InFileSpan (InFileLoc sl sc) (InFileLoc el ec)) lins
                let actual=S.fromList $ map (\v->let (Success ifl)=fromJSON v in ifl) $ V.toList arr
                assertEqual (T.unpack modu ++ "." ++ T.unpack name ++ ": " ++ show lins) expected actual  
        --V.elem (Number (I line)) arr=return ()
assertUsage _ _ modu name line _=assertBool (T.unpack modu ++ "." ++ T.unpack name ++ ": " ++ show line) False

