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
bfdbtests= [ testBuildAllCreatesBWInfo,
        testGenerateReferences]

testBuildAllCreatesBWInfo :: (APIFacade a)=> a -> Test
testBuildAllCreatesBWInfo api= TestLabel "testBuildAllCreatesBWInfo" (TestCase ( do
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
        putStrLn ("generateAST: " ++ (timeDiffToString $ diffClockTimes c2 c1))
        ef2<-doesFileExist bwI1
        assertBool (bwI1 ++ " file doesn't exist after generateAST") ef2
        ))

testGenerateReferences :: (APIFacade a)=> a -> Test
testGenerateReferences api= TestLabel "testGenerateReferences" (TestCase ( do
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
        assertBool ("returned false on bool1:"++(show nsErrors1))  bool1
        assertBool "no errors or warnings on nsErrors1" (null nsErrors1)
        (comps,_)<-getCabalComponents api root    
        mapM_ (generateAST api root) comps
        --sI<-fmap formatJSON (readFile  $ getInfoFile(root </> ".dist-buildwrapper" </>  rel))
        --putStrLn sI
        v<-readStoredUsage (root </> ".dist-buildwrapper" </>  rel)
        --sU<-fmap formatJSON (readFile  $ getUsageFile(root </> ".dist-buildwrapper" </>  rel))
        --putStrLn sU
      
        assertVarUsage "BWTest-0.1" "A" "Cons1" [2,10,16] v
        assertVarUsage "BWTest-0.1" "A" "Cons2" [4,11] v
        assertVarUsage "BWTest-0.1" "A" "mdS" [3] v
        assertVarUsage "BWTest-0.1" "A" "reset" [9,10,11,13] v
        assertVarUsage "BWTest-0.1" "A" "resetAll" [13] v
        assertVarUsage "BWTest-0.1" "A" "getString" [15,16,17] v
        assertVarUsage "base" "Data.Maybe" "Nothing" [17] v
        assertVarUsage "base" "Data.Maybe" "Just" [16] v
        assertVarUsage "base" "GHC.Base" "map" [13] v
        assertVarUsage "base" "GHC.Num" "fromInteger" [11] v
        
        assertTypeUsage "BWTest-0.1" "A" "MyData" [2,9,15] v
        assertTypeUsage "BWTest-0.1" "A" "MyString" [3,7,15] v
        assertTypeUsage "base" "Data.Maybe" "Maybe" [15] v
        assertTypeUsage "base" "GHC.Base" "String" [7] v
        assertTypeUsage "base" "GHC.Show" "Show" [5] v
        return ()
        ))


        
getUsageFile :: FilePath -- ^ the source file
        -> FilePath
getUsageFile fp= let 
        (dir,file)=splitFileName fp
        in combine dir ('.' : addExtension file ".bwusage")      
        
readStoredUsage :: FilePath  -- ^ the source file
        -> IO Value
readStoredUsage fp=do
       let ghcInfoFile=getUsageFile fp
       ex<-doesFileExist ghcInfoFile
       mv<-if ex
                then do
                       bs<-BSS.readFile ghcInfoFile
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
readStoredInfo fp=do
       let ghcInfoFile=getInfoFile fp
       ex<-doesFileExist ghcInfoFile
       mv<-if ex
                then do
                       bs<-BSS.readFile ghcInfoFile
                       return $ decode' $ BS.fromChunks [bs]
                else return Nothing
       return $ fromMaybe (object []) mv       
       

extractNameValue :: Value -> T.Text
extractNameValue (Object m) |Just (String s)<-HM.lookup "Name" m=s
extractNameValue _ = error "no name in value"

assertVarUsage :: T.Text -> T.Text -> T.Text -> [Integer] -> Value -> IO() 
assertVarUsage = assertUsage "vars"

assertTypeUsage :: T.Text -> T.Text -> T.Text -> [Integer] -> Value -> IO() 
assertTypeUsage = assertUsage "types"


assertUsage :: T.Text -> T.Text -> T.Text -> T.Text -> [Integer] -> Value -> IO()
assertUsage tp pkg modu name lines (Object m) |
        Just (Object m2)<-HM.lookup pkg m,
        Just (Object m3)<-HM.lookup modu m2,
        Just (Object m4)<-HM.lookup tp m3,
        Just (Array arr)<-HM.lookup name m4=assertEqual (T.unpack modu ++ "." ++ T.unpack name ++ ": " ++ (show lines))  (S.fromList lines) (S.fromList $ map (\(Number (I i))->i) $ V.toList arr)
        --V.elem (Number (I line)) arr=return ()
assertUsage _ _ modu name line _=assertBool (T.unpack modu ++ "." ++ T.unpack name ++ ": " ++ (show line)) False

