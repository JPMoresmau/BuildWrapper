{-# LANGUAGE CPP, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- |
-- Module      : Language.Haskell.BuildWrapper.CMDTests
-- Copyright   : (c) JP Moresmau 2013
-- License     : BSD3
-- 
-- Maintainer  : jpmoresmau@gmail.com
-- Stability   : beta
-- Portability : portable
-- 
-- Testing for long running operations
module Language.Haskell.BuildWrapper.CMDLongRunningTests where

import Language.Haskell.BuildWrapper.CMDTests 

import Language.Haskell.BuildWrapper.Base hiding (writeFile,readFile)

import Data.ByteString.Lazy ()
import Data.ByteString.Lazy.Char8()

import Data.Maybe
import Data.List

import System.FilePath


import Test.Framework
import Test.HUnit (Assertion)
import Control.Concurrent (threadDelay)

test_NameDefsInScopeLongRunning :: Assertion
test_NameDefsInScopeLongRunning = do
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
        (inp,out,_,_)<-build1lr root rel
        (mtts,ns)<- readResult out :: IO (OpResult (Maybe [NameDef]))
        assertBool (isJust mtts)
        assertBool (not $ notesInError ns)
        let tts=fromJust mtts
#if __GLASGOW_HASKELL__ >=708
        let functype="t"
#else
        let functype="forall a. a"
#endif                
        print $ NameDef "B.D.fD" [Function] (Just functype)
        assertBool (NameDef "Main.main" [Function] (Just "IO [Char]") `elem` tts)
        assertBool (NameDef "B.D.fD" [Function] (Just functype) `elem` tts)
        assertBool (NameDef "Main.Type1" [Type] Nothing `elem` tts)
        assertBool (NameDef "Main.MkType1_1" [Constructor] (Just "Int -> Type1") `elem` tts)
        assertBool (NameDef "GHC.Types.Char" [Type] Nothing `elem` tts)          
        assertBool (NameDef "Main.Type2" [Type] Nothing `notElem` tts)
        threadDelay 1000000
        write api root rel $ unlines [  
                  "module Main where",
                  "import B.D",
                  "main=return $ map id \"toto\"",
                  "data Type1=MkType1_1 Int",
                  "data Type2=MkType2_1 Int"
                  ]
        continue inp
        (mtts2,ns2)<-readResult out :: IO (OpResult (Maybe [NameDef]))  
        assertBool (not $ notesInError ns2)
        assertBool (isJust mtts2)
        let tts2=fromJust mtts2
        assertBool (NameDef "Main.Type1" [Type] Nothing `elem` tts2)
        assertBool (NameDef "Main.Type2" [Type] Nothing `elem` tts2)
        write api root rel $ unlines [  
                  "module Main where",
                  "import B.D",
                  "main=return $ map id \"toto\"",
                  "data Type1=MkType1_1 Int",
                  "data Type2=MkType2_1 Int2"
                  ]
        continue inp
        (_,ns3)<-readResult out :: IO (OpResult (Maybe [NameDef]))  
        assertBool (notesInError ns3)
        threadDelay 1000000
        write api root rel $ unlines [  
                  "module Main where",
                  "import B.D",
                  "main=return $ map id \"toto\"",
                  "data Type1=MkType1_1 Int",
                  "data Type2=MkType2_1 Int"
                  ]
        continue inp
        (mtts4,ns4)<-readResult out :: IO (OpResult (Maybe [NameDef]))  
        assertBool (not $ notesInError ns4)
        assertBool (isJust mtts4)
        let tts4=fromJust mtts4
        assertBool (NameDef "Main.Type1" [Type] Nothing `elem` tts4)
        assertBool (NameDef "Main.Type2" [Type] Nothing `elem` tts4)
        end inp
        
test_EvalLongRunning :: Assertion
test_EvalLongRunning = do
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
        (inp,out,_,_)<-build1lr root rel
        (mtts,ns)<- readResult out :: IO (OpResult (Maybe [NameDef]))
        assertBool (isJust mtts)
        assertBool (not $ notesInError ns) 
        evalLR inp "reverse \"toto\"" 
        (s1,_)<- readResult out :: IO (OpResult [EvalResult])
        assertEqual [EvalResult (Just "[GHC.Types.Char]") (Just "\"otot\"") Nothing] s1
        evalLR inp "main" 
        (s2,_)<- readResult out :: IO (OpResult [EvalResult])
        assertEqual [EvalResult (Just "[GHC.Types.Char]") (Just "\"toto\"") Nothing] s2     
        evalLR inp "MkType1_1"
        (s3,_)<- readResult out :: IO (OpResult [EvalResult])
        assertBool $ isPrefixOf "No instance for" $ (\(EvalResult _ _ (Just err))->err) $ head s3     
        threadDelay 1000000
        write api root rel $ unlines [  
                  "module Main where",
                  "import B.D",
                  "main=return $ map id \"titi\"",
                  "data Type1=MkType1_1 Int",
                  "data Type2=MkType2_1 Int"
                  ] 
        continue inp
        readResult out :: IO (OpResult (Maybe [NameDef]))  
        evalLR inp "main" 
        (s4,_)<- readResult out :: IO (OpResult [EvalResult])
        assertEqual [EvalResult (Just "[GHC.Types.Char]") (Just "\"titi\"") Nothing] s4
        end inp     
  
test_EvalTextLongRunning :: Assertion
test_EvalTextLongRunning = do
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
          "  build-depends:  base, text",
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
        configure api root Source        
        let rel="src"</>"Main.hs"
        writeFile (root </> rel) $ unlines [  
                  "{-# LANGUAGE OverloadedStrings #-}",
                  "module Main where",
                  "import qualified Data.Text as T",
                  "t=T.pack \"test\""
                  ] 
        build api root True Source
        synchronize api root False
        (inp,out,_,_)<-build1lr root rel
        (mtts,ns)<- readResult out :: IO (OpResult (Maybe [NameDef]))
        assertBool (isJust mtts)
        assertBool (not $ notesInError ns) 
        evalLR inp "t" 
        (s1,_)<- readResult out :: IO (OpResult [EvalResult])
        assertEqual [EvalResult (Just "Data.Text.Internal.Text") (Just "\"test\"") Nothing] s1
        evalLR inp "T.breakOnEnd \"/\" \"a/b\"" 
        (s2,_)<- readResult out :: IO (OpResult [EvalResult])
        assertEqual [EvalResult (Just "(Data.Text.Internal.Text, Data.Text.Internal.Text)") (Just "(\"a/\",\"b\")") Nothing] s2
        
        end inp    
      
test_TokenTypesLongRunning :: Assertion
test_TokenTypesLongRunning = do
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
        (inp,out,_,_)<-build1lr root rel
        (mtts,ns)<- readResult out :: IO (OpResult (Maybe [NameDef]))
        assertBool (isJust mtts)
        assertBool (not $ notesInError ns) 
        tokenTypesLR inp
        (tts,nsErrors1)<-readResult out ::IO (OpResult [TokenDef])
        assertBool (null nsErrors1)
        assertBool (not $ null tts)
        continue inp
        (mtts2,ns2)<-readResult out :: IO (OpResult (Maybe [NameDef]))  
        assertBool (not $ notesInError ns2)
        assertBool (isJust mtts2)
        end inp
     
test_ThingAtPointLongRunning :: Assertion
test_ThingAtPointLongRunning = do
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
        (inp,out,_,_)<-build1lr root rel
        (mtts,ns)<- readResult out :: IO (OpResult (Maybe [NameDef]))
        assertBool (isJust mtts)
        assertBool (not $ notesInError ns) 
        tokenTypesLR inp
        (tts,nsErrors1)<-readResult out ::IO (OpResult [TokenDef])
        assertBool (null nsErrors1)
        assertBool (not $ null tts)
        tapLR inp 3 16
        (tap1,nsErrorsTap)<-readResult out :: IO (OpResult (Maybe ThingAtPoint))
        assertBool (null nsErrorsTap)
        assertBool $ isJust tap1
        assertEqual "map" (tapName $ fromJust tap1)
        continue inp
        (mtts2,ns2)<-readResult out :: IO (OpResult (Maybe [NameDef]))  
        assertBool (not $ notesInError ns2)
        assertBool (isJust mtts2)
        end inp    
        
test_LocalsLongRunning :: Assertion
test_LocalsLongRunning = do
        let api=cabalAPI
        root<-createTestProject
        synchronize api root False
        configure api root Source        
        let rel="src"</>"Main.hs"
        writeFile (root </> rel) $ unlines [  
                  "module Main where",
                  "main=return $ map id \"toto\"",
                  "",
                  "fun1 l1=let",
                  "    l2=reverse \"toto\"",
                  "    in head l2"
                  ] 
        build api root True Source
        synchronize api root False
        (inp,out,_,_)<-build1lr root rel
        (mtts,ns)<- readResult out :: IO (OpResult (Maybe [NameDef]))
        assertBool (isJust mtts)
        assertBool (not $ notesInError ns) 
        localsLR inp 4 1 6 10
        (tap1,nsErrorsTap)<-readResult out :: IO (OpResult [ThingAtPoint])
        assertBool (null nsErrorsTap)
        let namesM=map tapName tap1
        assertBool ("l2" `elem` namesM)
        assertBool ("l1" `elem` namesM)
        continue inp
        (mtts2,ns2)<-readResult out :: IO (OpResult (Maybe [NameDef]))  
        assertBool (not $ notesInError ns2)
        assertBool (isJust mtts2)
        end inp   
        
test_ChangeLongRunning :: Assertion
test_ChangeLongRunning = do
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
        let rel2="src" </> "B" </> "D.hs"
        writeFile (root </> rel2) $ unlines ["module B.D where","fD=reverse"]     
        build api root True Source
        synchronize api root False
        (inp,out,_,_)<-build1lr root rel
        (mtts,ns)<- readResult out :: IO (OpResult (Maybe [NameDef]))
        assertBool (isJust mtts)
        assertBool (not $ notesInError ns) 
        tapLR inp 3 16
        (tap1,nsErrorsTap)<-readResult out :: IO (OpResult (Maybe ThingAtPoint))
        assertBool (null nsErrorsTap)
        assertBool $ isJust tap1
        assertEqual "map" (tapName $ fromJust tap1)
        changeLR inp rel2 "B.D"
        (mtts2,ns2)<- readResult out :: IO (OpResult (Maybe [NameDef]))
        assertBool (isJust mtts2)
        assertBool (not $ notesInError ns2) 
        tapLR inp 2 6
        (tap2,nsErrorsTap2)<-readResult out :: IO (OpResult (Maybe ThingAtPoint))
        assertBool (null nsErrorsTap2)
        assertBool $ isJust tap2
        assertEqual "reverse" (tapName $ fromJust tap2)
        changeLR inp rel "Main"
        (mtts3,ns3)<- readResult out :: IO (OpResult (Maybe [NameDef]))
        assertBool (isJust mtts3)
        assertBool (not $ notesInError ns3) 
        tapLR inp 3 16
        (tap3,nsErrorsTap3)<-readResult out :: IO (OpResult (Maybe ThingAtPoint))
        assertBool (null nsErrorsTap3)
        assertBool $ isJust tap3
        assertEqual "map" (tapName $ fromJust tap3)
        