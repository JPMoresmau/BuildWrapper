{-# LANGUAGE OverloadedStrings #-}
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
import Data.Char

import System.Directory
import System.FilePath
import System.Info

import Control.Monad


import Data.Attoparsec
import Data.Aeson
import Data.Aeson.Parser
import qualified Data.ByteString.Char8 as BS
import Data.List
import System.Exit
import System.Process

import Test.Framework
import Test.HUnit (Assertion)
import System.IO (Handle, hPutStrLn, hFlush)
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
        (mtts,ns)<-(readResult out) :: IO (OpResult (Maybe [NameDef]))
        assertBool (isJust mtts)
        assertBool (not $ notesInError ns)
        let tts=fromJust mtts
        assertBool (NameDef "Main.main" [Function] (Just "IO [Char]") `elem` tts)
        assertBool (NameDef "B.D.fD" [Function] (Just "forall a. a") `elem` tts)
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
        (mtts,ns)<-(readResult out) :: IO (OpResult (Maybe [NameDef]))
        assertBool (isJust mtts)
        assertBool (not $ notesInError ns) 
        eval inp "reverse \"toto\"" 
        s1<- readResult out :: IO [(String,String)]
        assertEqual [("[GHC.Types.Char]","\"otot\"")] s1
        eval inp "main" 
        s2<- readResult out :: IO [(String,String)]
        assertEqual [("[GHC.Types.Char]","\"toto\"")] s2     
        eval inp "MkType1_1"
        s3<- readResult out :: IO [(String,String)]
        assertBool $ isPrefixOf "No instance for" $ snd $ head s3     
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
        (mtts,ns)<-(readResult out) :: IO (OpResult (Maybe [NameDef]))
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
        (mtts,ns)<-(readResult out) :: IO (OpResult (Maybe [NameDef]))
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
        (mtts,ns)<-(readResult out) :: IO (OpResult (Maybe [NameDef]))
        assertBool (isJust mtts)
        assertBool (not $ notesInError ns) 
        localsLR inp 4 1 6 10
        (tap1,nsErrorsTap)<-readResult out :: IO (OpResult ([ThingAtPoint]))
        assertBool (null nsErrorsTap)
        let namesM=map tapName tap1
        assertBool (elem "l2" namesM)
        assertBool (elem "l1" namesM)
        continue inp
        (mtts2,ns2)<-readResult out :: IO (OpResult (Maybe [NameDef]))  
        assertBool (not $ notesInError ns2)
        assertBool (isJust mtts2)
        end inp   