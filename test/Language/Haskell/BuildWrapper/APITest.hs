{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Language.Haskell.BuildWrapper.APITest
-- Author      : JP Moresmau
-- Copyright   : (c) JP Moresmau 2011
-- License     : BSD3
-- 
-- Maintainer  : jpmoresmau@gmail.com
-- Stability   : beta
-- Portability : portable
-- 
-- Direct tests on the API code
module Language.Haskell.BuildWrapper.APITest where

import Language.Haskell.BuildWrapper.Base
import qualified Language.Haskell.BuildWrapper.Cabal as Cabal

import Test.HUnit


unitTests :: [Test]
unitTests=[testGetBuiltPath,testParseBuildMessages,testParseConfigureMessages]

testGetBuiltPath :: Test
testGetBuiltPath = TestLabel "testGetBuiltPath" (TestCase (do
        assertEqual "backslash path" (Just "src\\Language\\Haskell\\BuildWrapper\\Cabal.hs") $ Cabal.getBuiltPath "[4 of 7] Compiling Language.Haskell.BuildWrapper.Cabal ( src\\Language\\Haskell\\BuildWrapper\\Cabal.hs, dist\\build\\Language\\Haskell\\BuildWrapper\\Cabal.o )"
        assertEqual "forward slash path" (Just "src/Language/Haskell/BuildWrapper/Cabal.hs") $ Cabal.getBuiltPath "[4 of 7] Compiling Language.Haskell.BuildWrapper.Cabal ( src/Language/Haskell/BuildWrapper/Cabal.hs, dist/build/Language/Haskell/BuildWrapper/Cabal.o )"
        assertEqual "no path" Nothing $ Cabal.getBuiltPath "something else"
        ))     
        
testParseBuildMessages :: Test
testParseBuildMessages = TestLabel "testParseBuildMessages" (TestCase (do
        let s="Main.hs:2:3:Warning: Top-level binding with no type signature:\n           tests :: [test-framework-0.4.1.1:Test.Framework.Core.Test]\nLinking"
        let notes=Cabal.parseBuildMessages s
        assertEqual "not 1 note" 1 (length notes)
        assertEqual "not expected note" (BWNote BWWarning "Top-level binding with no type signature:\n           tests :: [test-framework-0.4.1.1:Test.Framework.Core.Test]\n" (BWLocation "Main.hs" 2 3)) (head notes)
        ))
        
testParseConfigureMessages :: Test
testParseConfigureMessages = TestLabel "testParseConfigureMessages" (TestCase (do
        let s="cabal.exe: Missing dependencies on foreign libraries:\n* Missing C libraries: tinfo, tinfo\n"
        let notes=Cabal.parseCabalMessages "test.cabal" "cabal.exe" s
        assertEqual "not 1 note" 1 (length notes)   
        assertEqual "not expected note" (BWNote BWError "Missing dependencies on foreign libraries:\n* Missing C libraries: tinfo, tinfo\n" (BWLocation "test.cabal" 1 1)) (head notes)
        ))