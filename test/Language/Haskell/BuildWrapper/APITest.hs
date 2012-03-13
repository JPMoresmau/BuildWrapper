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
unitTests=[testGetBuiltPath,testParseBuildMessages,testParseBuildMessagesLinker,testParseBuildMessagesCabal
        ,testParseConfigureMessages]

testGetBuiltPath :: Test
testGetBuiltPath = TestLabel "testGetBuiltPath" (TestCase (do
        assertEqual "backslash path" (Just "src\\Language\\Haskell\\BuildWrapper\\Cabal.hs") $ Cabal.getBuiltPath "[4 of 7] Compiling Language.Haskell.BuildWrapper.Cabal ( src\\Language\\Haskell\\BuildWrapper\\Cabal.hs, dist\\build\\Language\\Haskell\\BuildWrapper\\Cabal.o )"
        assertEqual "forward slash path" (Just "src/Language/Haskell/BuildWrapper/Cabal.hs") $ Cabal.getBuiltPath "[4 of 7] Compiling Language.Haskell.BuildWrapper.Cabal ( src/Language/Haskell/BuildWrapper/Cabal.hs, dist/build/Language/Haskell/BuildWrapper/Cabal.o )"
        assertEqual "no path" Nothing $ Cabal.getBuiltPath "something else"
        ))     
        
testParseBuildMessages :: Test
testParseBuildMessages = TestLabel "testParseBuildMessages" (TestCase (do
        let s="Main.hs:2:3:Warning: Top-level binding with no type signature:\n           tests :: [test-framework-0.4.1.1:Test.Framework.Core.Test]\nLinking"
        let notes=Cabal.parseBuildMessages "test.cabal" "cabal.exe" "" s
        assertEqual "not 1 note" 1 (length notes)
        assertEqual "not expected note" (BWNote BWWarning "Top-level binding with no type signature:\n           tests :: [test-framework-0.4.1.1:Test.Framework.Core.Test]\n" (BWLocation "Main.hs" 2 3)) (head notes)
        ))
        
         
testParseBuildMessagesLinker :: Test
testParseBuildMessagesLinker = TestLabel "testParseBuildMessagesLinker" (TestCase (do
        let s="Linking D:\\Documents\\Perso\\workspace\\HJVM\\.dist-buildwrapper\\dist\\build\\hjvm-test\\hjvm-test.exe ...\nD:\\Documents\\Perso\\workspace\\HJVM\\.dist-buildwrapper\\dist\\build/libHSHJVM-0.1.a(hjvm.o):hjvm.c:(.text+0x1d): undefined reference to `_imp__JNI_GetCreatedJavaVMs@12'\ncollect2: ld returned 1 exit status\n"
        let notes=Cabal.parseBuildMessages "test.cabal" "cabal.exe" "D:\\Documents\\Perso\\workspace\\HJVM\\.dist-buildwrapper\\dist" s
        assertEqual "not 1 note" 1 (length notes)
        assertEqual "not expected note" (BWNote BWError "hjvm.c:(.text+0x1d): undefined reference to `_imp__JNI_GetCreatedJavaVMs@12'\n" (BWLocation "test.cabal" 1 1)) (head notes)
        ))

testParseBuildMessagesCabal :: Test
testParseBuildMessagesCabal = TestLabel "testParseBuildMessagesCabal" (TestCase (do
        let s="cabal.exe: HJVM-0.1: library-dirs: src is a relative path (use --force\nto override)\n"
        let notes=Cabal.parseBuildMessages  "test.cabal" "cabal.exe" "" s
        assertEqual "not 1 note" 1 (length notes)
        assertEqual "not expected note" (BWNote BWError "HJVM-0.1: library-dirs: src is a relative path (use --force\nto override)\n" (BWLocation "test.cabal" 1 1)) (head notes)
        ))               
                
testParseConfigureMessages :: Test
testParseConfigureMessages = TestLabel "testParseConfigureMessages" (TestCase (do
        let s="cabal.exe: Missing dependencies on foreign libraries:\n* Missing C libraries: tinfo, tinfo\n"
        let notes=Cabal.parseCabalMessages "test.cabal" "cabal.exe" s
        assertEqual "not 1 note" 1 (length notes)   
        assertEqual "not expected note" (BWNote BWError "Missing dependencies on foreign libraries:\n* Missing C libraries: tinfo, tinfo\n" (BWLocation "test.cabal" 1 1)) (head notes)
        ))