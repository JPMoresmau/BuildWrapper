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

import qualified Language.Haskell.BuildWrapper.API as API
import Language.Haskell.BuildWrapper.Base
import Language.Haskell.BuildWrapper.Tests
import qualified Language.Haskell.BuildWrapper.Cabal as Cabal

import Control.Monad.State
import Test.HUnit

data DirectAPI=DirectAPI

instance APIFacade DirectAPI where
        synchronize _ r ff= runAPI r $ API.synchronize ff
        synchronize1 _ r ff fp= runAPI r $ API.synchronize1 ff fp
        write _ r fp s= runAPI r $ API.write fp s
        configure _ r t= runAPI r $ API.configure t
        configureWithFlags _ r t fgs= runAPIFlags r (API.configure t) fgs
        build _ r b wc= runAPI r $ API.build b wc
        build1 _ r fp= runAPI r $ API.build1 fp
        getBuildFlags _ r fp= runAPI r $ API.getBuildFlags fp
        getOutline _ r fp= runAPI r $ API.getOutline fp
        getTokenTypes _ r fp= runAPI r $ API.getTokenTypes fp
        getOccurrences _ r fp s= runAPI r $ API.getOccurrences fp s
        getThingAtPoint _ r fp l c= runAPI r $ API.getThingAtPoint fp l c
        getNamesInScope _ r fp= runAPI r $ API.getNamesInScope fp
        getCabalDependencies _ r= runAPI r API.getCabalDependencies
        getCabalComponents _ r= runAPI r API.getCabalComponents
        generateUsage _ r retAll cc=runAPI r $ API.generateUsage retAll $ cabalComponentName cc
        
runAPI:: FilePath -> StateT BuildWrapperState IO a  -> IO a
runAPI root f =runAPIFlags root f "" 

runAPIFlags:: FilePath -> StateT BuildWrapperState IO a  -> String -> IO a
runAPIFlags root f flags =
        evalStateT f (BuildWrapperState ".dist-buildwrapper" "cabal" (testCabalFile root) Normal flags [])

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
        assertEqual "not expected note" (BWNote BWWarning "Top-level binding with no type signature:\n           tests :: [test-framework-0.4.1.1:Test.Framework.Core.Test]\n" (mkEmptySpan "Main.hs" 2 3)) (head notes)
        ))
        
         
testParseBuildMessagesLinker :: Test
testParseBuildMessagesLinker = TestLabel "testParseBuildMessagesLinker" (TestCase (do
        let s="Linking D:\\Documents\\Perso\\workspace\\HJVM\\.dist-buildwrapper\\dist\\build\\hjvm-test\\hjvm-test.exe ...\nD:\\Documents\\Perso\\workspace\\HJVM\\.dist-buildwrapper\\dist\\build/libHSHJVM-0.1.a(hjvm.o):hjvm.c:(.text+0x1d): undefined reference to `_imp__JNI_GetCreatedJavaVMs@12'\ncollect2: ld returned 1 exit status\n"
        let notes=Cabal.parseBuildMessages "test.cabal" "cabal.exe" "D:\\Documents\\Perso\\workspace\\HJVM\\.dist-buildwrapper\\dist" s
        assertEqual "not 1 note" 1 (length notes)
        assertEqual "not expected note" (BWNote BWError "hjvm.c:(.text+0x1d): undefined reference to `_imp__JNI_GetCreatedJavaVMs@12'\n" (mkEmptySpan "test.cabal" 1 1)) (head notes)
        ))

testParseBuildMessagesCabal :: Test
testParseBuildMessagesCabal = TestLabel "testParseBuildMessagesCabal" (TestCase (do
        let s="cabal.exe: HJVM-0.1: library-dirs: src is a relative path (use --force\nto override)\n"
        let notes=Cabal.parseBuildMessages  "test.cabal" "cabal.exe" "" s
        assertEqual "not 1 note" 1 (length notes)
        assertEqual "not expected note" (BWNote BWError "HJVM-0.1: library-dirs: src is a relative path (use --force\nto override)\n" (mkEmptySpan "test.cabal" 1 1)) (head notes)
        ))               
                
testParseConfigureMessages :: Test
testParseConfigureMessages = TestLabel "testParseConfigureMessages" (TestCase (do
        let s="cabal.exe: Missing dependencies on foreign libraries:\n* Missing C libraries: tinfo, tinfo\n"
        let notes=Cabal.parseCabalMessages "test.cabal" "cabal.exe" s
        assertEqual "not 1 note" 1 (length notes)   
        assertEqual "not expected note" (BWNote BWError "Missing dependencies on foreign libraries:\n* Missing C libraries: tinfo, tinfo\n" (mkEmptySpan "test.cabal" 1 1)) (head notes)
        ))