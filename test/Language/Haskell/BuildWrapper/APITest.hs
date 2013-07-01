{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- |
-- Module      : Language.Haskell.BuildWrapper.APITest
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

import Test.Framework
import Test.HUnit (Assertion)
--data DirectAPI=DirectAPI
--
--instance APIFacade DirectAPI where
--        synchronize _ r ff= runAPI r $ API.synchronize ff
--        synchronize1 _ r ff fp= runAPI r $ API.synchronize1 ff fp
--        write _ r fp s= runAPI r $ API.write fp s
--        configure _ r t= runAPI r $ API.configure t
--        configureWithFlags _ r t fgs= runAPIFlags r (API.configure t) fgs
--        build _ r b wc= runAPI r $ API.build b wc
--        build1 _ r fp= runAPI r $ API.build1 fp Nothing
--        build1c _ r fp ccn= runAPI r $ API.build1 fp (Just ccn)
--        getBuildFlags _ r fp= runAPI r $ API.getBuildFlags fp  Nothing
--        getOutline _ r fp= runAPI r $ API.getOutline fp  Nothing
--        getTokenTypes _ r fp= runAPI r $ API.getTokenTypes fp
--        getOccurrences _ r fp s= runAPI r $ API.getOccurrences fp s  Nothing
--        getThingAtPoint _ r fp l c= runAPI r $ API.getThingAtPoint fp l c  Nothing
--        getNamesInScope _ r fp= runAPI r $ API.getNamesInScope fp  Nothing
--        getCabalDependencies _ r= runAPI r API.getCabalDependencies
--        getCabalComponents _ r= runAPI r API.getCabalComponents
--        generateUsage _ r retAll cc=runAPI r $ API.generateUsage retAll $ cabalComponentName cc
--        
--runAPI:: FilePath -> StateT BuildWrapperState IO a  -> IO a
--runAPI root f =runAPIFlags root f "" 
--
--runAPIFlags:: FilePath -> StateT BuildWrapperState IO a  -> String -> IO a
--runAPIFlags root f flags =
--        evalStateT f (BuildWrapperState ".dist-buildwrapper" "cabal" (testCabalFile root) Normal flags [])


test_GetBuiltPath :: Assertion
test_GetBuiltPath = do
        assertEqual (Just "src\\Language\\Haskell\\BuildWrapper\\Cabal.hs") $ Cabal.getBuiltPath "[4 of 7] Compiling Language.Haskell.BuildWrapper.Cabal ( src\\Language\\Haskell\\BuildWrapper\\Cabal.hs, dist\\build\\Language\\Haskell\\BuildWrapper\\Cabal.o )"
        assertEqual (Just "src/Language/Haskell/BuildWrapper/Cabal.hs") $ Cabal.getBuiltPath "[4 of 7] Compiling Language.Haskell.BuildWrapper.Cabal ( src/Language/Haskell/BuildWrapper/Cabal.hs, dist/build/Language/Haskell/BuildWrapper/Cabal.o )"
        assertEqual Nothing $ Cabal.getBuiltPath "something else"
           
        
test_ParseBuildMessages :: Assertion
test_ParseBuildMessages = do
        let s="Main.hs:2:3:Warning: Top-level binding with no type signature:\n           tests :: [test-framework-0.4.1.1:Test.Framework.Core.Test]\nLinking"
        let notes=Cabal.parseBuildMessages "test.cabal" "cabal.exe" "" s
        assertEqual 1 (length notes)
        assertEqual (BWNote BWWarning "Top-level binding with no type signature:\n           tests :: [test-framework-0.4.1.1:Test.Framework.Core.Test]\n" (mkEmptySpan "Main.hs" 2 3)) (head notes)

        
         
test_ParseBuildMessagesLinker :: Assertion
test_ParseBuildMessagesLinker = do
        let s="Linking D:\\Documents\\Perso\\workspace\\HJVM\\.dist-buildwrapper\\dist\\build\\hjvm-test\\hjvm-test.exe ...\nD:\\Documents\\Perso\\workspace\\HJVM\\.dist-buildwrapper\\dist\\build/libHSHJVM-0.1.a(hjvm.o):hjvm.c:(.text+0x1d): undefined reference to `_imp__JNI_GetCreatedJavaVMs@12'\ncollect2: ld returned 1 exit status\n"
        let notes=Cabal.parseBuildMessages "test.cabal" "cabal.exe" "D:\\Documents\\Perso\\workspace\\HJVM\\.dist-buildwrapper\\dist" s
        assertEqual 1 (length notes)
        assertEqual (BWNote BWError "hjvm.c:(.text+0x1d): undefined reference to `_imp__JNI_GetCreatedJavaVMs@12'\n" (mkEmptySpan "test.cabal" 1 1)) (head notes)


test_ParseBuildMessagesCabal :: Assertion
test_ParseBuildMessagesCabal = do
        let s="cabal.exe: HJVM-0.1: library-dirs: src is a relative path (use --force\nto override)\n"
        let notes=Cabal.parseBuildMessages  "test.cabal" "cabal.exe" "" s
        assertEqual 1 (length notes)
        assertEqual (BWNote BWError "HJVM-0.1: library-dirs: src is a relative path (use --force\nto override)\n" (mkEmptySpan "test.cabal" 1 1)) (head notes)
              
                
test_ParseConfigureMessages :: Assertion
test_ParseConfigureMessages = do
        let s="cabal.exe: Missing dependencies on foreign libraries:\n* Missing C libraries: tinfo, tinfo\n"
        let notes=Cabal.parseCabalMessages "test.cabal" "cabal.exe" s
        assertEqual 1 (length notes)   
        assertEqual (BWNote BWError "Missing dependencies on foreign libraries:\n* Missing C libraries: tinfo, tinfo\n" (mkEmptySpan "test.cabal" 1 1)) (head notes)
        let sx="cabal.exe: Missing dependencies on foreign libraries:\n* Missing C libraries: tinfo, tinfo\n"
        let notesx=Cabal.parseCabalMessages "test.cabal" "cabal-dev.exe" sx
        assertEqual 1 (length notesx)   
        assertEqual (BWNote BWError "Missing dependencies on foreign libraries:\n* Missing C libraries: tinfo, tinfo\n" (mkEmptySpan "test.cabal" 1 1)) (head notesx)
        let s1="cabal: At least the following dependencies are missing:\npgsql -any, pgsql-simple -any, psql-simple -any\n"
        let notes1=Cabal.parseCabalMessages "test.cabal" "cabal" s1
        assertEqual 1 (length notes1)   
        assertEqual (BWNote BWError "At least the following dependencies are missing:\npgsql -any, pgsql-simple -any, psql-simple -any\n" (mkEmptySpan "test.cabal" 1 1)) (head notes1)
        let notes2=Cabal.parseCabalMessages "test.cabal" "cabal-dev" s1
        assertEqual 1 (length notes2)   
        assertEqual (BWNote BWError "At least the following dependencies are missing:\npgsql -any, pgsql-simple -any, psql-simple -any\n" (mkEmptySpan "test.cabal" 1 1)) (head notes2)
        