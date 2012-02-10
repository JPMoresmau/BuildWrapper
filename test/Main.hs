-- |
-- Module      : Main
-- Author      : JP Moresmau
-- Copyright   : (c) JP Moresmau 2011
-- License     : BSD3
-- 
-- Maintainer  : jpmoresmau@gmail.com
-- Stability   : beta
-- Portability : portable
-- 
-- Testing exe entry point
module Main where

import Language.Haskell.BuildWrapper.APITest
import Language.Haskell.BuildWrapper.CMDTests
import Language.Haskell.BuildWrapper.GHCTests

import Test.Framework (defaultMain, testGroup,Test)
import Test.Framework.Providers.HUnit

main :: IO()
main = defaultMain tests

tests :: [Test]
tests = [testGroup "Unit Tests" (concatMap hUnitTestToTests unitTests)
        ,testGroup "GHC Tests" (concatMap hUnitTestToTests ghcTests)
        ,testGroup "Command Tests" (concatMap hUnitTestToTests cmdTests)
        ]

