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
-- TODO replace with test-framework
module Main where

import Language.Haskell.BuildWrapper.APITest
import Language.Haskell.BuildWrapper.CMDTests

import Test.Framework (defaultMain, testGroup,Test)
import Test.Framework.Providers.HUnit

main :: IO()
main = defaultMain tests

tests :: [Test]
tests = [testGroup "Unit Tests" (concatMap (hUnitTestToTests) unitTests)
        ,testGroup "Command Tests" (concatMap (hUnitTestToTests) cmdTests)
        ]

--import Language.Haskell.BuildWrapper.APITest
--import Language.Haskell.BuildWrapper.CMDTests
--
--import Control.Monad
--import Data.Monoid
--
--import System.Exit (exitFailure)
--import Test.HUnit
--
--main :: IO()
--main = do
--    unit<-mapM runTestTT [unitTests]
--    cmd<-mapM runTestTT [cmdTests]
--    --let cmd=[]
--    let allCounts=mconcat (unit ++ cmd)
--    when ((errors allCounts)>0 || (failures allCounts)>0) exitFailure     
--        
--        
--instance Monoid Counts where
--        mempty =Counts 0 0 0 0
--        mappend (Counts a b c d) (Counts a' b' c' d')=Counts (a+a') (b+b') (c+c') (d+d')
