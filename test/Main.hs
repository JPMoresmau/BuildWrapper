{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- |
-- Module      : Mainau
-- Copyright   : (c) JP Moresmau 2011
-- License     : BSD3
-- 
-- Maintainer  : jpmoresmau@gmail.com
-- Stability   : beta
-- Portability : portable
-- 
-- Testing exe entry point
module Main where

import {-@ HTF_TESTS @-} Language.Haskell.BuildWrapper.APITest
import {-@ HTF_TESTS @-} Language.Haskell.BuildWrapper.CMDTests
import {-@ HTF_TESTS @-} Language.Haskell.BuildWrapper.CMDLongRunningTests
import {-@ HTF_TESTS @-} Language.Haskell.BuildWrapper.GHCTests
import {-@ HTF_TESTS @-} Language.Haskell.BuildWrapper.UsagesTests
import {-@ HTF_TESTS @-} Language.Haskell.BuildWrapper.ImportsTests
import {-@ HTF_TESTS @-} Language.Haskell.BuildWrapper.CabalDevTests

import Test.Framework 

main :: IO()
main = htfMain htf_importedTests
