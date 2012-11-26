{-# OPTIONS_GHC -F -pgmF htfpp #-}
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

import {-@ HTF_TESTS @-} Language.Haskell.BuildWrapper.APITest
import {-@ HTF_TESTS @-} Language.Haskell.BuildWrapper.CMDTests
import {-@ HTF_TESTS @-} Language.Haskell.BuildWrapper.GHCTests
import {-@ HTF_TESTS @-} Language.Haskell.BuildWrapper.UsagesTests

import Test.Framework 

main :: IO()
main = htfMain htf_importedTests
