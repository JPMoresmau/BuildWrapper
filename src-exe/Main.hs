-- |
-- Module      : Main
-- Copyright   : (c) JP Moresmau 2011
-- License     : BSD3
-- 
-- Maintainer  : jpmoresmau@gmail.com
-- Stability   : beta
-- Portability : portable
-- 
-- Executable entry point
module Main where

import Language.Haskell.BuildWrapper.CMD
import System.IO (hSetBuffering, stdout, BufferMode(..))

-- | main entry point
main::IO()
main = do
  hSetBuffering stdout LineBuffering
  cmdMain