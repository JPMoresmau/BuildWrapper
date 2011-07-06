
module Main where

import Language.Haskell.BuildWrapper.APITest


import Control.Monad
import Data.Monoid

import System.Exit (exitFailure)
import Test.HUnit

main :: IO()
main = do
    allCounts<- (liftM mconcat) (mapM runTestTT [apiTests])
    when ((errors allCounts)>0 || (failures allCounts)>0) exitFailure     
        
        
instance Monoid Counts where
        mempty =Counts 0 0 0 0
        mappend (Counts a b c d) (Counts a' b' c' d')=Counts (a+a') (b+b') (c+c') (d+d')
