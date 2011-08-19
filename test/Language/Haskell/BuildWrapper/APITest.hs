{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.BuildWrapper.APITest where

import Language.Haskell.BuildWrapper.Base
import qualified Language.Haskell.BuildWrapper.API as API
import Language.Haskell.BuildWrapper.Tests


import Test.HUnit

import Control.Monad.State


apiTests::Test
apiTests=TestList $ map (\f->f DirectAPI) tests

data DirectAPI=DirectAPI

instance APIFacade DirectAPI where
        synchronize _ r= runAPI r API.synchronize
        synchronize1 _ r= runAPI r . API.synchronize1
        write _ r fp s= runAPI r $ API.write fp s
        configure _ r= runAPI r . API.configure
        build _ r= runAPI r . API.build
        getOutline _ r= runAPI r . API.getOutline
        getTokenTypes _ r= runAPI r . API.getTokenTypes
        getOccurrences _ r fp s= runAPI r $ API.getOccurrences fp s 
        getThingAtPoint _ r fp l c q t= runAPI r $ API.getThingAtPoint fp l c q t
        getNamesInScope _ r= runAPI r . API.getNamesInScope

runAPI:: FilePath -> StateT BuildWrapperState IO a -> IO a
runAPI root f= do
        evalStateT f (BuildWrapperState ".dist-buildwrapper" "cabal" (testCabalFile root) Normal)
        