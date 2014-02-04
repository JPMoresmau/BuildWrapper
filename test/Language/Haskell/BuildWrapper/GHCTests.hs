{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- |
-- Module      : Language.Haskell.BuildWrapper.GHCTests
-- Copyright   : (c) JP Moresmau 2011
-- License     : BSD3
-- 
-- Maintainer  : jpmoresmau@gmail.com
-- Stability   : beta
-- Portability : portable
-- 
-- Test some specific GHC operations
module Language.Haskell.BuildWrapper.GHCTests where

import Language.Haskell.BuildWrapper.Base
import Language.Haskell.BuildWrapper.GHC

import Test.Framework
import Test.HUnit (Assertion)

test_NoPreproc:: Assertion
test_NoPreproc=
        do
        let s="module Main\nwhere\nimport Data.Map\nmain=undefined\n"
        let (tt,s2)=preprocessSource s False
        assertBool (null tt)
        assertEqual s s2
        
test_Preproc:: Assertion
test_Preproc=
        do
        let s="\nmodule Main\nwhere\n#if GHC_VERSION=612\nimport Data.Map\n#endif\nmain=undefined\n"
        let (tt,s2)=preprocessSource s False
        assertEqual 2 (length tt)
        let (t1:t2:[])=tt
        assertEqual (TokenDef "PP" (mkLocation 4 1 4 20)) t1
        assertEqual (TokenDef "PP" (mkLocation 6 1 6 7)) t2
        assertEqual "\nmodule Main\nwhere\n\nimport Data.Map\n\nmain=undefined\n" s2
       

test_PreprocContiguous:: Assertion
test_PreprocContiguous=
        do
        let s="\nmodule Main\nwhere\n#if GHC_VERSION=612\nimport Data.Map\n#endif\n#if GHC_VERSION=612\nimport Data.Maybe\n#endif\nmain=undefined\n"
        let (tt,s2)=preprocessSource s False
        assertEqual 4 (length tt)
        let (t1:t2:t3:t4:[])=tt
        assertEqual (TokenDef "PP" (mkLocation 4 1 4 20)) t1
        assertEqual (TokenDef "PP" (mkLocation 6 1 6 7)) t2
        assertEqual (TokenDef "PP" (mkLocation 7 1 7 20)) t3
        assertEqual (TokenDef "PP" (mkLocation 9 1 9 7)) t4
        assertEqual "\nmodule Main\nwhere\n\nimport Data.Map\n\n\nimport Data.Maybe\n\nmain=undefined\n" s2


test_PreprocPragma:: Assertion
test_PreprocPragma=
        do
        let s="#if GHC_VERSION=612\n{-# LANGUAGE OverloadedStrings #-}\n#endif\nmodule Main\n"
        let (tt,s2)=preprocessSource s False
        assertEqual 3 (length tt)
        let (t1:t2:t3:[])=tt
        assertEqual (TokenDef "PP" (mkLocation 1 1 1 20)) t1
        assertEqual (TokenDef "P" (mkLocation 2 1 2 35)) t2
        assertEqual (TokenDef "PP" (mkLocation 3 1 3 7)) t3
        assertEqual "                                  \n\n\nmodule Main\n" s2

test_PreprocPragmaInside:: Assertion
test_PreprocPragmaInside=
        do
        let s="module Main({-# LANGUAGE OverloadedStrings #-})\n"
        let (tt,s2)=preprocessSource s False
        assertEqual 1 (length tt)
        let (t1:[])=tt
        assertEqual (TokenDef "P" (mkLocation 1 13 1 47)) t1
        assertEqual "module Main(                                  )\n" s2

test_PreprocPragma2Lines:: Assertion
test_PreprocPragma2Lines=
        do
        let s="#if GHC_VERSION=612\n{-# LANGUAGE OverloadedStrings,\n  RankNTypes,\n  MultiParamTypeClasses #-}\n#endif\nmodule Main\n"
        let (tt,s2)=preprocessSource s False
        assertEqual 5 (length tt)
        let (t1:t2:t3:t4:t5:[])=tt
        assertEqual (TokenDef "PP" (mkLocation 1 1 1 20)) t1
        assertEqual (TokenDef "P" (mkLocation 2 1 2 32)) t2
        assertEqual (TokenDef "P" (mkLocation 3 1 3 14)) t3
        assertEqual (TokenDef "P" (mkLocation 4 1 4 28)) t4
        assertEqual (TokenDef "PP" (mkLocation 5 1 5 7)) t5
        assertEqual "\n\n\n\n\nmodule Main\n" s2

test_Preproc2Lines:: Assertion
test_Preproc2Lines=
        do
        let s="\nmodule Main\nwhere\n#if GHC_VERSION\\\n=612\nimport Data.Map\n#endif\nmain=undefined\n"
        let (tt,s2)=preprocessSource s False
        assertEqual 3 (length tt)
        let (t1:t2:t3:[])=tt
        --putStrLn $ show tt
        assertEqual (TokenDef "PP" (mkLocation 4 1 4 17)) t1
        assertEqual (TokenDef "PP" (mkLocation 5 1 5 5)) t2
        assertEqual (TokenDef "PP" (mkLocation 7 1 7 7)) t3
        assertEqual  "\nmodule Main\nwhere\n\n\nimport Data.Map\n\nmain=undefined\n" s2
  
        
test_Literate:: Assertion
test_Literate= 
        do
        let s="comment for literate module\n> module Main\n2nd comment\n> where\n> import Data.Map\n"
        let (tt,s2)=preprocessSource s True
        assertEqual 2 (length tt)
        let (t1:t2:[])=tt
        assertEqual (TokenDef "DL" (mkLocation 1 1 1 28)) t1
        assertEqual (TokenDef "DL" (mkLocation 3 1 3 12)) t2
        assertEqual "\n  module Main\n\n  where\n  import Data.Map\n" s2


test_LiterateLatex:: Assertion
test_LiterateLatex= 
        do
        assertEqual 2 (length $ lines "line1\r\nline2")
        let s="\\begin{code}\r\n module Main\n where\n import Data.Map\n\\end{code}\n"
        let (tt,s2)=preprocessSource s True
        assertEqual 2 (length tt)
        let (t1:t2:[])=tt
        assertEqual (TokenDef "DL" (mkLocation 1 1 1 13)) t1
        assertEqual (TokenDef "DL" (mkLocation 5 1 5 11)) t2
        assertEqual "\n module Main\n where\n import Data.Map\n\n" s2

        
test_KeepIndent :: Assertion
test_KeepIndent = do
        let s="\nmodule Main\nwhere\nmain=do\n#if GHC_VERSION=612\n    do1\n#endif\n    do2\n"
        let (tt,s2)=preprocessSource s False
        assertEqual  2 (length tt)
        let (t1:t2:[])=tt
        assertEqual (TokenDef "PP" (mkLocation 5 1 5 20)) t1
        assertEqual (TokenDef "PP" (mkLocation 7 1 7 7)) t2
        assertEqual "\nmodule Main\nwhere\nmain=do\n    \n    do1\n    \n    do2\n" s2

        
mkLocation :: Int -> Int -> Int -> Int -> InFileSpan         
mkLocation sl sc el ec=InFileSpan (InFileLoc sl sc) (InFileLoc el ec)
