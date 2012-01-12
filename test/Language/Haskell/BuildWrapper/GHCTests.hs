{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.BuildWrapper.GHCTests where

import Test.HUnit
import Language.Haskell.BuildWrapper.Base
import Language.Haskell.BuildWrapper.GHC


ghcTests :: [Test]
ghcTests=[testNoPreproc,testPreproc,testPreprocContiguous,testPreproc2Lines,testLiterate,testLiterateLatex,
        testPreprocPragma ,testKeepIndent]

testNoPreproc:: Test
testNoPreproc=TestLabel "testNoPreproc" (TestCase (
        do
        let s="module Main\nwhere\nimport Data.Map\nmain=undefined\n"
        let (tt,s2)=preprocessSource s False
        assertBool "tt is not empty" (null tt)
        assertEqual ("content has changed: "++ s2) s s2
        ))
        
testPreproc:: Test
testPreproc=TestLabel "testPreproc" (TestCase (
        do
        let s="\nmodule Main\nwhere\n#if GHC_VERSION=612\nimport Data.Map\n#endif\nmain=undefined\n"
        let (tt,s2)=preprocessSource s False
        assertEqual "tt is not 2" 2 (length tt)
        let (t1:t2:[])=tt
        assertEqual "first tt is not correct" (TokenDef "PP" (mkLocation 4 1 4 20)) (t1)
        assertEqual "second tt is not correct" (TokenDef "PP" (mkLocation 6 1 6 7)) (t2)
        assertEqual ("content is not what expected: "++ s2) "\nmodule Main\nwhere\n\nimport Data.Map\n\nmain=undefined\n" s2
        ))       

testPreprocContiguous:: Test
testPreprocContiguous=TestLabel "testPreprocContiguous" (TestCase (
        do
        let s="\nmodule Main\nwhere\n#if GHC_VERSION=612\nimport Data.Map\n#endif\n#if GHC_VERSION=612\nimport Data.Maybe\n#endif\nmain=undefined\n"
        let (tt,s2)=preprocessSource s False
        assertEqual "tt is not 4" 4 (length tt)
        let (t1:t2:t3:t4:[])=tt
        assertEqual "first tt is not correct" (TokenDef "PP" (mkLocation 4 1 4 20)) (t1)
        assertEqual "second tt is not correct" (TokenDef "PP" (mkLocation 6 1 6 7)) (t2)
        assertEqual "third tt is not correct" (TokenDef "PP" (mkLocation 7 1 7 20)) (t3)
        assertEqual "fourth tt is not correct" (TokenDef "PP" (mkLocation 9 1 9 7)) (t4)
        assertEqual ("content is not what expected: "++ s2) "\nmodule Main\nwhere\n\nimport Data.Map\n\n\nimport Data.Maybe\n\nmain=undefined\n" s2
        ))  

testPreprocPragma:: Test
testPreprocPragma=TestLabel "testPreprocPragma" (TestCase (
        do
        let s="#if GHC_VERSION=612\n{-# LANGUAGE OverloadedStrings #-}\n#endif\nmodule Main\n"
        let (tt,s2)=preprocessSource s False
        assertEqual "tt is not 3" 3 (length tt)
        let (t1:t2:t3:[])=tt
        assertEqual "first tt is not correct" (TokenDef "PP" (mkLocation 1 1 1 20)) (t1)
        assertEqual "second tt is not correct" (TokenDef "D" (mkLocation 2 1 2 35)) (t2)
        assertEqual "third tt is not correct" (TokenDef "PP" (mkLocation 3 1 3 7)) (t3)
        assertEqual ("content is not what expected: "++ s2) "\n\n\nmodule Main\n" s2
        ))  

testPreproc2Lines:: Test
testPreproc2Lines=TestLabel "testPreproc2Lines" (TestCase (
        do
        let s="\nmodule Main\nwhere\n#if GHC_VERSION\\\n=612\nimport Data.Map\n#endif\nmain=undefined\n"
        let (tt,s2)=preprocessSource s False
        assertEqual "tt is not 3" 3 (length tt)
        let (t1:t2:t3:[])=tt
        --putStrLn $ show tt
        assertEqual "first tt is not correct" (TokenDef "PP" (mkLocation 4 1 4 17)) (t1)
        assertEqual "second tt is not correct" (TokenDef "PP" (mkLocation 5 1 5 5)) (t2)
        assertEqual "third tt is not correct" (TokenDef "PP" (mkLocation 7 1 7 7)) (t3)
        assertEqual ("content is not what expected: "++ s2) "\nmodule Main\nwhere\n\n\nimport Data.Map\n\nmain=undefined\n" s2
        ))       
        
testLiterate:: Test
testLiterate= TestLabel "testLiterate" (TestCase (
        do
        let s="comment for literate module\n> module Main\n2nd comment\n> where\n> import Data.Map\n"
        let (tt,s2)=preprocessSource s True
        assertEqual "tt is not 2" 2 (length tt)
        let (t1:t2:[])=tt
        assertEqual "first tt is not correct" (TokenDef "DL" (mkLocation 1 1 1 28)) (t1)
        assertEqual "second tt is not correct" (TokenDef "DL" (mkLocation 3 1 3 12)) (t2)
        assertEqual ("content is not what expected: "++ s2) "\n  module Main\n\n  where\n  import Data.Map\n" s2
        ))

testLiterateLatex:: Test
testLiterateLatex= TestLabel "testLiterateLatex" (TestCase (
        do
        assertEqual "lines does not give 2" 2 (length $ lines "line1\r\nline2")
        let s="\\begin{code}\r\n module Main\n where\n import Data.Map\n\\end{code}\n"
        let (tt,s2)=preprocessSource s True
        assertEqual "tt is not 2" 2 (length tt)
        let (t1:t2:[])=tt
        assertEqual "first tt is not correct" (TokenDef "DL" (mkLocation 1 1 1 13)) (t1)
        assertEqual "second tt is not correct" (TokenDef "DL" (mkLocation 5 1 5 11)) (t2)
        assertEqual ("content is not what expected: "++ s2) "\n module Main\n where\n import Data.Map\n\n" s2
        ))
        
testKeepIndent :: Test
testKeepIndent = TestLabel "testKeepIndent" (TestCase ( do
        let s="\nmodule Main\nwhere\nmain=do\n#if GHC_VERSION=612\n    do1\n#endif\n    do2\n"
        let (tt,s2)=preprocessSource s False
        assertEqual "tt is not 2" 2 (length tt)
        let (t1:t2:[])=tt
        assertEqual "first tt is not correct" (TokenDef "PP" (mkLocation 5 1 5 20)) (t1)
        assertEqual "second tt is not correct" (TokenDef "PP" (mkLocation 7 1 7 7)) (t2)
        assertEqual ("content is not what expected: "++ s2) "\nmodule Main\nwhere\nmain=do\n    \n    do1\n    \n    do2\n" s2
        ))
        
mkLocation :: Int -> Int -> Int -> Int -> InFileSpan         
mkLocation sl sc el ec=InFileSpan (InFileLoc sl sc) (InFileLoc el ec)
