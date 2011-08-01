{-# LANGUAGE TemplateHaskell,TypeSynonymInstances #-}
module Language.Haskell.BuildWrapper.Src where

import Language.Haskell.Exts
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.SrcLoc

import Text.JSON
import Data.DeriveTH
import Data.Derive.JSON

getHSEAST :: FilePath -> String -> [String] -> IO JSValue
getHSEAST fp mod options=do
        let exts=map classifyExtension options
        let mode=defaultParseMode {extensions=exts,ignoreLinePragmas=False} 
        pr<-parseFileWithComments mode fp
        return $ makeObj  [("parse" , (showJSON $ pr))]
        
        
        
$( derive makeJSON ''ParseResult )
$( derive makeJSON ''Module )
-- $( derive makeJSON ''SrcLoc )

instance JSON SrcLoc
        where showJSON src = JSArray  [showJSON $ srcLine src,showJSON $ srcColumn src]

$( derive makeJSON ''ModulePragma )
$( derive makeJSON ''WarningText )
$( derive makeJSON ''ExportSpec )
$( derive makeJSON ''ImportDecl )
$( derive makeJSON ''ImportSpec )
$( derive makeJSON ''Decl )
$( derive makeJSON ''ModuleName )
$( derive makeJSON ''Tool )
$( derive makeJSON ''CName )
$( derive makeJSON ''Name )
$( derive makeJSON ''DataOrNew )
$( derive makeJSON ''QualConDecl )
$( derive makeJSON ''Kind )
$( derive makeJSON ''TyVarBind )
$( derive makeJSON ''Asst )
$( derive makeJSON ''ConDecl )
$( derive makeJSON ''QName )
$( derive makeJSON ''Type )
$( derive makeJSON ''IPName )
$( derive makeJSON ''BangType )
$( derive makeJSON ''SpecialCon )
$( derive makeJSON ''Boxed )
$( derive makeJSON ''FunDep )
$( derive makeJSON ''ClassDecl )
$( derive makeJSON ''GadtDecl )
$( derive makeJSON ''InstDecl )
$( derive makeJSON ''Match )
$( derive makeJSON ''Rhs )
$( derive makeJSON ''Binds )
$( derive makeJSON ''Exp )
$( derive makeJSON ''GuardedRhs )
$( derive makeJSON ''IPBind )
$( derive makeJSON ''Splice )
$( derive makeJSON ''Literal )
$( derive makeJSON ''Pat )
$( derive makeJSON ''XName )
$( derive makeJSON ''Alt )
$( derive makeJSON ''QOp )
$( derive makeJSON ''XAttr )
$( derive makeJSON ''Bracket )
$( derive makeJSON ''QualStmt )
$( derive makeJSON ''Stmt )
$( derive makeJSON ''FieldUpdate )
$( derive makeJSON ''RPat )
$( derive makeJSON ''RPatOp )
$( derive makeJSON ''GuardedAlts )
$( derive makeJSON ''PXAttr )
$( derive makeJSON ''PatField )
$( derive makeJSON ''GuardedAlt )
$( derive makeJSON ''Activation )
$( derive makeJSON ''Annotation )
$( derive makeJSON ''Rule )
$( derive makeJSON ''CallConv )
$( derive makeJSON ''RuleVar )
$( derive makeJSON ''Safety )
$( derive makeJSON ''Op )
$( derive makeJSON ''Assoc )
$( derive makeJSON ''Comment )
$( derive makeJSON ''SrcSpan )

instance JSON Rational
        where showJSON=JSRational False 











