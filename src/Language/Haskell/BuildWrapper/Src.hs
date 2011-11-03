{-# LANGUAGE TypeSynonymInstances,OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Language.Haskell.BuildWrapper.Src
-- Author      : JP Moresmau
-- Copyright   : (c) JP Moresmau 2011
-- License     : BSD3
-- 
-- Maintainer  : jpmoresmau@gmail.com
-- Stability   : beta
-- Portability : portable
-- 
-- Use haskell-src-exts to get a module outline
module Language.Haskell.BuildWrapper.Src where

import Language.Haskell.BuildWrapper.Base

import Language.Haskell.Exts.Annotated


import qualified Data.Text as T
--import Text.JSON
--import Data.DeriveTH
--import Data.Derive.JSON

getHSEAST :: String -> [String] -> IO (ParseResult (Module SrcSpanInfo, [Comment]))
getHSEAST input options=do
        --putStrLn $ show options
        let exts=map classifyExtension options
        let extsFull=if elem "-fglasgow-exts" options
                then exts ++ glasgowExts
                else exts
        --putStrLn input
        --putStrLn $ show exts
        let mode=defaultParseMode {extensions=extsFull,ignoreLinePragmas=False,ignoreLanguagePragmas=False} 
        return $ parseFileContentsWithComments mode input
        --return $ makeObj  [("parse" , (showJSON $ pr))]
        
getHSEOutline :: (Module SrcSpanInfo, [Comment]) -> [OutlineDef]
getHSEOutline (Module _ _ _ _ decls,comments)=concat $ map declOutline decls
        where 
                declOutline :: Decl SrcSpanInfo -> [OutlineDef]
                declOutline (DataFamDecl l _ h _) = [OutlineDef (headDecl h) [Data,Family] (makeSpan l) []]
                declOutline (DataInsDecl l _ t cons _) = [OutlineDef (typeDecl t) [Data,Instance] (makeSpan l) (map qualConDeclOutline cons)]
                --declOutline (GDataInsDecl l _ t cons _) = [OutlineDef (typeDecl t) [Data,Instance] (makeSpan l) (map qualConDeclOutline cons)]
                declOutline (DataDecl l _ _ h cons _) = [OutlineDef (headDecl h) [Data] (makeSpan l) (map qualConDeclOutline cons)]
                --declOutline (GDataDecl l _ _ h cons _) = [OutlineDef (headDecl h) [Data] (makeSpan l) (map qualConDeclOutline cons)]
                declOutline (TypeFamDecl l h _) = [OutlineDef (headDecl h) [Type,Family] (makeSpan l) []]
                declOutline (TypeInsDecl l t1 _) = [OutlineDef ((typeDecl t1) ) [Type,Instance] (makeSpan l) []] -- ++ " "++(typeDecl t2)
                declOutline (TypeDecl l h _) = [OutlineDef (headDecl h) [Type] (makeSpan l) []]
                declOutline (ClassDecl l _ h _ cdecls) = [OutlineDef (headDecl h) [Class] (makeSpan l) (maybe [] (concatMap classDecl) cdecls)]
                declOutline (FunBind l matches) = [OutlineDef (matchDecl $ head matches) [Function] (makeSpan l) []]
                declOutline (PatBind l (PVar _ n) _ _ _)=[OutlineDef (nameDecl n) [Function] (makeSpan l) []]
                declOutline (InstDecl l _ h idecls)=[OutlineDef (iheadDecl h) [Instance] (makeSpan l) (maybe [] (concatMap instDecl) idecls)]
                declOutline (SpliceDecl l e)=[OutlineDef (spliceDecl e) [Splice] (makeSpan l) []]
                declOutline _ = []
                qualConDeclOutline :: QualConDecl SrcSpanInfo-> OutlineDef
                qualConDeclOutline (QualConDecl l _ _ con)=let
                        (n,defs)=conDecl con
                        in OutlineDef n [Constructor] (makeSpan l) defs
                declOutlineInClass :: Decl SrcSpanInfo -> [OutlineDef]
                declOutlineInClass (TypeSig l ns _)=map (\n->OutlineDef (nameDecl n) [Function] (makeSpan l) []) ns
                declOutlineInClass o=declOutline o
                headDecl :: DeclHead  a -> T.Text
                headDecl (DHead _ n  _)=nameDecl n
                headDecl (DHInfix _ _ n _)=nameDecl n
                headDecl (DHParen _ h)=headDecl h
                nameDecl :: Name a -> T.Text
                nameDecl (Ident _ s)=T.pack s
                nameDecl (Symbol _ s)=T.pack s
                typeDecl :: Type a -> T.Text
                typeDecl (TyForall _ _ _ t)=typeDecl t
                typeDecl (TyVar _ n )=nameDecl n
                typeDecl (TyCon _ qn )=qnameDecl qn
                typeDecl (TyList _ t )=T.concat  ["[",(typeDecl t),"]"]
                typeDecl (TyParen _ t )=typeDecl t
                typeDecl (TyApp _ t1 t2)=T.concat  [(typeDecl t1) , " ",(typeDecl t2)]
                typeDecl _ = ""
                qnameDecl :: QName a -> T.Text
                qnameDecl (Qual _ _ n)=nameDecl n
                qnameDecl (UnQual _ n)=nameDecl n
                qnameDecl _ =""
                matchDecl :: Match a -> T.Text
                matchDecl (Match _ n _ _ _)=nameDecl n     
                matchDecl (InfixMatch _ _ n _ _ _)=nameDecl n    
                iheadDecl :: InstHead a -> T.Text
                iheadDecl (IHead _ qn ts)= T.concat  [(qnameDecl qn) , " " , (T.intercalate " " (map typeDecl ts))]
                iheadDecl (IHInfix _ t1 qn t2)= T.concat  [(typeDecl t1), " ", (qnameDecl qn) , " " , (typeDecl t2)]
                iheadDecl (IHParen _ i)=iheadDecl i
                conDecl :: ConDecl SrcSpanInfo -> (T.Text,[OutlineDef])
                conDecl (ConDecl _ n _)=(nameDecl n,[])
                conDecl (InfixConDecl _ _ n _)=(nameDecl n,[])
                conDecl (RecDecl _ n fields)=(nameDecl n,concatMap fieldDecl fields)
                fieldDecl :: FieldDecl SrcSpanInfo -> [OutlineDef]
                fieldDecl (FieldDecl l ns _)=map (\n->OutlineDef (nameDecl n) [Field] (makeSpan l) []) ns
                classDecl :: ClassDecl SrcSpanInfo -> [OutlineDef]
                classDecl (ClsDecl _ d) = declOutlineInClass d
                classDecl _ = []
                instDecl :: InstDecl SrcSpanInfo -> [OutlineDef]
                instDecl (InsDecl _ d) = declOutlineInClass d
                instDecl _ = []
                spliceDecl :: Exp SrcSpanInfo -> T.Text
                spliceDecl (SpliceExp _ sp)= spliceName sp
                spliceDecl (App _ e1 _)=spliceDecl e1
                spliceDecl (Var _ qn)=qnameDecl qn
                spliceDecl _ = ""
                spliceName :: Splice SrcSpanInfo -> T.Text
                spliceName (IdSplice _ n)=T.pack n
                spliceName (ParenSplice  _ e)=spliceDecl e

getHSEOutline _ = []

 
makeSpan :: SrcSpanInfo -> InFileSpan
makeSpan si=let
        sis=srcInfoSpan si
        (sl,sc)=srcSpanStart sis
        (el,ec)=srcSpanEnd sis      
        in   InFileSpan (InFileLoc sl sc) (InFileLoc el ec)
   
knownExtensionNames :: [String]
knownExtensionNames = map show knownExtensions
   
        
-- $( derive makeJSON ''ParseResult )
-- $( derive makeJSON ''Module )
-- $( derive makeJSON ''ModuleHead )
-- $( derive makeJSON ''ExportSpecList )
---- $( derive makeJSON ''SrcLoc )
--
--instance JSON SrcLoc
--        where showJSON src = JSArray  [showJSON $ srcLine src,showJSON $ srcColumn src]
--
-- $( derive makeJSON ''ModulePragma )
-- $( derive makeJSON ''WarningText )
-- $( derive makeJSON ''ExportSpec )
-- $( derive makeJSON ''ImportDecl )
-- $( derive makeJSON ''ImportSpecList )
-- $( derive makeJSON ''ImportSpec )
-- $( derive makeJSON ''Decl )
-- $( derive makeJSON ''DeclHead )
-- $( derive makeJSON ''Deriving )
-- $( derive makeJSON ''Context )
-- $( derive makeJSON ''InstHead )
-- $( derive makeJSON ''ModuleName )
-- $( derive makeJSON ''Tool )
-- $( derive makeJSON ''CName )
-- $( derive makeJSON ''Name )
-- $( derive makeJSON ''DataOrNew )
-- $( derive makeJSON ''QualConDecl )
-- $( derive makeJSON ''Kind )
-- $( derive makeJSON ''TyVarBind )
-- $( derive makeJSON ''Asst )
-- $( derive makeJSON ''ConDecl )
-- $( derive makeJSON ''FieldDecl )
-- $( derive makeJSON ''QName )
-- $( derive makeJSON ''Type )
-- $( derive makeJSON ''IPName )
-- $( derive makeJSON ''BangType )
-- $( derive makeJSON ''SpecialCon )
-- $( derive makeJSON ''Boxed )
-- $( derive makeJSON ''FunDep )
-- $( derive makeJSON ''ClassDecl )
-- $( derive makeJSON ''GadtDecl )
-- $( derive makeJSON ''InstDecl )
-- $( derive makeJSON ''Match )
-- $( derive makeJSON ''Rhs )
-- $( derive makeJSON ''Binds )
-- $( derive makeJSON ''Exp )
-- $( derive makeJSON ''GuardedRhs )
-- $( derive makeJSON ''IPBind )
-- $( derive makeJSON ''Splice )
-- $( derive makeJSON ''Literal )
-- $( derive makeJSON ''Pat )
-- $( derive makeJSON ''XName )
-- $( derive makeJSON ''Alt )
-- $( derive makeJSON ''QOp )
-- $( derive makeJSON ''XAttr )
-- $( derive makeJSON ''Bracket )
-- $( derive makeJSON ''QualStmt )
-- $( derive makeJSON ''Stmt )
-- $( derive makeJSON ''FieldUpdate )
-- $( derive makeJSON ''RPat )
-- $( derive makeJSON ''RPatOp )
-- $( derive makeJSON ''GuardedAlts )
-- $( derive makeJSON ''PXAttr )
-- $( derive makeJSON ''PatField )
-- $( derive makeJSON ''GuardedAlt )
-- $( derive makeJSON ''Activation )
-- $( derive makeJSON ''Annotation )
-- $( derive makeJSON ''Rule )
-- $( derive makeJSON ''CallConv )
-- $( derive makeJSON ''RuleVar )
-- $( derive makeJSON ''Safety )
-- $( derive makeJSON ''Op )
-- $( derive makeJSON ''Assoc )
-- $( derive makeJSON ''Comment )
-- $( derive makeJSON ''SrcSpan )
-- $( derive makeJSON ''SrcSpanInfo )
--
--instance JSON Rational
--        where showJSON=JSRational False 











