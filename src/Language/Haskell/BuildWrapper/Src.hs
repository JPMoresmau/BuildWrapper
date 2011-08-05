{-# LANGUAGE TemplateHaskell,TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Haskell.BuildWrapper.Src where

import Language.Haskell.BuildWrapper.Base

import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.SrcLoc

import Data.List
import Data.Maybe

import Text.JSON
import Data.DeriveTH
import Data.Derive.JSON

getHSEAST :: FilePath -> String -> [String] -> IO (ParseResult (Module SrcSpanInfo, [Comment]))
getHSEAST fp mod options=do
        let exts=map classifyExtension options
        let mode=defaultParseMode {extensions=exts,ignoreLinePragmas=False} 
        parseFileWithComments mode fp
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
                declOutline _ = []
                qualConDeclOutline :: QualConDecl SrcSpanInfo-> OutlineDef
                qualConDeclOutline (QualConDecl l _ _ con)=let
                        (n,defs)=conDecl con
                        in OutlineDef n [Constructor] (makeSpan l) defs
                declOutlineInClass :: Decl SrcSpanInfo -> [OutlineDef]
                declOutlineInClass (TypeSig l ns _)=map (\n->OutlineDef (nameDecl n) [Function] (makeSpan l) []) ns
                declOutlineInClass o=declOutline o
                headDecl :: DeclHead  a -> String
                headDecl (DHead _ n  _)=nameDecl n
                headDecl (DHInfix _ _ n _)=nameDecl n
                headDecl (DHParen _ h)=headDecl h
                nameDecl :: Name a -> String
                nameDecl (Ident _ s)=s
                nameDecl (Symbol _ s)=s
                typeDecl :: Type a -> String
                typeDecl (TyForall _ _ _ t)=typeDecl t
                typeDecl (TyVar _ n )=nameDecl n
                typeDecl (TyCon _ qn )=qnameDecl qn
                typeDecl (TyList _ t )="["++(typeDecl t)++"]"
                typeDecl (TyParen _ t )=typeDecl t
                typeDecl (TyApp _ t1 t2)=(typeDecl t1) ++ " "++(typeDecl t2)
                typeDecl _ = ""
                qnameDecl :: QName a -> String
                qnameDecl (Qual _ _ n)=nameDecl n
                qnameDecl (UnQual _ n)=nameDecl n
                qnameDecl _ =""
                matchDecl :: Match a -> String
                matchDecl (Match _ n _ _ _)=nameDecl n     
                matchDecl (InfixMatch _ _ n _ _ _)=nameDecl n    
                iheadDecl :: InstHead a -> String
                iheadDecl (IHead _ qn ts)= (qnameDecl qn) ++ " " ++ (intercalate " " (map typeDecl ts))
                iheadDecl (IHInfix _ t1 qn t2)= (typeDecl t1) ++ " "++ (qnameDecl qn) ++ " " ++ (typeDecl t2) 
                iheadDecl (IHParen _ i)=iheadDecl i
                conDecl :: ConDecl SrcSpanInfo -> (String,[OutlineDef])
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

getHSEOutline _ = []

 
makeSpan :: SrcSpanInfo -> InFileSpan
makeSpan si=let
        sis=srcInfoSpan si
        (sl,sc)=srcSpanStart sis
        (el,ec)=srcSpanEnd sis      
        in   InFileSpan (InFileLoc sl sc) (InFileLoc el ec)
        
$( derive makeJSON ''ParseResult )
$( derive makeJSON ''Module )
$( derive makeJSON ''ModuleHead )
$( derive makeJSON ''ExportSpecList )
-- $( derive makeJSON ''SrcLoc )

instance JSON SrcLoc
        where showJSON src = JSArray  [showJSON $ srcLine src,showJSON $ srcColumn src]

$( derive makeJSON ''ModulePragma )
$( derive makeJSON ''WarningText )
$( derive makeJSON ''ExportSpec )
$( derive makeJSON ''ImportDecl )
$( derive makeJSON ''ImportSpecList )
$( derive makeJSON ''ImportSpec )
$( derive makeJSON ''Decl )
$( derive makeJSON ''DeclHead )
$( derive makeJSON ''Deriving )
$( derive makeJSON ''Context )
$( derive makeJSON ''InstHead )
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
$( derive makeJSON ''FieldDecl )
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
$( derive makeJSON ''SrcSpanInfo )

instance JSON Rational
        where showJSON=JSRational False 











