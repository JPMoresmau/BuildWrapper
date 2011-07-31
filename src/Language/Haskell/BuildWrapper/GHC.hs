{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeSynonymInstances #-}
module Language.Haskell.BuildWrapper.GHC where

import Control.Monad.State

import Text.JSON
import Data.DeriveTH
import Data.Derive.JSON
import Data.Generics hiding (Fixity)
import qualified Data.Map as M (insert)
import Data.Text hiding (map,head)
import Data.Vector (fromList)

import Bag
import BasicTypes
import GHC
import GHC.Paths ( libdir )
import Outputable
import FastString (FastString,unpackFS)
import ForeignCall
import UniqFM
import UniqSet
import OccName
import Type
import Unique
import Var

getGHCAST :: FilePath -> String -> [String] -> IO JSValue
getGHCAST fp mod options=do
    let lflags=map noLoc options
    (_leftovers, _) <- parseStaticFlags lflags
    (p,t)<-runGhc (Just libdir) $ do
        flg <- getSessionDynFlags
        (flg', _, _) <- parseDynamicFlags flg _leftovers
        setSessionDynFlags flg' { ghcLink = NoLink, ghcMode = OneShot }
        addTarget Target { targetId = TargetFile fp Nothing, targetAllowObjCode = False, targetContents = Nothing }
        load LoadAllTargets
        modSum <- getModSummary $ mkModuleName mod
        p <- parseModule modSum
        --return $ showSDocDump $ ppr $ pm_mod_summary p
        t <- typecheckModule p
        return (p,t)
        --return $ showSDocDump $ ppr $ pm_parsed_source p
        --return $ showSDocDump $ ppr $ tm_typechecked_source t
    return  $ makeObj  [("parse" , (showJSON $ tm_typechecked_source t))]
        
instance JSON SrcLoc
        where showJSON src 
                | isGoodSrcLoc src =makeObj [((unpackFS $ srcLocFile src) , (JSArray  [showJSON $ srcLocLine src,showJSON $ srcLocCol src])) ]
                | otherwise = JSNull
        
instance JSON SrcSpan
        where showJSON src 
                | isGoodSrcSpan src=makeObj [((unpackFS $ srcSpanFile src) , (JSArray  [showJSON $ srcSpanStartLine src,showJSON $ srcSpanStartCol src,showJSON $ srcSpanEndLine src,showJSON $ srcSpanEndCol src])) ]
                | otherwise = JSNull     
  
instance JSON FastString
        where showJSON=JSString . toJSString . unpackFS
  
instance JSON ModuleName
        where showJSON=JSString . toJSString . moduleNameString
        
instance JSON OccName
        where showJSON=JSString . toJSString . showSDocDump . ppr  

instance JSON Type
        where showJSON =JSString . toJSString . showSDocDump . ppr
  
instance JSON DataCon
        where showJSON=JSString . toJSString . showSDocDump . ppr  

instance JSON Unique
        where showJSON=JSString . toJSString . showSDocDump . ppr  

instance JSON Name
        where showJSON=JSString . toJSString . showSDocDump . ppr  

instance JSON EvBindsVar
        where showJSON=JSString . toJSString . showSDocDump . ppr  

instance JSON PackageId
        where showJSON=JSString . toJSString . showSDocDump . ppr  
        
instance (JSON a)=> JSON (Bag a)
        where showJSON=JSArray . map showJSON . bagToList

instance (JSON a)=> JSON (UniqSet  a)
        where showJSON=JSArray . map showJSON . uniqSetToList 

instance JSON Rational
        where showJSON=JSRational False 

instance JSON Var
        where showJSON v=makeObj [("Name",showJSON $ Var.varName v),("Unique",showJSON $ varUnique v),("Type",showJSON $ varType v)] 

instance JSON RdrName
        where 
                showJSON (Unqual on) = JSArray [showJSON on]
                showJSON (Qual mn on)  = JSArray [showJSON mn,showJSON on]  
  
instance(JSON a)=> JSON (Located a)
        where showJSON (L s o)=case showJSON o of
                JSObject o->let
                        ass=fromJSObject o
                        in JSObject $ toJSObject (("Loc",(showJSON s)):ass)
                JSNull -> JSNull
                v->makeObj [("Loc",(showJSON s)),("Object" ,v)]  
  
$( derive makeJSON ''HsModule )
$( derive makeJSON ''ImportDecl )      
$( derive makeJSON ''HsDocString )   
$( derive makeJSON ''HsDecl)   
$( derive makeJSON ''WarningTxt)   
      

$( derive makeJSON ''InstDecl )
-- $( derive makeJSON ''HsBind )
$( derive makeJSON ''HsBindLR )
$( derive makeJSON ''DefaultDecl )
$( derive makeJSON ''WarnDecl )
$( derive makeJSON ''RuleDecl )
$( derive makeJSON ''DocDecl )
$( derive makeJSON ''HsQuasiQuote )
$( derive makeJSON ''SpliceDecl )
$( derive makeJSON ''AnnDecl )
$( derive makeJSON ''ForeignDecl )
$( derive makeJSON ''Sig )
$( derive makeJSON ''DerivDecl )
$( derive makeJSON ''TyClDecl )
      
$( derive makeJSON ''NewOrData )
$( derive makeJSON ''HsTyVarBndr )
$( derive makeJSON ''HsPred )
$( derive makeJSON ''HsType )
$( derive makeJSON ''ConDecl )
$( derive makeJSON ''FamilyFlavour )
$( derive makeJSON ''ResType )
$( derive makeJSON ''HsConDetails )
$( derive makeJSON ''HsExplicitFlag )
$( derive makeJSON ''ConDeclField )
$( derive makeJSON ''Boxity )
$( derive makeJSON ''HsSplice )
$( derive makeJSON ''HsBang )
$( derive makeJSON ''IPName )
$( derive makeJSON ''HsExpr )
  
$( derive makeJSON ''HsLit)
$( derive makeJSON ''MatchGroup)
$( derive makeJSON ''HsStmtContext)
$( derive makeJSON ''HsBracket)
$( derive makeJSON ''Pat)
$( derive makeJSON ''HsWrapper)
$( derive makeJSON ''HsCmdTop)
$( derive makeJSON ''Fixity)
$( derive makeJSON ''HsArrAppType)
$( derive makeJSON ''ArithSeqInfo)
$( derive makeJSON ''HsRecFields )
$( derive makeJSON ''HsRecField )
$( derive makeJSON ''StmtLR )
$( derive makeJSON ''HsLocalBindsLR )
$( derive makeJSON ''HsTupArg )
$( derive makeJSON ''HsOverLit )
$( derive makeJSON ''OverLitVal )  
$( derive makeJSON ''HsValBindsLR )
$( derive makeJSON ''HsIPBinds )
$( derive makeJSON ''IPBind ) 
$( derive makeJSON ''FixitySig ) 
$( derive makeJSON ''InlinePragma ) 
$( derive makeJSON ''Match ) 
$( derive makeJSON ''Activation )
$( derive makeJSON ''RuleMatchInfo )
$( derive makeJSON ''InlineSpec )
$( derive makeJSON ''RecFlag )
$( derive makeJSON ''GRHSs )
$( derive makeJSON ''GRHS )
$( derive makeJSON ''FixityDirection )
$( derive makeJSON ''EvTerm )
$( derive makeJSON ''TcEvBinds )
$( derive makeJSON ''ForeignExport )
$( derive makeJSON ''ForeignImport )
$( derive makeJSON ''HsMatchContext )
$( derive makeJSON ''HsGroup )
$( derive makeJSON ''CExportSpec )
$( derive makeJSON ''CImportSpec )
$( derive makeJSON ''CCallConv )
$( derive makeJSON ''CCallTarget )
$( derive makeJSON ''EvBind )
$( derive makeJSON ''Safety )
$( derive makeJSON ''AnnProvenance )
$( derive makeJSON ''RuleBndr )
$( derive makeJSON ''TcSpecPrags )
$( derive makeJSON ''TcSpecPrag )

instance (JSON a)=> JSON (IE a)
        where
            showJSON= showJSON . ieName 
      
 {--
         p <- parseModule modSum
        t <- typecheckModule p
        d <- desugarModule t
        l <- loadModule d
        n <- getNamesInScope
        c <- return $ coreModule d
 
        g <- getModuleGraph
        mapM showModule g     
        return $ (parsedSource d,"/n-----/n",  typecheckedSource d)
        --} 
        
{--
instance ToJSON SrcLoc
        where toJSON src 
                | isGoodSrcLoc src =object [(pack $ unpackFS $ srcLocFile src) .= (Array $ fromList [toJSON $ srcLocLine src,toJSON $ srcLocCol src]) ]
                | otherwise = Null
        
instance ToJSON SrcSpan
        where toJSON src 
                | isGoodSrcSpan src=object [(pack $ unpackFS $ srcSpanFile src) .= (Array $ fromList [toJSON $ srcSpanStartLine src,toJSON $ srcSpanStartCol src,toJSON $ srcSpanEndLine src,toJSON $ srcSpanEndCol src]) ]
                | otherwise = Null
                        
instance(ToJSON a)=> ToJSON (Located a)
        where toJSON (L s o)=case toJSON o of
                Object o->Object (M.insert "Loc" (toJSON s) o)
                Null -> Null
                v->object ["Loc" .= (toJSON s),"Object" .= v]

instance (ToJSON a, OutputableBndr a)=> ToJSON (HsModule a)
        where toJSON hsm=object ["ModName" .= (toJSON $ hsmodName hsm),
                "Exports" .= (toJSON $ hsmodExports hsm),
                "Imports" .= (toJSON $ hsmodImports hsm),
                "Decls" .= (toJSON $  hsmodDecls hsm)
                ]

instance ToJSON FastString
        where toJSON=toJSON . pack . unpackFS
       
instance ToJSON ModuleName
        where toJSON=toJSON . moduleNameString
        
instance ToJSON OccName
        where toJSON=toJSON . showSDocDump . ppr
        
instance ToJSON RdrName
        where 
                toJSON (Unqual on) = Array $ fromList [toJSON on]
                toJSON (Qual mn on)  = Array $ fromList [toJSON mn,toJSON on]

instance (ToJSON a)=> ToJSON (IE a)
        where
            toJSON= toJSON . ieName --}
            {--toJSON (IEVar name)= object ["IEVar" .= toJSON name]      
            toJSON (IEThingAbs name)=object ["IEThingAbs" .= toJSON name]      
            toJSON (IEThingAll name)= object ["IEThingAll" .= toJSON name]      
            toJSON (IEThingWith name ns)= object ["IEVar" .= toJSON name]      
            toJSON (IEModuleContents mn)= object ["IEModuleContents" .= toJSON name]      
            toJSON (IEGroup i hds)= object ["IEVar" .= toJSON hds]      
            toJSON (IEDoc hds)=  object ["IEDoc" .= toJSON hds]      
            toJSON (IEDocNamed s)= object ["IEDocNamed" .= toJSON s]      --}
{-- 
instance (ToJSON a)=> ToJSON (ImportDecl a)
        where
            toJSON imd=object ["ModName" .= toJSON (ideclName imd),
                "PackageQualified" .= toJSON (ideclPkgQual imd),
                "Source" .= toJSON (ideclSource imd),
                "Qualified" .= toJSON (ideclQualified imd),
                "As" .= toJSON (ideclAs imd),
                "Hiding" .= toJSON (ideclHiding imd)
                ]

instance (ToJSON a, OutputableBndr a)=> ToJSON (HsDecl a)
        where toJSON =toJSON . showSDocDump . ppr
        
        --}