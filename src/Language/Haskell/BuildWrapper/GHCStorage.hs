{-# LANGUAGE CPP,OverloadedStrings,PatternGuards #-}
-- |
-- Module      : Language.Haskell.BuildWrapper.GHCStorage
-- Author      : JP Moresmau
-- Copyright   : (c) JP Moresmau 2012
-- License     : BSD3
-- 
-- Maintainer  : jpmoresmau@gmail.com
-- Stability   : beta
-- Portability : portable
-- 
-- Store to disk in JSON format the results of the GHC AST build, and the build flags
-- this helps us with performance (we only call GHC when the file has changed, not everytime we want to find what's at a given source point
module Language.Haskell.BuildWrapper.GHCStorage where

import Language.Haskell.BuildWrapper.Base

import Data.Generics
import System.Directory
import System.FilePath

import PprTyThing
import GHC
import Outputable
import Bag(Bag,bagToList)
import Var(Var,varType,varName)
import FastString(FastString)
import NameSet(NameSet)
import Name hiding (varName)
import DataCon (dataConName)
#if __GLASGOW_HASKELL__ < 700
import GHC.SYB.Instances
#endif 

#if __GLASGOW_HASKELL__ < 702
import TypeRep ( Type(..), PredType(..) )
#elif __GLASGOW_HASKELL__ < 704
import TypeRep ( Type(..), Pred(..) )
#else
import TypeRep ( Type(..) )
#endif

#if __GLASGOW_HASKELL__ >= 704
import TcEvidence
#endif

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC (putStrLn)
import qualified Data.ByteString as BSS
import Data.Aeson
import Data.Maybe
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as DM
import qualified Data.Vector as V
import Data.Attoparsec.Number (Number(I))
import System.Time (ClockTime)
import Type (splitFunTys)
import Unique (getUnique)
import Data.List (sortBy)
import GHC.SYB.Utils (Stage(..), showData)


-- | get the file storing the information for the given source file
getInfoFile :: FilePath -- ^ the source file
        -> FilePath
getInfoFile fp= let 
        (dir,file)=splitFileName fp
        in combine dir ('.' : addExtension file ".bwinfo")

-- | get the file storing the information for the given source file
getUsageFile :: FilePath -- ^ the source file
        -> FilePath
getUsageFile fp= let 
        (dir,file)=splitFileName fp
        in combine dir ('.' : addExtension file ".bwusage")

-- | remove the storage file
clearInfo :: FilePath -- ^ the source file
        -> IO()
clearInfo fp =do
        let ghcInfoFile=getInfoFile fp
        removeFile ghcInfoFile

-- | store the build flags
storeBuildFlagsInfo :: FilePath -- ^ the source file
        -> (BuildFlags,[BWNote]) -- ^  build flags and notes
        -> IO()
storeBuildFlagsInfo fp bf=setStoredInfo fp "BuildFlags"  (toJSON bf)

-- | generate the JSON from the typechecked module
-- this incorporates info from the renamed source with types annotations from the typechecked source
generateGHCInfo :: TypecheckedModule -> Value
generateGHCInfo tcm=let
        -- extract usages from typechecked source
        tcvals=extractUsages $ dataToJSON $ typecheckedSource tcm
        -- store objects with type annotations in a map keyed by module, name, line and column
        tcByNameLoc=foldr buildMap DM.empty tcvals
        -- extract usages from renamed source
        rnvals=extractUsages $ dataToJSON $ tm_renamed_source tcm
        -- add type information on objects
        typedVals=map (addType tcByNameLoc) rnvals
        in (Array $ V.fromList typedVals)
        where 
                buildMap v@(Object m) dm | 
                        Just pos<-HM.lookup "Pos" m,
                        Success ifs <- fromJSON pos,
                        Just (String s)<-HM.lookup "Name" m,
                        Just (String mo)<-HM.lookup "Module" m,
                        Just _<-HM.lookup "QType" m,
                        Just _<-HM.lookup "Type" m,
                        Just "v"<-HM.lookup "HType" m,
                        Just _<-HM.lookup "GType" m=
                                DM.insert (mo,s,ifl_line $ ifs_start ifs,0) v $ -- add column 0 for some cases where the spans are funny 
                                DM.insert (mo,s,ifl_line $ ifs_start ifs,ifl_column $ ifs_start ifs) v dm
                buildMap _ dm=dm
                addType dm v@(Object m1) |
                        Just pos<-HM.lookup "Pos" m1,
                        Success ifs <- fromJSON pos,
                        Just (String s)<-HM.lookup "Name" m1,
                        Just (String mo)<-HM.lookup "Module" m1,
                        Just "v"<-HM.lookup "HType" m1=let
                                mv=DM.lookup (mo,s,ifl_line $ ifs_start ifs,ifl_column $ ifs_start ifs) dm
                                mv2=case mv of
                                        Nothing -> DM.lookup (mo,s,ifl_line $ ifs_start ifs,0) dm
                                        a->a
                                in case mv2 of
                                        Just (Object m2) |
                                                 Just qt<-HM.lookup "QType" m2,
                                                 Just t<-HM.lookup "Type" m2,
                                                 Just gt<-HM.lookup "GType" m2 -> Object (HM.insert "QType" qt $
                                                        HM.insert "Type" t $
                                                        HM.insert "GType" gt
                                                        m1) 
                                        _ -> v
                addType _ v=v

-- | store the GHC generated AST
storeGHCInfo :: FilePath -- ^ the source file
        -> TypecheckedModule -- ^ the GHC AST
        -> IO()
storeGHCInfo fp tcm=  -- do
--        putStrLn $ showData TypeChecker 4 $ typecheckedSource tcm
--        let tcvals=extractUsages $ dataToJSON $ typecheckedSource tcm
--        BSC.putStrLn $ encode $ Array $ V.fromList tcvals
--        let rnvals=extractUsages $ dataToJSON $ tm_renamed_source tcm 
--        BSC.putStrLn $ encode $ Array $ V.fromList rnvals
        setStoredInfo fp "AST" $ generateGHCInfo tcm
        
                
-- | read the GHC AST as a JSON value
readGHCInfo :: FilePath -- ^ the source file
        -> IO(Maybe Value)
readGHCInfo fp=do
       (Object hm)<-readStoredInfo fp
       return $ HM.lookup "AST" hm

-- | read the build flags and notes as a JSON value
readBuildFlagsInfo :: FilePath -- ^ the source file
        -> ClockTime -- ^ time the cabal file was changed. If the file was changed after the storage file, we return Nothing
        -> IO (Maybe (BuildFlags,[BWNote]))
readBuildFlagsInfo fp ct=do
       let ghcInfoFile=getInfoFile fp
       ex<-doesFileExist ghcInfoFile
       if ex then do
               ctF<-getModificationTime ghcInfoFile
               if ctF>ct 
                   then do
                       (Object hm)<-readStoredInfo fp
                       return $ maybe Nothing (\x-> case fromJSON x of
                                Success a->Just a
                                Error _->Nothing) $ HM.lookup "BuildFlags" hm
                   else return Nothing
             else return Nothing

-- | utility function to store the given value under the given key 
setStoredInfo :: FilePath -- ^ the source file
        -> T.Text -- ^ the key under which the value will be put 
        -> Value -- ^ the value
        -> IO()
setStoredInfo fp k v=do
        let ghcInfoFile=getInfoFile fp
        (Object hm)<-readStoredInfo fp
        let hm2=HM.insert k v hm
        BSS.writeFile ghcInfoFile $ BSS.concat $ BS.toChunks $ encode $ Object hm2

-- | read the top JSON value containing all the information
readStoredInfo :: FilePath  -- ^ the source file
        -> IO Value
readStoredInfo fp=do
       let ghcInfoFile=getInfoFile fp
       ex<-doesFileExist ghcInfoFile
       mv<-if ex
                then do
                       bs<-BSS.readFile ghcInfoFile
                       return $ decode' $ BS.fromChunks [bs]
                else return Nothing
       return $ fromMaybe (object []) mv

-- | write the usage info file
setUsageInfo :: FilePath -- ^ the source file
        -> Value -- ^ the value
        -> IO()
setUsageInfo fp v=do
        let usageFile=getUsageFile fp
        BSS.writeFile usageFile $ BSS.concat $ BS.toChunks $ encode v

-- | read the usage info file
getUsageInfo :: FilePath -- ^ the source file
        -> IO Value
getUsageInfo fp=do
        let usageFile=getUsageFile fp
        ex<-doesFileExist usageFile
        mv<-if ex
                then do
                       bs<-BSS.readFile usageFile
                       return $ decode' $ BS.fromChunks [bs]
                else return Nothing
        return $ fromMaybe (object []) mv
        

-- | convert a Data into a JSON value, with specific treatment for interesting GHC AST objects, and avoiding the holes
dataToJSON :: Data a =>a -> Value
dataToJSON  = 
  generic `ext1Q` list `extQ` string `extQ` fastString `extQ` srcSpanToJSON 
          `extQ` name `extQ` occName `extQ` modName `extQ` var `extQ` exprVar `extQ` dataCon
          `extQ` bagName `extQ` bagRdrName `extQ` bagVar `extQ` nameSet
          `extQ` postTcType `extQ` fixity  `extQ` hsBind
  where generic :: Data a => a -> Value
        generic t =arr $ gmapQ dataToJSON t
                -- object [(T.pack $ showConstr (toConstr t)) .= sub ] 
        string     = Data.Aeson.String . T.pack :: String -> Value
        fastString:: FastString -> Value
        fastString fs= object ["FastString" .= T.pack (show fs)] 
        list l     = arr $ map dataToJSON l
        arr a = let
                sub=filter (/= Null) a
                in case sub of
                     [] -> Null
                     [x] -> x
                     _ -> toJSON sub          
        name :: Name -> Value
        name  n     = object (nameAndModule n ++["GType" .= string "Name","HType".= string (if isValOcc (nameOccName n) then "v" else "t")])
        occName :: OccName -> Value
        occName o   = name (mkSystemName (getUnique o) o) 
                --object ["Name" .= string (OccName.occNameString o),"HType" .= string (if isValOcc o then "v" else "t")]
        modName  :: ModuleName -> Value
        modName m= object [ "Name" .= string (showSDoc $ ppr m),"GType" .= string "ModuleName","HType" .= string "m"]

        var :: Var -> Value
        var  v     = typedVar v (varType v)
        dataCon ::  DataCon -> Value
        dataCon  d  = let
                t=dataConUserType d
                in object (nameAndModule (dataConName d) ++ typeToJSON t ++ [
                        "GType" .= string "DataCon",
                        "HType" .=  string "v"])
--        simple:: T.Text -> String -> Value
--        simple nm v=object [nm .= T.pack v]
        simpleV:: T.Text -> Value -> Value
        simpleV nm v=object [nm .= v]
        bagRdrName:: Bag (Located (HsBind RdrName)) -> Value
        bagRdrName = simpleV "Bag(Located (HsBind RdrName))" . list . bagToList 
        bagName   :: Bag (Located (HsBind Name)) -> Value
        bagName    = simpleV "Bag(Located (HsBind Name))" . list . bagToList 
        bagVar    :: Bag (Located (HsBind Var)) -> Value
        bagVar     = simpleV "Bag(Located (HsBind Var))". list . bagToList 
        exprVar    :: HsExpr Var -> Value
        exprVar   ev  = let
                mt=typeOfExpr ev
                in case mt of
                        Just (t,v)-> typedVar v t
                        Nothing->generic ev
        typedVar :: Var -> Type -> Value
        typedVar v t=object (nameAndModule (varName v) ++ typeToJSON t ++
                ["GType" .= string "Var",
                "HType" .=  string (if isValOcc (nameOccName (Var.varName v)) then "v" else "t")])

        nameSet = const $ Data.Aeson.String "{!NameSet placeholder here!}" :: NameSet -> Value
                
        postTcType  = const Null :: Type -> Value -- string . showSDoc . ppr 

        fixity  = const Null :: GHC.Fixity -> Value --simple "Fixity" . showSDoc . ppr 

        typeToJSON :: Type -> [(T.Text,Value)]
        typeToJSON t =  -- let
                --appT=let (a,b)= splitAppTys t in (a:b)
                --allT2=concatMap typesInsideType appT
                -- allT=typesInsideType t
                -- allT2=allT ++ concatMap (\t2->let (a,b)= splitAppTys t2 in (a:b)) allT
                -- in
                ["Type" .= string (showSDocUnqual $ pprTypeForUser True t),
                "QType" .= string (showSDoc $ pprTypeForUser True t)]
              --  ,"AllTypes" .= (map string $ filter ("[]" /=) $ nubOrd $ map (showSDoc . withPprStyle (mkUserStyle ((\_ _ -> NameNotInScope2), const True) AllTheWay) . pprTypeForUser True) allT2)]
        hsBind :: HsBindLR Name Name -> Value
        hsBind (FunBind fid _ (MatchGroup matches _) _ _ _) =arr $ map (\m->arr [arr [dataToJSON $ getLoc m,dataToJSON $ unLoc fid],dataToJSON m]) matches
        hsBind a=generic a
        -- nameAndModule :: Name -> [Pair]
        nameAndModule n=let
                mm=nameModule_maybe n
                mn=maybe "" (showSDoc . ppr . moduleName) mm
                pkg=maybe "" (showSDoc . ppr . modulePackageId) mm
                na=showSDocUnqual $ ppr n
                in ["Module" .= string mn,"Package" .= string pkg, "Name" .= string na]

srcSpanToJSON :: SrcSpan -> Value
srcSpanToJSON src 
        | isGoodSrcSpan src   = object[ "SrcSpan" .= toJSON [srcLocToJSON $ srcSpanStart src, srcLocToJSON $ srcSpanEnd src]] 
        | otherwise = Null
#if __GLASGOW_HASKELL__ < 702   
srcLocToJSON :: SrcLoc -> Value
srcLocToJSON sl 
        | isGoodSrcLoc  sl=object ["line" .= toJSON (srcLocLine sl),"column" .= toJSON (srcLocCol sl)]
        | otherwise = Null
#else
srcLocToJSON :: SrcLoc -> Value
srcLocToJSON (RealSrcLoc sl)=object ["line" .= toJSON (srcLocLine sl),"column" .= toJSON (srcLocCol sl)]
srcLocToJSON _ = Null        
#endif


typesInsideType :: Type -> [Type]
typesInsideType t=let
         (f1,f2)=splitFunTys t
         in f2 : concatMap typesInsideType f1
        
-- | debug function: shows on standard output the JSON representation of the given data
debugToJSON :: Data a =>a -> IO()
debugToJSON = BSC.putStrLn . encode . dataToJSON

-- | debug searching thing at point in given data
debugFindInJSON :: Data a => Int -> Int -> a -> IO()
debugFindInJSON l c a= do
        let v=dataToJSON a
        let mv=findInJSON (overlap l c) v
        case mv of
                Just rv->do
                        putStrLn "something found!"
                        BSC.putStrLn $ encode rv
                Nothing->putStrLn "nothing found!"

-- | simple type for search function
type FindFunc= Value -> Bool

-- | find in JSON AST and return the string result
findInJSONFormatted :: Bool -- ^ should the output be qualified?
        -> Bool -- ^ should the output be fully typed?
        -> Maybe Value -- ^ result of search 
        -> String
findInJSONFormatted qual typed (Just (Object m)) | Just (String name)<-HM.lookup "Name" m=let
        tn=T.unpack name
        qn=if qual
                then 
                     let mo=maybe "" addDot $ HM.lookup "Module" m
                     in mo ++ tn
                else tn
        in if typed then
                        let mt=HM.lookup (if qual then "QType" else "Type") m
                        in case mt of
                                Just (String t)->qn ++ " :: " ++ T.unpack t
                                _ -> tn
                else
                       let mt=HM.lookup "HType" m
                       in case mt of
                                Just (String t)->qn ++ " " ++ T.unpack t
                                _ -> tn
        where 
                addDot :: Value -> String
                addDot (String s)=T.unpack s ++ "."
                addDot _=error "expected String value for Module key"
findInJSONFormatted _ _ _="no info"

findInJSONData :: Maybe Value -> Maybe ThingAtPoint
findInJSONData (Just o@(Object m)) | Just (String _)<-HM.lookup "Name" m=case fromJSON o of
        Success tap->tap
        Error _ -> Nothing 
findInJSONData _=Nothing

---- | find in JSON AST
--findInJSON :: FindFunc -- ^ the evaluation function  
--        -> Value -- ^ the root object containing the AST 
--        -> Maybe Value
--findInJSON f (Array arr) | not $ V.null arr=let
--        v1=arr V.! 0
--        in if f v1 && V.length arr==2 -- we have an array of two elements, the first one being a matching SrcSpan we go down the second element
--           then 
--                let
--                        mv=findInJSON f $ arr V.! 1
--                in case mv of
--                        Just rv-> Just rv -- found something underneath
--                        Nothing -> Just $ arr V.! 1 -- found nothing underneath, return second element of the array
--           else
--                let rvs=catMaybes $ V.toList $ fmap (findInJSON f) arr -- other case of arrays: check on each element
--                in case rvs of
--                        (x:_)->Just x -- return first match
--                        []->Nothing
--findInJSON f (Object obj)=let rvs=mapMaybe (findInJSON f) $ HM.elems obj -- in a complex object: check on contained elements
--                in case rvs of
--                        (x:_)->Just x
--                        []->Nothing
--findInJSON _ _= Nothing
--
---- | overlap function: find whatever is at the given line and column
--overlap :: Int  -- ^ line
--        -> Int -- ^ column
--        -> FindFunc
--overlap l c (Object m) | 
--        Just pos<-HM.lookup "SrcSpan" m,
--        Just (l1,c1,l2,c2)<-extractSourceSpan pos=l1<=l && c1<=c && l2>=l && c2>=c
--overlap _ _ _=False

-- | find in JSON AST
findInJSON :: FindFunc -- ^ the evaluation function  
        -> Value -- ^ the root object containing the AST 
        -> Maybe Value
findInJSON f (Array vals)=listToMaybe $ sortBy lastPos $ filter f $ V.toList vals
findInJSON _ _=Nothing

-- | sort Value by position, descending        
lastPos :: Value -> Value -> Ordering
lastPos (Object m1) (Object m2) |
       Just pos1<-HM.lookup "Pos" m1,
       Success ifs1 <- fromJSON pos1,
       Just pos2<-HM.lookup "Pos" m2,
       Success ifs2 <- fromJSON pos2 =let
                c1=compare (ifl_line $ ifs_start ifs2) (ifl_line $ ifs_start ifs1)
                in case c1 of
                        EQ -> compare (ifl_column $ ifs_start ifs2) (ifl_column $ ifs_start ifs1)
                        a -> a
lastPos _ _=EQ      

-- | overlap function: find whatever is at the given line and column
overlap :: Int  -- ^ line
        -> Int -- ^ column
        -> FindFunc
overlap l c (Object m) | 
        Just pos<-HM.lookup "Pos" m,
        Success ifs <- fromJSON pos=iflOverlap ifs (InFileLoc l c)
overlap _ _ _=False

extractUsages :: Value -- ^ the root object containing the AST 
        -> [Value]
extractUsages (Array arr) | not $ V.null arr=let
        v1=arr V.! 0
        msrc=extractSource v1
        in if isJust msrc && V.length arr==2 -- we have an array of two elements, the first one being a matching SrcSpan we go down the second element
           then extractName v1 (arr V.! 1) ++ extractUsages (arr V.! 1)
           else concat $ V.toList $ fmap extractUsages arr -- other case of arrays: check on each element
extractUsages (Object obj)=concatMap extractUsages $ HM.elems obj -- in a complex object: check on contained elements
        -- (extractName o) : 
extractUsages  _= []

extractName :: Value -> Value -> [Value]
extractName src (Object m) |
        Just ifl<-extractSource src,
        Just (String s)<-HM.lookup "Name" m,
        Just (String mo)<-HM.lookup "Module" m,
        not $ T.null mo,
        Just (String p)<-HM.lookup "Package" m,
        mqt<-HM.lookup "QType" m,
        mst<-HM.lookup "Type" m,
        mgt<-HM.lookup "GType" m,    
        Just (String t)<-HM.lookup "HType" m,
        at<-fromMaybe (Array V.empty) $ HM.lookup "AllTypes" m
                =[object ["Name" .= s,"Module" .= mo,"Package" .= p,"HType" .= t,"AllTypes" .= at, "Pos" .= toJSON ifl, "QType" .= mqt, "Type" .= mst,"GType" .= mgt]]
extractName _ _=[]

extractSource :: Value ->  Maybe InFileSpan
extractSource (Object m) | 
        Just pos<-HM.lookup "SrcSpan" m,
        Just (sl,sc,el,ec)<-extractSourceSpan pos=Just $ InFileSpan (InFileLoc sl sc) (InFileLoc el ec)
extractSource _=Nothing

-- | extract the source span from JSON
extractSourceSpan :: Value -> Maybe (Int,Int,Int,Int)
extractSourceSpan (Array arr) | V.length arr==2 = do
        let v1=arr V.! 0
        let v2=arr V.! 1
        (l1,c1)<-extractSourceLoc v1
        (l2,c2)<-extractSourceLoc v2
        return (l1,c1,l2,c2)
extractSourceSpan _ =Nothing

-- | extract the source location from JSON
extractSourceLoc :: Value -> Maybe (Int,Int)
extractSourceLoc (Object m) | 
        Just (Number(I l))<-HM.lookup "line" m,
        Just (Number(I c))<-HM.lookup "column" m=Just (fromIntegral l,fromIntegral c)  
extractSourceLoc _ = Nothing       

-- | resolve the type of an expression
typeOfExpr :: HsExpr Var -> Maybe (Type,Var)
typeOfExpr (HsWrap wr (HsVar ident)) =
  let -- Unwrap a HsWrapper and its associated type
      unwrap WpHole t            = t
      unwrap (WpCompose w1 w2) t = unwrap w1 (unwrap w2 t)
      unwrap (WpCast _) t        = t -- XXX: really?
      unwrap (WpTyApp t') t      = AppTy t t'
      unwrap (WpTyLam tv) t      = ForAllTy tv t
      -- do something else with coercion/dict vars?
#if __GLASGOW_HASKELL__ < 700
      unwrap (WpApp v) t         = AppTy t (TyVarTy v)
      unwrap (WpLam v) t         = ForAllTy v t
#else
      -- unwrap (WpEvApp v) t       = AppTy t (TyVarTy v)
      unwrap (WpEvLam v) t       = ForAllTy v t
      unwrap (WpEvApp _) t       = t
#endif
      unwrap (WpLet _) t       = t
#ifdef WPINLINE
      unwrap WpInline t          = t
#endif
  in  Just (reduceType $ unwrap wr (varType ident), ident)
-- All other search results produce no type information
typeOfExpr _ = Nothing

-- | Reduce a top-level type application if possible.  That is, we perform the
-- following simplification step:
-- @
--     (forall v . t) t'   ==>   t [t'/v]
-- @
-- where @[t'/v]@ is the substitution of @t'@ for @v@.
--
reduceType :: Type -> Type
reduceType (AppTy (ForAllTy tv b) t) =
    reduceType (substType tv t b)
reduceType t = t

substType :: TyVar -> Type -> Type -> Type
substType v t'  = go 
  where
    go t = case t of
      TyVarTy tv 
        | tv == v   -> t'
        | otherwise -> t
      AppTy t1 t2   -> AppTy (go t1) (go t2)
      TyConApp c ts -> TyConApp c (map go ts)
      FunTy t1 t2   -> FunTy (go t1) (go t2)
      ForAllTy v' bt 
        | v == v'   -> t
        | otherwise -> ForAllTy v' (go bt)
#if __GLASGOW_HASKELL__ < 704       
      PredTy pt     -> PredTy (go_pt pt) 
      
   -- XXX: this is probably not right
    go_pt (ClassP c ts)  = ClassP c (map go ts)
    go_pt (IParam i t)   = IParam i (go t)
    go_pt (EqPred t1 t2) = EqPred (go t1) (go t2)
#endif    
    