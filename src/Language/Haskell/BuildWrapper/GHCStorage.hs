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
import qualified Data.Vector as V
import Data.Attoparsec.Number (Number(I))
import System.Time (ClockTime)
-- import GHC.SYB.Utils (showData, Stage(..))
import Type (splitFunTys, splitAppTys)
import Unique (getUnique)


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

-- | store the GHC generated AST
storeGHCInfo :: FilePath -- ^ the source file
        -> TypecheckedSource -- ^ the GHC AST
        -> IO()
storeGHCInfo fp tcs= -- do
        -- putStrLn $ showData TypeChecker 4 tcs
        setStoredInfo fp "AST"  (dataToJSON tcs)

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

setUsageInfo :: FilePath -- ^ the source file
        -> Value -- ^ the value
        -> IO()
setUsageInfo fp v=do
        let usageFile=getUsageFile fp
        BSS.writeFile usageFile $ BSS.concat $ BS.toChunks $ encode v

-- | convert a Data into a JSON value, with specific treatment for interesting GHC AST objects, and avoiding the holes
dataToJSON :: Data a =>a -> Value
dataToJSON  = 
  generic `ext1Q` list `extQ` string `extQ` fastString `extQ` srcSpan 
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
        srcSpan :: SrcSpan -> Value
        srcSpan src 
                | isGoodSrcSpan src   = object[ "SrcSpan" .= toJSON [srcLoc $ srcSpanStart src, srcLoc $ srcSpanEnd src]] 
                | otherwise = Null
#if __GLASGOW_HASKELL__ < 702   
        srcLoc :: SrcLoc -> Value
        srcLoc sl 
                | isGoodSrcLoc  sl=object ["line" .= toJSON (srcLocLine sl),"column" .= toJSON (srcLocCol sl)]
                | otherwise = Null
#else
        srcLoc :: SrcLoc -> Value
        srcLoc (RealSrcLoc sl)=object ["line" .= toJSON (srcLocLine sl),"column" .= toJSON (srcLocCol sl)]
        srcLoc _ = Null        
#endif
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

-- | find in JSON AST
findInJSON :: FindFunc -- ^ the evaluation function  
        -> Value -- ^ the root object containing the AST 
        -> Maybe Value
findInJSON f (Array arr) | not $ V.null arr=let
        v1=arr V.! 0
        in if f v1 && V.length arr==2 -- we have an array of two elements, the first one being a matching SrcSpan we go down the second element
           then 
                let
                        mv=findInJSON f $ arr V.! 1
                in case mv of
                        Just rv-> Just rv -- found something underneath
                        Nothing -> Just $ arr V.! 1 -- found nothing underneath, return second element of the array
           else
                let rvs=catMaybes $ V.toList $ fmap (findInJSON f) arr -- other case of arrays: check on each element
                in case rvs of
                        (x:_)->Just x -- return first match
                        []->Nothing
findInJSON f (Object obj)=let rvs=mapMaybe (findInJSON f) $ HM.elems obj -- in a complex object: check on contained elements
                in case rvs of
                        (x:_)->Just x
                        []->Nothing
findInJSON _ _= Nothing

-- | overlap function: find whatever is at the given line and column
overlap :: Int  -- ^ line
        -> Int -- ^ column
        -> FindFunc
overlap l c (Object m) | 
        Just pos<-HM.lookup "SrcSpan" m,
        Just (l1,c1,l2,c2)<-extractSourceSpan pos=l1<=l && c1<=c && l2>=l && c2>=c
overlap _ _ _=False

extractUsages :: Value -- ^ the root object containing the AST 
        -> [Maybe Value]
extractUsages (Array arr) | not $ V.null arr=let
        v1=arr V.! 0
        msrc=extractSource v1
        in if isJust msrc && V.length arr==2 -- we have an array of two elements, the first one being a matching SrcSpan we go down the second element
           then (extractName v1 $ arr V.! 1)  ++
                         (extractUsages $ arr V.! 1)
           else concat $ V.toList $ fmap extractUsages arr -- other case of arrays: check on each element
extractUsages (Object obj)=(concatMap extractUsages $ HM.elems obj) -- in a complex object: check on contained elements
        -- (extractName o) : 
extractUsages  _= []

extractName :: Value -> Value -> [Maybe Value]
extractName src (Object m) |
        Just (l1,c1,l2,c2)<-extractSource src,
        Just (String s)<-HM.lookup "Name" m,
        Just (String mo)<-HM.lookup "Module" m,
        not $ T.null mo,
        Just (String p)<-HM.lookup "Package" m,
        --Just (String qt)<-HM.lookup "QType" m, ,"QType" .= qt
        Just (String t)<-HM.lookup "HType" m,
        at<-fromMaybe (Array V.empty) $ HM.lookup "AllTypes" m
                =[Just $ object ["Name" .= s,"Module" .= mo,"Package" .= p,"HType" .= t,"AllTypes" .= at, "Pos" .= toJSON [l1,c1,l2,c2]]]
--extractName src (Array arr) | not $ V.null arr && isNothing (extractSource $ arr V.! 0)=concatMap (extractName src) $ V.toList arr
extractName _ _=[]

extractSource :: Value ->  Maybe (Int,Int,Int,Int)
extractSource (Object m) | 
        Just pos<-HM.lookup "SrcSpan" m=extractSourceSpan pos
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
substType v t' t0 = go t0
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
    