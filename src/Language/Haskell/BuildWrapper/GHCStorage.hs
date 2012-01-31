{-# LANGUAGE CPP,OverloadedStrings,PatternGuards #-}
module Language.Haskell.BuildWrapper.GHCStorage where

import Language.Haskell.BuildWrapper.Base

import Data.Generics
import System.Directory
import System.FilePath

-- import qualified GHC.Paths
import PprTyThing
import GHC
import Outputable
import qualified OccName(occNameString)
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
#else 
import TypeRep ( Type(..), Pred(..) )
#endif


import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS
import Data.Aeson
import Data.Maybe
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import Data.Attoparsec.Number (Number(I))
import System.Time (ClockTime)


getInfoFile :: FilePath -> FilePath
getInfoFile fp= let 
        (dir,file)=splitFileName fp
        in combine dir ("." ++ (addExtension file ".bwinfo"))

clearInfo :: FilePath -> IO()
clearInfo fp =do
        let ghcInfoFile=getInfoFile fp
        removeFile ghcInfoFile

storeBuildFlagsInfo :: FilePath -> (BuildFlags,[BWNote]) -> IO()
storeBuildFlagsInfo fp bf=setStoredInfo fp "BuildFlags"  (toJSON bf)

storeGHCInfo :: FilePath -> TypecheckedSource -> IO()
storeGHCInfo fp tcs=setStoredInfo fp "AST"  (dataToJSON tcs)

readGHCInfo :: FilePath -> IO(Maybe Value)
readGHCInfo fp=do
       (Object hm)<-readStoredInfo fp
       return $ HM.lookup "AST" hm

readBuildFlagsInfo :: FilePath -> ClockTime -> IO (Maybe (BuildFlags,[BWNote]))
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

setStoredInfo :: FilePath -> T.Text -> Value -> IO()
setStoredInfo fp k v=do
        let ghcInfoFile=getInfoFile fp
        (Object hm)<-readStoredInfo fp
        let hm2=HM.insert k v hm
        BSS.writeFile ghcInfoFile $ BSS.concat $ BS.toChunks $ encode $ Object hm2

readStoredInfo :: FilePath -> IO Value
readStoredInfo fp=do
       let ghcInfoFile=getInfoFile fp
       ex<-doesFileExist ghcInfoFile
       mv<-if ex
                then do
                       bs<-BSS.readFile ghcInfoFile
                       return $ decode' $ BS.fromChunks [bs]
                else return Nothing
       return $ fromMaybe (object []) mv

dataToJSON :: Data a =>a -> Value
dataToJSON  = 
  generic `ext1Q` list `extQ` string `extQ` fastString `extQ` srcSpan 
          `extQ` name `extQ` occName `extQ` modName `extQ` var `extQ` exprVar `extQ` dataCon
          `extQ` bagName `extQ` bagRdrName `extQ` bagVar `extQ` nameSet
          `extQ` postTcType `extQ` fixity
  where generic :: Data a => a -> Value
        generic t =arr $ gmapQ dataToJSON t
                -- object [(T.pack $ showConstr (toConstr t)) .= sub ] 
        string     = Data.Aeson.String . T.pack :: String -> Value
        fastString:: FastString -> Value
        fastString fs= object ["FastString" .= (T.pack $ show fs)] 
        list l     = arr $ map dataToJSON l
        arr a = let
                sub=filter (/= Null) $ a
                in case sub of
                     [] -> Null
                     [x] -> x
                     _ -> toJSON sub          
        name :: Name -> Value
        name  n     = object ((nameAndModule n) ++["GType" .=  (string $ "Name"),"HType".= (string $ (if isValOcc (nameOccName n) then "v" else "t"))])
        occName :: OccName -> Value
        occName o   = object ["OccName" .=  (string $ OccName.occNameString o),"HType".= (string $ (if isValOcc o then "v" else "t"))]
        modName  :: ModuleName -> Value
        modName m= object [ "Name" .=  (string $ showSDoc $ ppr m),"GType" .= (string "ModuleName"),"HType".= (string $ "m")]
        srcSpan :: SrcSpan -> Value
        srcSpan src 
                | isGoodSrcSpan src   = object[ "SrcSpan" .= toJSON [srcLoc $ srcSpanStart src, srcLoc $ srcSpanEnd src]] 
                | otherwise = Null
        srcLoc :: SrcLoc -> Value
        srcLoc sl 
                | isGoodSrcLoc  sl=object ["line" .= toJSON (srcLocLine sl),"column" .= toJSON (srcLocCol sl)]
                | otherwise = Null
        var :: Var -> Value
        var  v     = typedVar v (varType v)
        dataCon ::  DataCon -> Value
        dataCon  d  = object ((nameAndModule $ dataConName d) ++["GType" .=  (string $ "DataCon")])
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
        typedVar v t=object ((nameAndModule $ varName v)++ [
                                "GType" .= (string "Var")
                                ,"Type" .= (string $ showSDocUnqual $ pprTypeForUser True $ t)
                                ,"QType" .= (string $ showSDoc $ pprTypeForUser True $ t)
                                ,"HType".= (string $ (if isValOcc (nameOccName (Var.varName v)) then "v" else "t"))])

        nameSet = const $ Data.Aeson.String "{!NameSet placeholder here!}" :: NameSet -> Value
                
        postTcType  = const Null :: Type -> Value -- string . showSDoc . ppr 

        fixity  = const Null :: GHC.Fixity -> Value --simple "Fixity" . showSDoc . ppr 

        -- nameAndModule :: Name -> [Pair]
        nameAndModule n=let
                mn=maybe "" (showSDoc . ppr . moduleName) $ nameModule_maybe n
                na=showSDocUnqual $ ppr n
                in ["Module" .= (string mn), "Name" .= (string na)]

debugToJSON :: Data a =>a -> IO()
debugToJSON = BS.putStrLn . encode . dataToJSON

debugFindInJSON :: Data a => Int -> Int -> a -> IO()
debugFindInJSON l c a= do
        let v=dataToJSON a
        let mv=findInJSON (overlap l c) v
        case mv of
                Just rv->do
                        putStrLn "something found!"
                        BS.putStrLn $ encode rv
                Nothing->putStrLn "nothing found!"

type FindFunc=(Value -> Bool)


findInJSONFormatted :: Bool -> Bool -> Maybe Value -> String
findInJSONFormatted qual typed (Just (Object m)) | Just (String name)<-HM.lookup "Name" m=let
        tn=T.unpack name
        qn=if qual
                then 
                     let mo=maybe "" (\(String s)->(T.unpack s)++".") $ HM.lookup "Module" m
                     in mo ++ tn
                else tn
        in if typed then
                        let mt=HM.lookup (if qual then "QType" else "Type") m
                        in case mt of
                                Just (String t)->qn ++ " :: " ++ (T.unpack t)
                                _ -> tn
                else
                       let mt=HM.lookup "HType" m
                       in case mt of
                                Just (String t)->qn ++ " " ++ (T.unpack t)
                                _ -> tn
findInJSONFormatted _ _ _="no info"

findInJSON :: FindFunc -> Value -> Maybe Value
findInJSON f (Array arr) | not $ V.null arr=let
        v1=arr V.! 0
        in if f v1 && V.length arr==2
           then 
                let
                        mv=findInJSON f $ arr V.! 1
                in case mv of
                        Just rv-> Just rv
                        Nothing -> Just $ arr V.! 1
           else
                let rvs=catMaybes $ V.toList $ fmap (findInJSON f) arr
                in case rvs of
                        (x:_)->Just x
                        []->Nothing
findInJSON f (Object obj)=let rvs=mapMaybe (findInJSON f) $ HM.elems obj
                in case rvs of
                        (x:_)->Just x
                        []->Nothing
findInJSON _ _= Nothing

overlap :: Int -> Int -> Value -> Bool
overlap l c (Object m) | 
        Just pos<-HM.lookup "SrcSpan" m,
        Just (l1,c1,l2,c2)<-extractSourceSpan pos=l1<=l && c1<=c && l2>=l && c2>=c
overlap _ _ _=False

extractSourceSpan :: Value -> Maybe (Int,Int,Int,Int)
extractSourceSpan (Array arr) | V.length arr==2 = do
        let v1=arr V.! 0
        let v2=arr V.! 1
        (l1,c1)<-extractSourceLoc v1
        (l2,c2)<-extractSourceLoc v2
        return (l1,c1,l2,c2)
extractSourceSpan _ =Nothing

extractSourceLoc :: Value -> Maybe (Int,Int)
extractSourceLoc (Object m) | 
        Just (Number(I l))<-HM.lookup "line" m,
        Just (Number(I c))<-HM.lookup "column" m=Just (fromIntegral l,fromIntegral c)  
extractSourceLoc _ = Nothing       


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
  in  Just $ (reduce_type $ unwrap wr (varType ident),ident)
-- All other search results produce no type information
typeOfExpr _ = Nothing

-- | Reduce a top-level type application if possible.  That is, we perform the
-- following simplification step:
-- @
--     (forall v . t) t'   ==>   t [t'/v]
-- @
-- where @[t'/v]@ is the substitution of @t'@ for @v@.
--
reduce_type :: Type -> Type
reduce_type (AppTy (ForAllTy tv b) t) =
    reduce_type (subst_type tv t b)
reduce_type t = t

subst_type :: TyVar -> Type -> Type -> Type
subst_type v t' t0 = go t0
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
      PredTy pt     -> PredTy (go_pt pt) 
      
   -- XXX: this is probably not right
    go_pt (ClassP c ts)  = ClassP c (map go ts)
    go_pt (IParam i t)   = IParam i (go t)
    go_pt (EqPred t1 t2) = EqPred (go t1) (go t2)
    
    
--haddockType  :: SearchResult a -> String
--haddockType (FoundName n)
--        | isValOcc (nameOccName n)="v"
--        | otherwise= "t"
--haddockType (FoundId i)
--        | isValOcc (nameOccName (Var.varName i))="v"
--        | otherwise= "t"
--haddockType (FoundModule _)="m"
--haddockType _="t"    
    