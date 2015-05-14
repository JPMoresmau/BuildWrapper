{-# LANGUAGE CPP,OverloadedStrings,PatternGuards,RankNTypes, ScopedTypeVariables #-}
-- |
-- Module      : Language.Haskell.BuildWrapper.GHCStorage
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

import qualified Data.ByteString.Lazy as BS
--import qualified Data.ByteString.Lazy.Char8 as BSC (putStrLn)
import qualified Data.ByteString as BSS
import Data.Aeson
import Data.Maybe
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as DM
import qualified Data.Vector as V
#if __GLASGOW_HASKELL__ < 706
import System.Time (ClockTime)
#else
import Data.Time.Clock (UTCTime)
#endif
#if __GLASGOW_HASKELL__ >= 708
import ConLike
#endif
import Type (splitFunTys)
import Unique (getUnique)
import Data.List (sortBy)
--import GHC.SYB.Utils (Stage(..), showData)
import qualified MonadUtils as GMU
#if __GLASGOW_HASKELL__ < 707
import TcRnTypes (tcg_type_env,tcg_rdr_env)
#endif
import qualified CoreUtils (exprType)
import Desugar (deSugarExpr)
import Control.Monad (liftM)
import Data.Aeson.Types (Pair)

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
generateGHCInfo :: DynFlags -> HscEnv -> TypecheckedModule -> IO Value
generateGHCInfo df env tcm=do
        -- extract usages from typechecked source
        tcvals<-liftM extractUsages $ dataToJSON df env tcm $ typecheckedSource tcm
        print tcvals
        -- store objects with type annotations in a map keyed by module, name, line and column
        let tcByNameLoc=foldr buildMap DM.empty tcvals
        -- print tcByNameLoc
        -- extract usages from renamed source
        rnvals<-liftM extractUsages $ dataToJSON df env tcm $ tm_renamed_source tcm
        -- print rnvals
        -- add type information on objects
        let typedVals=map (addType tcByNameLoc) rnvals
        return (Array $ V.fromList typedVals)
        where
                mn=T.pack $ showSD True df $ ppr $ moduleName $ ms_mod $ pm_mod_summary $ tm_parsed_module tcm
                buildMap v@(Object m) dm |
                        Just pos<-HM.lookup "Pos" m,
                        Success ifs <- fromJSON pos,
                        Just (String s)<-HM.lookup "Name" m,
                        Just (String mo)<-HM.lookup "Module" m,
                        Just _<-HM.lookup "QType" m,
                        Just _<-HM.lookup "Type" m,
                        Just "v"<-HM.lookup "HType" m,
                        Just _<-HM.lookup "GType" m=
                                DM.insert (mo,s,iflLine $ ifsStart ifs,0) v $ -- add column 0 for some cases where the spans are funny
                                DM.insert (mo,s,iflLine $ ifsStart ifs,iflColumn $ ifsStart ifs) v dm
                buildMap _ dm=dm
                addType dm v@(Object m1) |
                        Just pos<-HM.lookup "Pos" m1,
                        Success ifs <- fromJSON pos,
                        Just (String s)<-HM.lookup "Name" m1,
                        Just (String mo)<-HM.lookup "Module" m1,
                        Just "v"<-HM.lookup "HType" m1=let
                                mv=DM.lookup (mo,s,iflLine $ ifsStart ifs,iflColumn $ ifsStart ifs) dm
                                mv2=case mv of
                                        Nothing -> DM.lookup (mo,s,iflLine $ ifsStart ifs,0) dm
                                        a->a
                                mv3=if mn==mo && isNothing mv2
                                  then DM.lookup ("",s,iflLine $ ifsStart ifs,0) dm
                                  else mv2
                                in case mv3 of
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
storeGHCInfo ::
        DynFlags
        -> HscEnv
        -> FilePath -- ^ the source file
        -> TypecheckedModule -- ^ the GHC AST
        -> IO()
storeGHCInfo df env fp tcm =
--        putStrLn $ showData TypeChecker 4 $ typecheckedSource tcm
--        putStrLn "Typechecked"
--        BSC.putStrLn $ encode $ dataToJSON $ typecheckedSource tcm
--        putStrLn "Renamed"
--        BSC.putStrLn $ encode $ dataToJSON $ tm_renamed_source tcm
--        let tcvals=extractUsages $ dataToJSON $ typecheckedSource tcm
--        BSC.putStrLn $ encode $ Array $ V.fromList tcvals
--        let rnvals=extractUsages $ dataToJSON $ tm_renamed_source tcm
--        BSC.putStrLn $ encode $ Array $ V.fromList rnvals
        generateGHCInfo df env tcm >>= setStoredInfo fp "AST"


-- | read the GHC AST as a JSON value
readGHCInfo :: FilePath -- ^ the source file
        -> IO(Maybe Value)
readGHCInfo fp=do
       (Object hm)<-readStoredInfo fp
       return $ HM.lookup "AST" hm

-- | read the build flags and notes as a JSON value
readBuildFlagsInfo :: FilePath -- ^ the source file
#if __GLASGOW_HASKELL__ < 706
        -> ClockTime -- ^ time the cabal file was changed. If the file was changed after the storage file, we return Nothing
#else
        -> UTCTime
#endif
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
dataToJSON :: Data a => DynFlags -> HscEnv -> TypecheckedModule -> a -> IO Value
dataToJSON  df env tcm=
  generic `ext1Q` list `extQ` (return . string) `extQ` (return . fastString) `extQ` (return . srcSpanToJSON)
          `extQ` (return . name) `extQ` (return . ocName) `extQ` (return . modName) `extQ` var `extQ` exprVar `extQ` (return . dataCon)
#if __GLASGOW_HASKELL__ >= 708
          `extQ` (return . rDataCon)
#endif
          `extQ` bagName `extQ` bagRdrName `extQ` bagVar `extQ` (return . nameSet)
          `extQ` (return . postTcType) `extQ` (return . fixity)  `extQ` hsBind
  where generic :: Data a => a -> IO Value
        generic t=do
                let sub=gmapQ (dataToJSON df env tcm) t
                liftM arr $ sequence sub
                --res<- gmapQ (dataToJSON df env tcm) t
                --liftM arr res
        string     = Data.Aeson.String . T.pack :: String -> Value
        fastString:: FastString -> Value
        fastString fs=object ["FastString" .= T.pack (show fs)]
        list l     = liftM arr $ mapM (dataToJSON df env tcm) l
        arr a = let
                sub=filter (/= Null) a
                in case sub of
                     [] -> Null
                     [x] -> x
                     _ -> toJSON sub
        name :: Name -> Value
        name  n     = object (nameAndModule n ++["GType" .= string "Name","HType".= string (if isValOcc (nameOccName n) then "v" else "t")])
        ocName :: OccName -> Value
        ocName o   = name (mkSystemName (getUnique o) o)
        modName  :: ModuleName -> Value
        modName m=  object [ "Name" .= string (showSD True df $ ppr m),"GType" .= string "ModuleName","HType" .= string "m"]

        var ::  Var -> IO Value
        var  v     = return $ typedVar v (varType v)
#if __GLASGOW_HASKELL__ >= 708
        rDataCon (RealDataCon dc)=dataCon dc
        rDataCon _ = Null
#endif
        dataCon ::  DataCon -> Value
        dataCon  d  = let
                t=dataConUserType d
                in object (nameAndModule (dataConName d) ++ typeToJSON t ++ [
                        "GType" .= string "DataCon",
                        "HType" .=  string "v"])
        simpleV:: T.Text -> Value -> Value
        simpleV nm v=object [nm .= v]
        bagRdrName:: Bag (Located (HsBind RdrName)) -> IO Value
        bagRdrName = liftM (simpleV "Bag(Located (HsBind RdrName))") . list . bagToList
        bagName   :: Bag (Located (HsBind Name)) -> IO Value
        bagName    = liftM (simpleV "Bag(Located (HsBind Name))") . list . bagToList
        bagVar    :: Bag (Located (HsBind Var)) -> IO Value
        bagVar     = liftM (simpleV "Bag(Located (HsBind Var))") . list . bagToList
        exprVar    :: HsExpr Var ->IO Value
        exprVar ev  = do
                -- https://github.com/JPMoresmau/BuildWrapper/issues/23 : catch panics
                mt<- getType env tcm (L noSrcSpan ev) `gcatch` (\(_::(GhcException))->return Nothing)
                case mt of
                        Just t->  case identOfExpr ev of
                                Just v -> return $ typedVar v t
                                Nothing->generic ev
                                        --do
                                        --val<-generic ev
                                        --return $ object (["Name" .= string "Expr","Module" .= string "", "Package" .= string "","GType" .= string "HsExpr","HType" .= string "v","sub" .= val] ++ typeToJSON t)
                        Nothing->generic ev
        typedVar :: Var -> Type -> Value
        typedVar v t=object (nameAndModule (varName v) ++ typeToJSON t ++
                ["GType" .= string "Var",
                "HType" .=  string (if isValOcc (nameOccName (Var.varName v)) then "v" else "t")])

        nameSet = const $ Data.Aeson.String "{!NameSet placeholder here!}" :: NameSet -> Value

        postTcType  = const Null :: Type -> Value -- string . showSDoc . ppr

        fixity  = const Null :: GHC.Fixity -> Value --simple "Fixity" . showSDoc . ppr

        typeToJSON :: Type -> [(T.Text,Value)]
        typeToJSON t =
#if __GLASGOW_HASKELL__ >= 707
                ["Type" .= string (showSD False df $ pprTypeForUser t),
                "QType" .= string (showSD True df $ pprTypeForUser t)]
#else
                ["Type" .= string (showSD False df $ pprTypeForUser True t),
                "QType" .= string (showSD True df $ pprTypeForUser True t)]
#endif
        hsBind :: HsBindLR Name Name -> IO Value
#if __GLASGOW_HASKELL__ >= 707
        hsBind (FunBind fid _ mg _ _ _) =do
                let matches = mg_alts mg
#else
        hsBind (FunBind fid _ (MatchGroup matches _) _ _ _) =do
#endif
                d2<-dataToJSON df env tcm $ unLoc fid
                liftM arr $ mapM (\m->do
                        d1<-dataToJSON df env tcm $ getLoc m
                        d3<-dataToJSON df env tcm m
                        return $ arr [arr [d1,d2],d3]) matches
        hsBind a=generic a
        nameAndModule :: Name -> [Pair]
        nameAndModule n=let
                mm=nameModule_maybe n
                mn=maybe "" (showSD True df . ppr . moduleName) mm
#if __GLASGOW_HASKELL__ >= 710
                pkg=maybe "" (showSD True df . ppr . modulePackageKey) mm
#else
                pkg=maybe "" (showSD True df . ppr . modulePackageId) mm
#endif
                na=showSD False df $ ppr n
                in ["Module" .= string mn,"Package" .= string pkg, "Name" .= string na]

-- | get type of an expression, inspired by the code in ghc-mod
getType ::  HscEnv -> TypecheckedModule -> LHsExpr Var -> IO(Maybe Type)
--getType _ _ (L _ (HsDo ArrowExpr _ _))=return Nothing
--getType _ _ (L _ (HsArrApp {}))=return Nothing
#if __GLASGOW_HASKELL__ >= 707
getType hs_env _ e = do
      (_, mbe) <- GMU.liftIO $ deSugarExpr hs_env e
#else
getType hs_env tcm e = do
      let modu   = ms_mod $ pm_mod_summary $ tm_parsed_module tcm
          rn_env = tcg_rdr_env $ fst $ tm_internals_ tcm
          ty_env = tcg_type_env $ fst $ tm_internals_ tcm
      (_, mbe) <- GMU.liftIO $ deSugarExpr hs_env modu rn_env ty_env e
#endif
      return $ fmap CoreUtils.exprType mbe

-- | show SDoc wrapper
showSD :: Bool
        -> DynFlags
        -> SDoc
        -> String
#if __GLASGOW_HASKELL__ < 706
showSD True _ =showSDoc
showSD False _ =showSDocUnqual
#else
showSD True df =showSDoc df
showSD False df =showSDocUnqual df
#endif

-- | show SDoc for user wrapper
showSDUser :: PrintUnqualified
        -> DynFlags
        -> SDoc
        -> String
#if __GLASGOW_HASKELL__ < 706
showSDUser unqual _ =showSDocForUser unqual
#else
showSDUser unqual df =showSDocForUser df unqual
#endif

-- | show SDoc for dump wrapper
showSDDump :: DynFlags
        -> SDoc
        -> String
#if __GLASGOW_HASKELL__ < 706
showSDDump _ =showSDocDump
#else
showSDDump df =showSDocDump df
#endif

-- | convert a SrcSpan to a JSON Value
srcSpanToJSON :: SrcSpan -> Value
srcSpanToJSON src
        | isGoodSrcSpan src   = object[ "SrcSpan" .= toJSON [srcLocToJSON $ srcSpanStart src, srcLocToJSON $ srcSpanEnd src]]
        | otherwise = Null

-- | convert a SrcLoc to a JSON Value
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

-- | get all types contained by another type
typesInsideType :: Type -> [Type]
typesInsideType t=let
         (f1,f2)=splitFunTys t
         in f2 : concatMap typesInsideType f1

---- | debug function: shows on standard output the JSON representation of the given data
--debugToJSON :: Data a =>a -> IO()
--debugToJSON = BSC.putStrLn . encode . dataToJSON
--
---- | debug searching thing at point in given data
--debugFindInJSON :: Data a => Int -> Int -> a -> IO()
--debugFindInJSON l c a= do
--        let v=dataToJSON a
--        let mv=findInJSON (overlap l c) v
--        case mv of
--                Just rv->do
--                        putStrLn "something found!"
--                        BSC.putStrLn $ encode rv
--                Nothing->putStrLn "nothing found!"

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

-- | find a named value in JSON data
findInJSONData :: Maybe Value -> Maybe ThingAtPoint
findInJSONData (Just o@(Object m)) | Just (String _)<-HM.lookup "Name" m=case fromJSON o of
        Success tap->tap
        Error _ -> Nothing
findInJSONData _=Nothing


-- | find in JSON AST
findInJSON :: FindFunc -- ^ the evaluation function
        -> Value -- ^ the root object containing the AST
        -> Maybe Value
findInJSON f (Array vals)=listToMaybe $ sortBy lastPos $ filter f $ V.toList vals
findInJSON _ _=Nothing

-- | find in JSON AST
findAllInJSON :: FindFunc -- ^ the evaluation function
        -> Value -- ^ the root object containing the AST
        -> [Value]
findAllInJSON f (Array vals)=filter f $ V.toList vals
findAllInJSON _ _=[]

-- | sort Value by position, descending
lastPos :: Value -> Value -> Ordering
lastPos (Object m1) (Object m2) |
       Just pos1<-HM.lookup "Pos" m1,
       Success ifs1 <- fromJSON pos1,
       Just pos2<-HM.lookup "Pos" m2,
       Success ifs2 <- fromJSON pos2 =let
                c1=compare (iflLine $ ifsStart ifs2) (iflLine $ ifsStart ifs1)
                in case c1 of
                        EQ -> compare (iflColumn $ ifsStart ifs2) (iflColumn $ ifsStart ifs1)
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

-- | contains function: find whatever is contained inside the given span
contains :: Int  -- ^ start line
        -> Int -- ^ start column
        -> Int  -- ^ end line
        -> Int -- ^ end column
        -> FindFunc
contains sl sc el ec (Object m) |
        Just pos<-HM.lookup "Pos" m,
        Success ifs <- fromJSON pos=iflOverlap (InFileSpan (InFileLoc sl sc) (InFileLoc el ec)) (ifsStart ifs)
contains _ _ _ _ _=False

-- | isGHCType function: find whatever has the proper GHCType
isGHCType :: String --  ^ the type
        -> FindFunc
isGHCType tp (Object m) |
        Just pos<-HM.lookup "GType" m,
        Success ghcType <- fromJSON pos=tp == ghcType
isGHCType _ _ =False


-- | extract usages from a global JSON Value
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

-- | Extract name values from a JSON Value
extractName :: Value -> Value -> [Value]
extractName src (Object m) |
        Just ifl<-extractSource src,
        Just (String s)<-HM.lookup "Name" m,
        Just (String mo)<-HM.lookup "Module" m,
        -- not $ T.null mo, -- keep local objects
        Just (String p)<-HM.lookup "Package" m,
        mqt<-HM.lookup "QType" m,
        mst<-HM.lookup "Type" m,
        mgt<-HM.lookup "GType" m,
        Just (String t)<-HM.lookup "HType" m,
        at<-fromMaybe (Array V.empty) $ HM.lookup "AllTypes" m
                =let
                atts=["Name" .= s,"Module" .= mo,"Package" .= p,"HType" .= t,"AllTypes" .= at, "Pos" .= toJSON ifl, "QType" .= mqt, "Type" .= mst,"GType" .= mgt]
                --atts1=if T.null mo
                --        then atts
                --        else ():atts
                in [object atts]
extractName _ _=[]

-- | extract source information from a JSON Value
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
        Just (Number l)<-HM.lookup "line" m,
        Just (Number c)<-HM.lookup "column" m=Just (round l,round c)
extractSourceLoc _ = Nothing

-- | resolve the ident in an expression
identOfExpr :: HsExpr Var -> Maybe Var
identOfExpr (HsWrap _ (HsVar ident)) = Just ident
identOfExpr (HsWrap _ wr1) =identOfExpr wr1
-- All other search results produce no ident information
identOfExpr _ = Nothing
