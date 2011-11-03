{-# LANGUAGE CPP, OverloadedStrings, TypeSynonymInstances,StandaloneDeriving,DeriveDataTypeable,ScopedTypeVariables, MultiParamTypeClasses, PatternGuards  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Language.Haskell.BuildWrapper.GHC
-- Author      : JP Moresmau
-- Copyright   : (c) JP Moresmau 2011
-- License     : BSD3
-- 
-- Maintainer  : jpmoresmau@gmail.com
-- Stability   : beta
-- Portability : portable
-- 
-- Load relevant module in the GHC AST and get GHC messages and thing at point info.
module Language.Haskell.BuildWrapper.GHC where
import Language.Haskell.BuildWrapper.Base hiding (Target)
import Language.Haskell.BuildWrapper.Find


-- import Text.JSON
-- import Data.DeriveTH
-- import Data.Derive.JSON
import Data.Char
import Data.Generics hiding (Fixity, typeOf)
import Data.Maybe
import Data.Monoid

import Data.IORef
import qualified Data.List as List
import Data.Ord (comparing)
import qualified Data.Text as T

import DynFlags
import ErrUtils ( ErrMsg(..), WarnMsg, mkPlainErrMsg,Messages,ErrorMessages,WarningMessages, Message )
import GHC
import SrcLoc 
import GHC.Paths ( libdir )
import HscTypes ( srcErrorMessages, SourceError)
import Outputable
import FastString (FastString,unpackFS,concatFS,fsLit,mkFastString)
import Lexer
import PprTyThing ( pprTypeForUser )
import Bag

#if __GLASGOW_HASKELL__ >= 610
import StringBuffer
#endif

import System.FilePath
--import System.Time

import qualified MonadUtils as GMU

getAST :: FilePath -> FilePath -> String -> [String] -> IO (OpResult (Maybe TypecheckedSource))
getAST =withASTNotes (\t -> do
        return $ tm_typechecked_source t
        )

withAST ::  (TypecheckedModule -> Ghc a) -> FilePath -> FilePath ->  String -> [String] -> IO (Maybe a)
withAST f fp base_dir mod options= do
        (a,_)<-withASTNotes f fp base_dir mod options
        return a


withASTNotes ::  (TypecheckedModule -> Ghc a) -> FilePath -> FilePath -> String -> [String] -> IO (OpResult (Maybe a))
withASTNotes f fp base_dir mod options=do
    let lflags=map noLoc options
    --putStrLn $ show options
    (_leftovers, _) <- parseStaticFlags lflags
    runGhc (Just libdir) $ do
        flg <- getSessionDynFlags
        (flg', _, _) <- parseDynamicFlags flg _leftovers
        ref <- GMU.liftIO $ newIORef []
        setSessionDynFlags flg'  { hscTarget = HscNothing, ghcLink = NoLink , ghcMode = CompManager, log_action = logAction ref }
        -- $ dopt_set (flg' { ghcLink = NoLink , ghcMode = CompManager }) Opt_ForceRecomp
        addTarget Target { targetId = TargetFile fp Nothing, targetAllowObjCode = True, targetContents = Nothing }
        --c1<-GMU.liftIO getClockTime
        let modName=mkModuleName mod
        -- loadWithLogger (logWarnErr ref)
        res<- load (LoadUpTo modName) -- LoadAllTargets
                   `gcatch` (\(e :: SourceError) -> handle_error ref e)
        --(warns, errs) <- GMU.liftIO $ readIORef ref
        --let notes = ghcMessagesToNotes base_dir (warns, errs)
        notes <- GMU.liftIO $ readIORef ref
        --c2<-GMU.liftIO getClockTime
        --GMU.liftIO $ putStrLn ("load all targets: " ++ (timeDiffToString  $ diffClockTimes c2 c1))
        case res of 
                Succeeded -> do
                        modSum <- getModSummary $ modName
                        p <- parseModule modSum
                        --return $ showSDocDump $ ppr $ pm_mod_summary p
                        t <- typecheckModule p
                        d <- desugarModule t -- to get warnings
                        l <- loadModule d
                        --c3<-GMU.liftIO getClockTime
                        setContext [ms_mod modSum] []
                        --GMU.liftIO $ putStrLn ("parse, typecheck load: " ++ (timeDiffToString  $ diffClockTimes c3 c2))
                        a<-f (dm_typechecked_module l)
#if __GLASGOW_HASKELL__ < 702                           
                        warns <- getWarnings
                        return $ (Just a,notes++ (reverse $ ghcMessagesToNotes base_dir (warns, emptyBag)))
#else
                        notes2 <- GMU.liftIO $ readIORef ref
                        return $ (Just a,notes2)
#endif
                Failed -> return $ (Nothing,notes)
        where
--            logWarnErr :: GhcMonad m => IORef [BWNote] -> Maybe SourceError -> m ()
--            logWarnErr ref err = do
--              let errs = case err of
--                           Nothing -> mempty
--                           Just exc -> srcErrorMessages exc
--              warns <- getWarnings
--              clearWarnings
--              add_warn_err ref warns errs
        
            add_warn_err :: GhcMonad m => IORef [BWNote] -> WarningMessages -> ErrorMessages -> m()
            add_warn_err ref warns errs = do
              let notes = ghcMessagesToNotes base_dir (warns, errs)
              GMU.liftIO $ modifyIORef ref $
                         \ns -> ( ns ++ notes)
        
            handle_error :: GhcMonad m => IORef [BWNote] -> SourceError -> m SuccessFlag
            handle_error ref e = do
               let errs = srcErrorMessages e
               add_warn_err ref emptyBag errs
--               warns <- getWarnings
--               add_warn_err ref warns errs
--               clearWarnings
               return Failed
               
            logAction :: IORef [BWNote] -> Severity -> SrcSpan -> PprStyle -> Message -> IO ()
            logAction ref s loc ppr msg
                | (Just status)<-bwSeverity s=do
                        let n=BWNote { bwn_location = ghcSpanToBWLocation base_dir loc
                                 , bwn_status = status
                                 , bwn_title = removeStatus status $ showSDocForUser (qualName ppr,qualModule ppr) msg
                                 }
                        modifyIORef ref $  \ns -> ( ns ++ [n])
                | otherwise=do
                        return ()
            
            bwSeverity :: Severity -> Maybe BWNoteStatus
            bwSeverity SevWarning = Just BWWarning       
            bwSeverity SevError   = Just BWError
            bwSeverity SevFatal   = Just BWError
            bwSeverity _          = Nothing
            
        --f $ tm_typechecked_source t
        --return $ showSDocDump $ ppr $ pm_parsed_source p
        --return $ showSDocDump $ ppr $ tm_typechecked_source t
    --return  $ makeObj  [("parse" , (showJSON $ tm_typechecked_source t))]
    --return r
   
-- | Convert 'GHC.Messages' to '[BWNote]'.
--
-- This will mix warnings and errors, but you can split them back up
-- by filtering the '[BWNote]' based on the 'bw_status'.
ghcMessagesToNotes :: FilePath ->  Messages -> [BWNote]
ghcMessagesToNotes base_dir (warns, errs) =
             (map_bag2ms (ghcWarnMsgToNote base_dir) warns) ++
             (map_bag2ms (ghcErrMsgToNote base_dir) errs)
  where
    map_bag2ms f =  map f . Bag.bagToList   
   
   
getGhcNamesInScope  :: FilePath -> FilePath -> String -> [String] -> IO [String]
getGhcNamesInScope f base_dir mod options=do
        names<-withAST (\_->do
                --c1<-GMU.liftIO getClockTime
                names<-getNamesInScope
                --c2<-GMU.liftIO getClockTime
                --GMU.liftIO $ putStrLn ("getNamesInScope: " ++ (timeDiffToString  $ diffClockTimes c2 c1))
                return $ map (showSDocDump . ppr ) names)  f base_dir mod options
        return $ fromMaybe[] names
   
getThingAtPoint :: Int -> Int -> Bool -> Bool -> FilePath -> FilePath -> String -> [String] -> IO String
getThingAtPoint line col qual typed fp base_dir mod options= do
        t<-withAST (\tcm->do
              let loc = srcLocSpan $ mkSrcLoc (fsLit fp) line (scionColToGhcCol col)
              uq<-unqualifiedForModule tcm
              let f=(if typed then (doThingAtPointTyped $ typecheckedSource tcm) else (doThingAtPointUntyped $ renamedSource tcm))
              --tap<- doThingAtPoint loc qual typed tcm (if typed then (typecheckedSource tcm) else (renamedSource tcm))
              let tap=f loc qual tcm uq
              --(if typed then (doThingAtPointTyped $ typecheckedSource tcm)
              -- else doThingAtPointTyped (renamedSource tcm) loc qual tcm
              return tap) fp base_dir mod options
        return $ fromMaybe "" t
      where
            doThingAtPointTyped :: TypecheckedSource -> SrcSpan -> Bool -> TypecheckedModule -> PrintUnqualified -> String
            doThingAtPointTyped src loc qual tcm uq=let
                    in_range = overlaps loc
                    r = searchBindBag in_range noSrcSpan src
                    unqual = if qual
                        then alwaysQualify
                        else uq
                    --liftIO $ putStrLn $ showData TypeChecker 2 src
                    in case pathToDeepest r of
                      Nothing -> "no info"
                      Just (x,xs) ->
                        case typeOf (x,xs) of
                          Just t ->
                              showSDocForUser unqual
                                (prettyResult x <+> dcolon <+>
                                  pprTypeForUser True t)
                          _ -> showSDocForUser unqual (prettyResult x) --(Just (showSDocDebug (ppr x $$ ppr xs )))
            doThingAtPointUntyped :: (Search id a, OutputableBndr id) => a -> SrcSpan -> Bool -> TypecheckedModule  -> PrintUnqualified -> String
            doThingAtPointUntyped src loc qual tcm uq=let
                    in_range = overlaps loc
                    r = findHsThing in_range src
                    unqual = if qual
                        then neverQualify
                        else uq
                    in case pathToDeepest r of
                      Nothing -> "no info"
                      Just (x,_) ->
                        if qual
                                then showSDocForUser unqual ((qualifiedResult x) <+> (text $ haddockType x))
                                else showSDocForUser unqual ((prettyResult x) <+> (text $ haddockType x))   
   
unqualifiedForModule :: TypecheckedMod m => m -> Ghc PrintUnqualified
unqualifiedForModule tcm =fromMaybe alwaysQualify `fmap` mkPrintUnqualifiedForModule (moduleInfo tcm)   
   
--data S a=S String    
--    
--instance Show (S a) where
--        show (S s)=s    
--   
--data TestLoc=TestLoc Int Int
--        deriving (Show,Data,Typeable)
--data Test=Test [TestLoc]
--        deriving (Show,Data,Typeable)
--   
--test1=Test [TestLoc 3 4,TestLoc 2 14,TestLoc 3 16]
--
--getThingAtPoint :: TypecheckedSource -> Int -> Int -> [String]
--getThingAtPoint ts line col= everything (++) ([] `mkQ` (\x -> p x )) test1
--        -- synthesize [] toS (mkQ Nothing toS) test1
--        where 
--               p :: TestLoc -> [String]
--               p t@(TestLoc a b)= if a==line || b==col
--                        then [show t]
--                        else []
--               
   
--getThingAtPoint :: TypecheckedSource -> Int -> Int -> [String]
--getThingAtPoint ts line col= maybeToList  $ something (mkQ Nothing toS) test1
--        where 
--               toS :: TestLoc -> Maybe String
--               toS t@(TestLoc a b)=if a==line || b==col
--                        then Just $ show t
--                        else Nothing


--getThingAtPoint :: TypecheckedSource -> Int -> Int -> [String]
--getThingAtPoint ts line col=map show ((listify (mkQ False (isJust . toS)) test1)::[TestLoc])
--        -- maybeToList  $ something (mkQ Nothing toS) test1
--        where 
--               toS :: TestLoc -> Maybe String
--               toS t@(TestLoc a b)=if a==line || b==col
--                        then Just $ show t
--                        else Nothing

--newtype C a = C a deriving (Data,Typeable)
--
----getThingAtPoint :: TypecheckedSource -> Int -> Int -> [String]
--
--coerce :: a -> C a
--coerce = unsafeCoerce
--uncoerce :: C a -> a
--uncoerce = unsafeCoerce
--
--fmapData :: forall t a b. (Typeable a, Data (t (C a)), Data (t a)) =>
--    (a -> b) -> t a -> t b
--fmapData f input = uc . everything (++) ([] `mkQ` (\(x::C a) -> [coerce (f (uncoerce x))]))
--                    $ (coerce input)
--    where uc = unsafeCoerce
--
--getThingAtPoint :: TypecheckedSource -> Int -> Int -> [String]
--getThingAtPoint ts line col= --map unLoc $ filter (\(L _ o)->not $ null o) $ fmapData overlap (bagToList ts)
--        map unLoc $ filter (\(L _ o)->not $ null o) $  everything (++) ([] `mkQ` overlap) (bagToList ts)
--   where 
--        --overlap :: forall b1 . (Outputable b1, Typeable b1) =>Located b1  -> Located String
--        overlap :: Located (HsBindLR Id Id)  -> [Located String]
--        overlap (L loc o)= let
--                st=srcSpanStart loc
--                en=srcSpanEnd loc
--                in if (isGoodSrcLoc st) && (isGoodSrcLoc en) && ((srcLocLine st) <= line) && ((srcLocCol st) <=col) && ((srcLocLine en) >= line) && ((srcLocCol en) >=col)
--                        then [L loc $ showSDocDump $ ppr o]
--                        else []
--   
-- [showData TypeChecker 4 ts]


--getThingAtPoint :: TypecheckedSource -> Int -> Int -> [String]
--getThingAtPoint ts line col= map pr lf
--        --maybeToList  $ something (mkQ Nothing toS) ts -- listify (mkQ False overlap) ts
--   where 
--        lf :: forall b1 . (Outputable b1, Typeable b1) => [Located b1]
--        lf=listify (mkQ False overlap) ts
--        --find :: (Outputable a,Typeable a) => TypecheckedSource -> [Located a]
--        --find = listify $ overlap
--        toS :: forall b1 . (Outputable b1, Typeable b1) =>Located b1  -> Maybe String
--        toS l= if overlap l
--                then Just $ pr l
--                else Nothing
--        pr :: forall b1 . (Outputable b1) => Located b1 -> String
--        pr=showSDocDump . ppr . unLoc
--        --showData TypeChecker 4
--        overlap :: forall b1 . (Outputable b1, Typeable b1) =>Located b1  -> Bool
--        overlap (a::Located b1)= let
--                (L loc _)=a
--                st=srcSpanStart loc
--                en=srcSpanEnd loc
--                in (isGoodSrcLoc st) && (isGoodSrcLoc en) && ((srcLocLine st) <= line) && ((srcLocCol st) <=col) && ((srcLocLine en) >= line) && ((srcLocCol en) >=col)
--                --        overlap _ =False
--   
--   

ghcSpanToLocation :: FilePath -- ^ Base directory
                  -> GHC.SrcSpan
                  -> InFileSpan
ghcSpanToLocation baseDir sp
  | GHC.isGoodSrcSpan sp =let
      (stl,stc)=start sp
      (enl,enc)=end sp
      in mkFileSpan 
                 stl
                 (ghcColToScionCol stc)
                 (enl)
                 (ghcColToScionCol enc)
  | otherwise = mkFileSpan 0 0 0 0

   
   
ghcSpanToBWLocation :: FilePath -- ^ Base directory
                  -> GHC.SrcSpan
                  -> BWLocation
ghcSpanToBWLocation baseDir sp
  | GHC.isGoodSrcSpan sp =
      let (stl,stc)=start sp
      in BWLocation (makeRelative baseDir $ foldr f [] $ normalise $ unpackFS (sfile sp))
                 stl
                 (ghcColToScionCol $stc)
  | otherwise = BWLocation "" 1 1
        where   
                f c (x:xs) 
                        | c=='\\' && x=='\\'=x:xs   -- WHY do we get two slashed after the drive sometimes?
                        | otherwise=c:x:xs
                f c s=c:s
#if __GLASGOW_HASKELL__ < 702   
                sfile ss= GHC.srcSpanFile ss
#else 
                sfile (RealSrcSpan ss)= GHC.srcSpanFile ss
#endif 
                        
ghcColToScionCol :: Int -> Int
#if __GLASGOW_HASKELL__ < 700
ghcColToScionCol c=c+1 -- GHC 6.x starts at 0 for columns
#else
ghcColToScionCol c=c -- GHC 7 starts at 1 for columns
#endif

scionColToGhcCol :: Int -> Int
#if __GLASGOW_HASKELL__ < 700
scionColToGhcCol c=c-1 -- GHC 6.x starts at 0 for columns
#else
scionColToGhcCol c=c -- GHC 7 starts at 1 for columns
#endif        
        
-- | Get a stream of tokens generated by the GHC lexer from the current document
ghctokensArbitrary :: FilePath -- ^ The file path to the document
                   -> String -- ^ The document's contents
                   -> [String] -- ^ The options
                   -> IO (Either BWNote [Located Token])
ghctokensArbitrary base_dir contents options= do
#if __GLASGOW_HASKELL__ < 702
        sb <- stringToStringBuffer contents
#else
        let sb=stringToStringBuffer contents
#endif
        let lflags=map noLoc options
        (_leftovers, _) <- parseStaticFlags lflags
        runGhc (Just libdir) $ do
                flg <- getSessionDynFlags
                (flg', _, _) <- parseDynamicFlags flg _leftovers
         
#if __GLASGOW_HASKELL__ >= 700
                let dflags1 = List.foldl' xopt_set flg' lexerFlags
#else
                let dflags1 = List.foldl' dopt_set flg' lexerFlags
#endif
                let prTS = lexTokenStream sb lexLoc dflags1
                case prTS of
                        POk _ toks      -> return $ Right $ (filter ofInterest toks)
                        PFailed loc msg -> return $ Left $ ghcErrMsgToNote base_dir $ mkPlainErrMsg loc msg

#if __GLASGOW_HASKELL__ < 702
lexLoc :: SrcLoc
lexLoc = mkSrcLoc (mkFastString "<interactive>") 1 (scionColToGhcCol 1)
#else
lexLoc :: RealSrcLoc
lexLoc = mkRealSrcLoc  (mkFastString "<interactive>") 1 (scionColToGhcCol 1)
#endif


#if __GLASGOW_HASKELL__ >= 700
lexerFlags :: [ExtensionFlag]
#else
lexerFlags :: [DynFlag]
#endif
lexerFlags =
        [ Opt_ForeignFunctionInterface
        , Opt_Arrows
#if __GLASGOW_HASKELL__ < 702        
        , Opt_PArr
#else
        , Opt_ParallelArrays
#endif        
        , Opt_TemplateHaskell
        , Opt_QuasiQuotes
        , Opt_ImplicitParams
        , Opt_BangPatterns
        , Opt_TypeFamilies
#if __GLASGOW_HASKELL__ < 700
        , Opt_Haddock
#endif
        , Opt_MagicHash
        , Opt_KindSignatures
        , Opt_RecursiveDo
        , Opt_UnicodeSyntax
        , Opt_UnboxedTuples
        , Opt_StandaloneDeriving
        , Opt_TransformListComp
#if __GLASGOW_HASKELL__ < 702          
        , Opt_NewQualifiedOperators
#endif        
#if GHC_VERSION > 611       
        , Opt_ExplicitForAll -- 6.12
        , Opt_DoRec -- 6.12
#endif
        ]                
               
-- | Filter tokens whose span appears legitimate (start line is less than end line, start column is
-- less than end column.)
ofInterest :: Located Token -> Bool
ofInterest (L span _) =
  let  (sl,sc) = start span
       (el,ec) = end span
  in (sl < el) || (sc < ec)

--toInteractive ::  Location -> Location
--toInteractive l =
--  let (_, sl1, sc1, el1, ec1) = viewLoc l
--  in  mkLocation interactive sl1 sc1 el1 ec1        
       
-- | Convert a GHC token to an interactive token (abbreviated token type)
tokenToType :: FilePath -> Located Token -> TokenDef
tokenToType base_dir (L sp t) = TokenDef (tokenType t) (ghcSpanToLocation base_dir sp)       
        
-- | Generate the interactive token list used by EclipseFP for syntax highlighting
tokenTypesArbitrary :: FilePath -> String -> Bool -> [String] -> IO (Either BWNote [TokenDef])
tokenTypesArbitrary projectRoot contents literate options = generateTokens projectRoot contents literate options convertTokens id
  where
    convertTokens = map (tokenToType projectRoot)        
        
-- | Extract occurrences based on lexing  
occurrences :: FilePath     -- ^ Project root or base directory for absolute path conversion
            -> String    -- ^ Contents to be parsed
            -> T.Text    -- ^ Token value to find
            -> Bool      -- ^ Literate source flag (True = literate, False = ordinary)
            -> [String]  -- ^ Options
            -> IO (Either BWNote [TokenDef])
occurrences projectRoot contents query literate options = 
  let 
      qualif = isJust $ T.find (=='.') query
      -- Get the list of tokens matching the query for relevant token types
      tokensMatching :: [TokenDef] -> [TokenDef]
      tokensMatching = filter matchingVal
      matchingVal :: TokenDef -> Bool
      matchingVal (TokenDef v _)=query==v
      mkTokenDef (L sp t)=TokenDef (tokenValue qualif t) (ghcSpanToLocation projectRoot sp)
  in generateTokens projectRoot contents literate options (map mkTokenDef) tokensMatching        
        
-- | Parse the current document, generating a TokenDef list, filtered by a function
generateTokens :: FilePath                        -- ^ The project's root directory
               -> String                          -- ^ The current document contents, to be parsed
               -> Bool                            -- ^ Literate Haskell flag
               -> [String]                         -- ^ The options
               -> ([Located Token] -> [TokenDef]) -- ^ Transform function from GHC tokens to TokenDefs
               -> ([TokenDef] -> a)               -- ^ The TokenDef filter function
               -> IO (Either BWNote a)
generateTokens projectRoot contents literate options  xform filterFunc =
  let (ppTs, ppC) = preprocessSource contents literate
  in   ghctokensArbitrary projectRoot ppC options
       >>= (\result ->
             case result of 
               Right toks ->
                 let filterResult = filterFunc $ List.sortBy (comparing td_loc) (ppTs ++ (xform toks))
                 --liftIO $ putStrLn $ show tokenList
                 in return $ Right filterResult
               Left n -> return $ Left n
               )
     
-- | Preprocess some source, returning the literate and Haskell source as tuple.
--preprocessSource ::  String -> Bool -> ([TokenDef],String)
--preprocessSource contents literate=
--        let 
--                (ts1,s2)=if literate then ppSF contents ppSLit else ([],contents) 
--                (ts2,s3)=ppSF s2 ppSCpp
--        in (ts1++ts2,s3)
--        where 
--                ppSF contents2 p= let
--                        linesWithCount=zip (lines contents2) [1..]
--                        (ts,nc,_)= List.foldl' p (Seq.empty,Seq.empty,False) linesWithCount
--                        in (F.toList ts, F.concatMap (++ "\n") nc)
--                ppSCpp :: (Seq.Seq TokenDef,Seq.Seq String,Bool) -> (String,Int) -> (Seq.Seq TokenDef,Seq.Seq String,Bool)
--                ppSCpp (ts2,l2,f) (l,c) 
--                        | f = addPPToken "PP" (l,c) (ts2,l2,'\\' == (last l))
--                        | ('#':_)<-l =addPPToken "PP" (l,c) (ts2,l2,'\\' == (last l)) 
--                        | ("{-# " `List.isPrefixOf` l)=addPPToken "D" (l,c) (ts2,l2,False) 
--                        | otherwise =(ts2,l2 Seq.|> l,False)
--                ppSLit :: (Seq.Seq TokenDef,Seq.Seq String,Bool) -> (String,Int) -> (Seq.Seq TokenDef,Seq.Seq String,Bool)
--                ppSLit (ts2,l2,f) (l,c) 
--                        | "\\begin{code}" `List.isPrefixOf` l=addPPToken "DL" ("\\begin{code}",c) (ts2,l2,True)
--                        | "\\end{code}" `List.isPrefixOf` l=addPPToken "DL" ("\\end{code}",c) (ts2,l2,False)
--                        | f = (ts2,l2 Seq.|> l,True)
--                        | ('>':lCode)<-l=(ts2,l2 Seq.|> (' ':lCode ),f)
--                        | otherwise =addPPToken "DL" (l,c) (ts2,l2,f)  
--                addPPToken :: T.Text -> (String,Int) -> (Seq.Seq TokenDef,Seq.Seq String,Bool) -> (Seq.Seq TokenDef,Seq.Seq String,Bool)
--                addPPToken name (l,c) (ts2,l2,f) =(ts2 Seq.|> (TokenDef name (mkFileSpan c 1 c ((length l)+1))),l2 Seq.|> "",f)

preprocessSource ::  String -> Bool -> ([TokenDef],String)
preprocessSource contents literate=
        let 
                (ts1,s2)=if literate then ppSF contents ppSLit else ([],contents) 
                (ts2,s3)=ppSF s2 ppSCpp
        in (ts1++ts2,s3)
        where 
                ppSF contents2 p= let
                        linesWithCount=zip (lines contents2) [1..]
                        (ts,nc,_)= List.foldl' p ([],[],False) linesWithCount
                        in (reverse ts, unlines $ reverse nc)
                ppSCpp :: ([TokenDef],[String],Bool) -> (String,Int) -> ([TokenDef],[String],Bool)
                ppSCpp (ts2,l2,f) (l,c) 
                        | f = addPPToken "PP" (l,c) (ts2,l2,'\\' == (last l))
                        | ('#':_)<-l =addPPToken "PP" (l,c) (ts2,l2,'\\' == (last l)) 
                        | ("{-# " `List.isPrefixOf` l)=addPPToken "D" (l,c) (ts2,l2,False) 
                        | otherwise =(ts2,l:l2,False)
                ppSLit :: ([TokenDef],[String],Bool) -> (String,Int) -> ([TokenDef],[String],Bool)
                ppSLit (ts2,l2,f) (l,c) 
                        | "\\begin{code}" `List.isPrefixOf` l=addPPToken "DL" ("\\begin{code}",c) (ts2,l2,True)
                        | "\\end{code}" `List.isPrefixOf` l=addPPToken "DL" ("\\end{code}",c) (ts2,l2,False)
                        | f = (ts2,l:l2,True)
                        | ('>':lCode)<-l=(ts2, (' ':lCode ):l2,f)
                        | otherwise =addPPToken "DL" (l,c) (ts2,l2,f)  
                addPPToken :: T.Text -> (String,Int) -> ([TokenDef],[String],Bool) -> ([TokenDef],[String],Bool)
                addPPToken name (l,c) (ts2,l2,f) =((TokenDef name (mkFileSpan c 1 c ((length l)+1))):ts2 ,"":l2,f)


ghcErrMsgToNote :: FilePath -> ErrMsg -> BWNote
ghcErrMsgToNote = ghcMsgToNote BWError

ghcWarnMsgToNote :: FilePath -> WarnMsg -> BWNote
ghcWarnMsgToNote = ghcMsgToNote BWWarning

-- Note that we don *not* include the extra info, since that information is
-- only useful in the case where we don not show the error location directly
-- in the source.
ghcMsgToNote :: BWNoteStatus -> FilePath -> ErrMsg -> BWNote
ghcMsgToNote note_kind base_dir msg =
    BWNote { bwn_location = ghcSpanToBWLocation base_dir loc
         , bwn_status = note_kind
         , bwn_title = removeStatus note_kind $ show_msg (errMsgShortDoc msg)
         }
  where
    loc | (s:_) <- errMsgSpans msg = s
        | otherwise                    = GHC.noSrcSpan
    unqual = errMsgContext msg
    show_msg = showSDocForUser unqual
    
removeStatus :: BWNoteStatus -> String -> String
removeStatus BWWarning s 
        | List.isPrefixOf "Warning:" s = List.dropWhile isSpace $ drop 8 s
        | otherwise = s
removeStatus BWError s 
        | List.isPrefixOf "Error:" s = List.dropWhile isSpace $ drop 6 s
        | otherwise = s        

#if CABAL_VERSION == 106
deriving instance Typeable StringBuffer
deriving instance Data StringBuffer
#endif

mkUnqualTokenValue :: FastString
                   -> T.Text
mkUnqualTokenValue = T.pack . unpackFS


mkQualifiedTokenValue :: FastString
                      -> FastString
                      -> T.Text
mkQualifiedTokenValue q a = (T.pack . unpackFS . concatFS) [q, dotFS, a]

-- | Make a token definition from its source location and Lexer.hs token type.
mkTokenDef :: FilePath -> Located Token -> TokenDef
mkTokenDef base_dir (L sp t) = TokenDef (mkTokenName t) (ghcSpanToLocation base_dir sp)

mkTokenName :: Token -> T.Text
mkTokenName = T.pack . showConstr . toConstr

deriving instance Typeable Token
deriving instance Data Token

#if CABAL_VERSION == 106
deriving instance Typeable StringBuffer
deriving instance Data StringBuffer
#endif


tokenType :: Token -> T.Text
tokenType  ITas = "K"                         -- Haskell keywords
tokenType  ITcase = "K"
tokenType  ITclass = "K"
tokenType  ITdata = "K"
tokenType  ITdefault = "K"
tokenType  ITderiving = "K"
tokenType  ITdo = "K"
tokenType  ITelse = "K"
tokenType  IThiding = "K"
tokenType  ITif = "K"
tokenType  ITimport = "K"
tokenType  ITin = "K"
tokenType  ITinfix = "K"
tokenType  ITinfixl = "K"
tokenType  ITinfixr = "K"
tokenType  ITinstance = "K"
tokenType  ITlet = "K"
tokenType  ITmodule = "K"
tokenType  ITnewtype = "K"
tokenType  ITof = "K"
tokenType  ITqualified = "K"
tokenType  ITthen = "K"
tokenType  ITtype = "K"
tokenType  ITwhere = "K"
tokenType  ITscc = "K"                       -- ToDo: remove (we use {-# SCC "..." #-} now)

tokenType  ITforall = "EK"                    -- GHC extension keywords
tokenType  ITforeign = "EK"
tokenType  ITexport= "EK"
tokenType  ITlabel= "EK"
tokenType  ITdynamic= "EK"
tokenType  ITsafe= "EK"
#if __GLASGOW_HASKELL__ < 702
tokenType  ITthreadsafe= "EK"
#endif
tokenType  ITunsafe= "EK"
tokenType  ITstdcallconv= "EK"
tokenType  ITccallconv= "EK"
#if __GLASGOW_HASKELL__ >= 612
tokenType  ITprimcallconv= "EK"
#endif
tokenType  ITmdo= "EK"
tokenType  ITfamily= "EK"
tokenType  ITgroup= "EK"
tokenType  ITby= "EK"
tokenType  ITusing= "EK"

        -- Pragmas
tokenType  (ITinline_prag {})="P"          -- True <=> INLINE, False <=> NOINLINE
#if __GLASGOW_HASKELL__ >= 612 && __GLASGOW_HASKELL__ < 700 
tokenType  (ITinline_conlike_prag {})="P"  -- same
#endif
tokenType  ITspec_prag="P"                 -- SPECIALISE   
tokenType  (ITspec_inline_prag {})="P"     -- SPECIALISE INLINE (or NOINLINE)
tokenType  ITsource_prag="P"
tokenType  ITrules_prag="P"
tokenType  ITwarning_prag="P"
tokenType  ITdeprecated_prag="P"
tokenType  ITline_prag="P"
tokenType  ITscc_prag="P"
tokenType  ITgenerated_prag="P"
tokenType  ITcore_prag="P"                 -- hdaume: core annotations
tokenType  ITunpack_prag="P"
#if __GLASGOW_HASKELL__ >= 612
tokenType  ITann_prag="P"
#endif
tokenType  ITclose_prag="P"
tokenType  (IToptions_prag {})="P"
tokenType  (ITinclude_prag {})="P"
tokenType  ITlanguage_prag="P"

tokenType  ITdotdot="S"                    -- reserved symbols
tokenType  ITcolon="S"
tokenType  ITdcolon="S"
tokenType  ITequal="S"
tokenType  ITlam="S"
tokenType  ITvbar="S"
tokenType  ITlarrow="S"
tokenType  ITrarrow="S"
tokenType  ITat="S"
tokenType  ITtilde="S"
tokenType  ITdarrow="S"
tokenType  ITminus="S"
tokenType  ITbang="S"
tokenType  ITstar="S"
tokenType  ITdot="S"

tokenType  ITbiglam="ES"                    -- GHC-extension symbols

tokenType  ITocurly="SS"                    -- special symbols
tokenType  ITccurly="SS" 
tokenType  ITocurlybar="SS"                 -- "{|", for type applications
tokenType  ITccurlybar="SS"                 -- "|}", for type applications
tokenType  ITvocurly="SS" 
tokenType  ITvccurly="SS" 
tokenType  ITobrack="SS" 
tokenType  ITopabrack="SS"                   -- [:, for parallel arrays with -XParr
tokenType  ITcpabrack="SS"                   -- :], for parallel arrays with -XParr
tokenType  ITcbrack="SS" 
tokenType  IToparen="SS" 
tokenType  ITcparen="SS" 
tokenType  IToubxparen="SS" 
tokenType  ITcubxparen="SS" 
tokenType  ITsemi="SS" 
tokenType  ITcomma="SS" 
tokenType  ITunderscore="SS" 
tokenType  ITbackquote="SS" 

tokenType  (ITvarid {})="IV"        -- identifiers
tokenType  (ITconid {})="IC"
tokenType  (ITvarsym {})="IV"
tokenType  (ITconsym {})="IC"
tokenType  (ITqvarid {})="IV"
tokenType  (ITqconid {})="IC"
tokenType  (ITqvarsym {})="IV"
tokenType  (ITqconsym {})="IC"
tokenType  (ITprefixqvarsym {})="IV"
tokenType  (ITprefixqconsym {})="IC"

tokenType  (ITdupipvarid {})="EI"   -- GHC extension: implicit param: ?x

tokenType  (ITchar {})="LC"
tokenType  (ITstring {})="LS"
tokenType  (ITinteger {})="LI"
tokenType  (ITrational {})="LR"

tokenType  (ITprimchar {})="LC"
tokenType  (ITprimstring {})="LS"
tokenType  (ITprimint {})="LI"
tokenType  (ITprimword {})="LW"
tokenType  (ITprimfloat {})="LF"
tokenType  (ITprimdouble {})="LD"

  -- Template Haskell extension tokens
tokenType  ITopenExpQuote="TH"              --  [| or [e|
tokenType  ITopenPatQuote="TH"              --  [p|
tokenType  ITopenDecQuote="TH"              --  [d|
tokenType  ITopenTypQuote="TH"              --  [t|         
tokenType  ITcloseQuote="TH"                --tokenType ]
tokenType  (ITidEscape {})="TH"    --  $x
tokenType  ITparenEscape="TH"               --  $( 
tokenType  ITvarQuote="TH"                  --  '
tokenType  ITtyQuote="TH"                   --  ''
tokenType  (ITquasiQuote {})="TH" --  [:...|...|]

  -- Arrow notation extension
tokenType  ITproc="A"
tokenType  ITrec="A"
tokenType  IToparenbar="A"                 --  (|
tokenType  ITcparenbar="A"                 --tokenType )
tokenType  ITlarrowtail="A"                --  -<
tokenType  ITrarrowtail="A"                --  >-
tokenType  ITLarrowtail="A"                --  -<<
tokenType  ITRarrowtail="A"                --  >>-

#if __GLASGOW_HASKELL__ <= 611
tokenType  ITdotnet="SS"                   -- ??
tokenType  (ITpragma _) = "SS"             -- ??
#endif

tokenType  (ITunknown {})=""           -- Used when the lexer can't make sense of it
tokenType  ITeof=""                       -- end of file token

  -- Documentation annotations
tokenType  (ITdocCommentNext {})="D"     -- something beginning '-- |'
tokenType  (ITdocCommentPrev {})="D"    -- something beginning '-- ^'
tokenType  (ITdocCommentNamed {})="D"     -- something beginning '-- $'
tokenType  (ITdocSection {})="D" -- a section heading
tokenType  (ITdocOptions {})="D"    -- doc options (prune, ignore-exports, etc)
tokenType  (ITdocOptionsOld {})="D"     -- doc options declared "-- # ..."-style
tokenType  (ITlineComment {})="D"     -- comment starting by "--"
tokenType  (ITblockComment {})="D"     -- comment in {- -}

  -- 7.2 new token types 
#if __GLASGOW_HASKELL__ >= 702
tokenType  (ITinterruptible {})="EK"
tokenType  (ITvect_prag {})="P"
tokenType  (ITvect_scalar_prag {})="P"
tokenType  (ITnovect_prag {})="P"
#endif

dotFS :: FastString
dotFS = fsLit "."

tokenValue :: Bool -> Token -> T.Text
tokenValue _ t | elem (tokenType t) ["K","EK"] = T.drop 2 $ mkTokenName t
tokenValue _ (ITvarid a) = mkUnqualTokenValue a
tokenValue _ (ITconid a) = mkUnqualTokenValue a
tokenValue _ (ITvarsym a) = mkUnqualTokenValue a
tokenValue _ (ITconsym a) = mkUnqualTokenValue a
tokenValue False (ITqvarid (_,a)) = mkUnqualTokenValue a
tokenValue True (ITqvarid (q,a)) = mkQualifiedTokenValue q a
tokenValue False(ITqconid (_,a)) = mkUnqualTokenValue a
tokenValue True (ITqconid (q,a)) = mkQualifiedTokenValue q a
tokenValue False (ITqvarsym (_,a)) = mkUnqualTokenValue a
tokenValue True (ITqvarsym (q,a))  = mkQualifiedTokenValue q a
tokenValue False (ITqconsym (_,a)) = mkUnqualTokenValue a
tokenValue True (ITqconsym (q,a)) = mkQualifiedTokenValue q a
tokenValue False (ITprefixqvarsym (_,a)) = mkUnqualTokenValue a
tokenValue True (ITprefixqvarsym (q,a)) = mkQualifiedTokenValue q a
tokenValue False (ITprefixqconsym (_,a)) = mkUnqualTokenValue a
tokenValue True (ITprefixqconsym (q,a)) = mkQualifiedTokenValue q a
tokenValue _ _= ""                       
        
instance Monoid (Bag a) where
  mempty = emptyBag
  mappend = unionBags
  mconcat = unionManyBags        
        
--instance JSON SrcLoc
--        where showJSON src 
--                | isGoodSrcLoc src =makeObj [((unpackFS $ srcLocFile src) , (JSArray  [showJSON $ srcLocLine src,showJSON $ srcLocCol src])) ]
--                | otherwise = JSNull
--        
--instance JSON SrcSpan
--        where showJSON src 
--                | isGoodSrcSpan src=makeObj [((unpackFS $ srcSpanFile src) , (JSArray  [showJSON $ srcSpanStartLine src,showJSON $ srcSpanStartCol src,showJSON $ srcSpanEndLine src,showJSON $ srcSpanEndCol src])) ]
--                | otherwise = JSNull     
--  
--instance JSON FastString
--        where showJSON=JSString . toJSString . unpackFS
--  
--instance JSON ModuleName
--        where showJSON=JSString . toJSString . moduleNameString
--        
--instance JSON OccName
--        where showJSON=JSString . toJSString . showSDocDump . ppr  
--
--instance JSON Type
--        where showJSON =JSString . toJSString . showSDocDump . ppr
--  
--instance JSON DataCon
--        where showJSON=JSString . toJSString . showSDocDump . ppr  
--
--instance JSON Unique
--        where showJSON=JSString . toJSString . showSDocDump . ppr  
--
--instance JSON Name
--        where showJSON=JSString . toJSString . showSDocDump . ppr  
--
--instance JSON EvBindsVar
--        where showJSON=JSString . toJSString . showSDocDump . ppr  
--
--instance JSON PackageId
--        where showJSON=JSString . toJSString . showSDocDump . ppr  
--        
--instance (JSON a)=> JSON (Bag a)
--        where showJSON=JSArray . map showJSON . bagToList
--
--instance (JSON a)=> JSON (UniqSet  a)
--        where showJSON=JSArray . map showJSON . uniqSetToList 
--
--instance JSON Rational
--        where showJSON=JSRational False 
--
--instance JSON Var
--        where showJSON v=makeObj [("Name",showJSON $ Var.varName v),("Unique",showJSON $ varUnique v),("Type",showJSON $ varType v)] 
--
--instance JSON RdrName
--        where 
--                showJSON (Unqual on) = JSArray [showJSON on]
--                showJSON (Qual mn on)  = JSArray [showJSON mn,showJSON on]  
--  
--instance(JSON a)=> JSON (Located a)
--        where showJSON (L s o)=case showJSON o of
--                JSObject o->let
--                        ass=fromJSObject o
--                        in JSObject $ toJSObject (("Loc",(showJSON s)):ass)
--                JSNull -> JSNull
--                v->makeObj [("Loc",(showJSON s)),("Object" ,v)]  
--  
-- $( derive makeJSON ''HsModule )
-- $( derive makeJSON ''ImportDecl )      
-- $( derive makeJSON ''HsDocString )   
-- $( derive makeJSON ''HsDecl)   
-- $( derive makeJSON ''WarningTxt)   
--      
--
-- $( derive makeJSON ''InstDecl )
---- $( derive makeJSON ''HsBind )
-- $( derive makeJSON ''HsBindLR )
-- $( derive makeJSON ''DefaultDecl )
-- $( derive makeJSON ''WarnDecl )
-- $( derive makeJSON ''RuleDecl )
-- $( derive makeJSON ''DocDecl )
-- $( derive makeJSON ''HsQuasiQuote )
-- $( derive makeJSON ''SpliceDecl )
-- $( derive makeJSON ''AnnDecl )
-- $( derive makeJSON ''ForeignDecl )
-- $( derive makeJSON ''Sig )
-- $( derive makeJSON ''DerivDecl )
-- $( derive makeJSON ''TyClDecl )
--      
-- $( derive makeJSON ''NewOrData )
-- $( derive makeJSON ''HsTyVarBndr )
-- $( derive makeJSON ''HsPred )
-- $( derive makeJSON ''HsType )
-- $( derive makeJSON ''ConDecl )
-- $( derive makeJSON ''FamilyFlavour )
-- $( derive makeJSON ''ResType )
-- $( derive makeJSON ''HsConDetails )
-- $( derive makeJSON ''HsExplicitFlag )
-- $( derive makeJSON ''ConDeclField )
-- $( derive makeJSON ''Boxity )
-- $( derive makeJSON ''HsSplice )
-- $( derive makeJSON ''HsBang )
-- $( derive makeJSON ''IPName )
-- $( derive makeJSON ''HsExpr )
--  
-- $( derive makeJSON ''HsLit)
-- $( derive makeJSON ''MatchGroup)
-- $( derive makeJSON ''HsStmtContext)
-- $( derive makeJSON ''HsBracket)
-- $( derive makeJSON ''Pat)
-- $( derive makeJSON ''HsWrapper)
-- $( derive makeJSON ''HsCmdTop)
-- $( derive makeJSON ''Fixity)
-- $( derive makeJSON ''HsArrAppType)
-- $( derive makeJSON ''ArithSeqInfo)
-- $( derive makeJSON ''HsRecFields )
-- $( derive makeJSON ''HsRecField )
-- $( derive makeJSON ''StmtLR )
-- $( derive makeJSON ''HsLocalBindsLR )
-- $( derive makeJSON ''HsTupArg )
-- $( derive makeJSON ''HsOverLit )
-- $( derive makeJSON ''OverLitVal )  
-- $( derive makeJSON ''HsValBindsLR )
-- $( derive makeJSON ''HsIPBinds )
-- $( derive makeJSON ''IPBind ) 
-- $( derive makeJSON ''FixitySig ) 
-- $( derive makeJSON ''InlinePragma ) 
-- $( derive makeJSON ''Match ) 
-- $( derive makeJSON ''Activation )
-- $( derive makeJSON ''RuleMatchInfo )
-- $( derive makeJSON ''InlineSpec )
-- $( derive makeJSON ''RecFlag )
-- $( derive makeJSON ''GRHSs )
-- $( derive makeJSON ''GRHS )
-- $( derive makeJSON ''FixityDirection )
-- $( derive makeJSON ''EvTerm )
-- $( derive makeJSON ''TcEvBinds )
-- $( derive makeJSON ''ForeignExport )
-- $( derive makeJSON ''ForeignImport )
-- $( derive makeJSON ''HsMatchContext )
-- $( derive makeJSON ''HsGroup )
-- $( derive makeJSON ''CExportSpec )
-- $( derive makeJSON ''CImportSpec )
-- $( derive makeJSON ''CCallConv )
-- $( derive makeJSON ''CCallTarget )
-- $( derive makeJSON ''EvBind )
-- $( derive makeJSON ''Safety )
-- $( derive makeJSON ''AnnProvenance )
-- $( derive makeJSON ''RuleBndr )
-- $( derive makeJSON ''TcSpecPrags )
-- $( derive makeJSON ''TcSpecPrag )
--
--instance (JSON a)=> JSON (IE a)
--        where
--            showJSON= showJSON . ieName 
--      
-- {--
--         p <- parseModule modSum
--        t <- typecheckModule p
--        d <- desugarModule t
--        l <- loadModule d
--        n <- getNamesInScope
--        c <- return $ coreModule d
-- 
--        g <- getModuleGraph
--        mapM showModule g     
--        return $ (parsedSource d,"/n-----/n",  typecheckedSource d)
--        --} 
--        
--{--
--instance ToJSON SrcLoc
--        where toJSON src 
--                | isGoodSrcLoc src =object [(pack $ unpackFS $ srcLocFile src) .= (Array $ fromList [toJSON $ srcLocLine src,toJSON $ srcLocCol src]) ]
--                | otherwise = Null
--        
--instance ToJSON SrcSpan
--        where toJSON src 
--                | isGoodSrcSpan src=object [(pack $ unpackFS $ srcSpanFile src) .= (Array $ fromList [toJSON $ srcSpanStartLine src,toJSON $ srcSpanStartCol src,toJSON $ srcSpanEndLine src,toJSON $ srcSpanEndCol src]) ]
--                | otherwise = Null
--                        
--instance(ToJSON a)=> ToJSON (Located a)
--        where toJSON (L s o)=case toJSON o of
--                Object o->Object (M.insert "Loc" (toJSON s) o)
--                Null -> Null
--                v->object ["Loc" .= (toJSON s),"Object" .= v]
--
--instance (ToJSON a, OutputableBndr a)=> ToJSON (HsModule a)
--        where toJSON hsm=object ["ModName" .= (toJSON $ hsmodName hsm),
--                "Exports" .= (toJSON $ hsmodExports hsm),
--                "Imports" .= (toJSON $ hsmodImports hsm),
--                "Decls" .= (toJSON $  hsmodDecls hsm)
--                ]
--
--instance ToJSON FastString
--        where toJSON=toJSON . pack . unpackFS
--       
--instance ToJSON ModuleName
--        where toJSON=toJSON . moduleNameString
--        
--instance ToJSON OccName
--        where toJSON=toJSON . showSDocDump . ppr
--        
--instance ToJSON RdrName
--        where 
--                toJSON (Unqual on) = Array $ fromList [toJSON on]
--                toJSON (Qual mn on)  = Array $ fromList [toJSON mn,toJSON on]
--
--instance (ToJSON a)=> ToJSON (IE a)
--        where
--            toJSON= toJSON . ieName --}
--            {--toJSON (IEVar name)= object ["IEVar" .= toJSON name]      
--            toJSON (IEThingAbs name)=object ["IEThingAbs" .= toJSON name]      
--            toJSON (IEThingAll name)= object ["IEThingAll" .= toJSON name]      
--            toJSON (IEThingWith name ns)= object ["IEVar" .= toJSON name]      
--            toJSON (IEModuleContents mn)= object ["IEModuleContents" .= toJSON name]      
--            toJSON (IEGroup i hds)= object ["IEVar" .= toJSON hds]      
--            toJSON (IEDoc hds)=  object ["IEDoc" .= toJSON hds]      
--            toJSON (IEDocNamed s)= object ["IEDocNamed" .= toJSON s]      --}
--{-- 
--instance (ToJSON a)=> ToJSON (ImportDecl a)
--        where
--            toJSON imd=object ["ModName" .= toJSON (ideclName imd),
--                "PackageQualified" .= toJSON (ideclPkgQual imd),
--                "Source" .= toJSON (ideclSource imd),
--                "Qualified" .= toJSON (ideclQualified imd),
--                "As" .= toJSON (ideclAs imd),
--                "Hiding" .= toJSON (ideclHiding imd)
--                ]
--
--instance (ToJSON a, OutputableBndr a)=> ToJSON (HsDecl a)
--        where toJSON =toJSON . showSDocDump . ppr
--        
--        --}