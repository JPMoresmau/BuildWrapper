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
-- Load relevant module in the GHC AST and get GHC messages and thing at point info. Also use the GHC lexer for syntax highlighting.
module Language.Haskell.BuildWrapper.GHC where
import Language.Haskell.BuildWrapper.Base hiding (Target)
import Language.Haskell.BuildWrapper.GHCStorage

import Data.Char
import Data.Generics hiding (Fixity, typeOf)
import Data.Maybe
import Data.Monoid
import Data.Aeson

import Data.IORef
import qualified Data.List as List
import Data.Ord (comparing)
import qualified Data.Text as T

import DynFlags
import ErrUtils ( ErrMsg(..), WarnMsg, mkPlainErrMsg,Messages,ErrorMessages,WarningMessages, Message )
import GHC
import GHC.Paths ( libdir )
import HscTypes ( srcErrorMessages, SourceError)
import Outputable
import FastString (FastString,unpackFS,concatFS,fsLit,mkFastString)
import Lexer hiding (loc)
import Bag

#if __GLASGOW_HASKELL__ >= 702
import SrcLoc
#endif

#if __GLASGOW_HASKELL__ >= 610
import StringBuffer
#endif

import System.FilePath

import qualified MonadUtils as GMU


-- | get the GHC typechecked AST
getAST :: FilePath -- ^ the source file
        -> FilePath -- ^ the base directory
        ->  String -- ^ the module name 
        -> [String] -- ^ the GHC options 
        -> IO (OpResult (Maybe TypecheckedSource))
getAST =withASTNotes (return . tm_typechecked_source
        )

-- | perform an action on the GHC Typechecked module
withAST ::  (TypecheckedModule -> Ghc a) -- ^ the action
        -> FilePath -- ^ the source file
        -> FilePath -- ^ the base directory
        ->  String -- ^ the module name 
        -> [String] -- ^ the GHC options
        -> IO (Maybe a)
withAST f fp base_dir modul options= do
        (a,_)<-withASTNotes f fp base_dir modul options
        return a

-- | perform an action on the GHC JSON AST
withJSONAST :: (Value -> IO a) -- ^ the action
        -> FilePath -- ^ the source file
        -> FilePath -- ^ the base directory
        ->  String -- ^ the module name 
        -> [String] -- ^ the GHC options
        -> IO (Maybe a)
withJSONAST f fp base_dir modul options=do
        mv<-readGHCInfo fp
        case mv of 
                Just v-> fmap Just (f v) 
                Nothing->do
                        (mTc,_)<-getAST fp base_dir modul options
                        case mTc of
                                Just tc->fmap Just (f (dataToJSON tc)) 
                                Nothing -> return Nothing

-- | the main method loading the source contents into GHC
withASTNotes ::  (TypecheckedModule -> Ghc a) -- ^ the final action to perform on the result
         -> FilePath -- ^ the source file
        -> FilePath -- ^ the base directory
        ->  String -- ^ the module name 
        -> [String] -- ^ the GHC options 
        -> IO (OpResult (Maybe a))
withASTNotes f fp base_dir modul options=do
    let lflags=map noLoc options
    --print options
    (_leftovers, _) <- parseStaticFlags lflags
    runGhc (Just libdir) $ do
        flg <- getSessionDynFlags
        (flg', _, _) <- parseDynamicFlags flg _leftovers
        GHC.defaultCleanupHandler flg' $ do
                ref <- GMU.liftIO $ newIORef []
                -- our options here
                -- if we use OneShot, we need the other modules to be built
                -- so we can't use hscTarget = HscNothing
                -- and it takes a while to actually generate the o and hi files for big modules
                -- if we use CompManager, it's slower for modules with lots of dependencies but we can keep hscTarget= HscNothing which makes it better for bigger modules
                setSessionDynFlags flg'  {hscTarget = HscNothing,  ghcLink = NoLink , ghcMode = CompManager, log_action = logAction ref }
                --  $ dopt_set (flg' { ghcLink = NoLink , ghcMode = CompManager }) Opt_ForceRecomp
                addTarget Target { targetId = TargetFile fp Nothing, targetAllowObjCode = True, targetContents = Nothing }
                --c1<-GMU.liftIO getClockTime
                let modName=mkModuleName modul
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
                                modSum <- getModSummary modName
                                p <- parseModule modSum
                                --return $ showSDocDump $ ppr $ pm_mod_summary p
                                t <- typecheckModule p
                                d <- desugarModule t -- to get warnings
                                l <- loadModule d
                                --c3<-GMU.liftIO getClockTime
#if __GLASGOW_HASKELL__ < 704
                                setContext [ms_mod modSum] []
#else
                                setContext [IIModule $ ms_mod modSum]
#endif                                
                                GMU.liftIO $ storeGHCInfo fp (typecheckedSource $ dm_typechecked_module l)
                                --GMU.liftIO $ putStrLn ("parse, typecheck load: " ++ (timeDiffToString  $ diffClockTimes c3 c2))
                                a<-f (dm_typechecked_module l)
#if __GLASGOW_HASKELL__ < 702                           
                                warns <- getWarnings
                                return (Just a,List.nub $ notes ++ reverse (ghcMessagesToNotes base_dir (warns, emptyBag)))
#else
                                notes2 <- GMU.liftIO $ readIORef ref
                                return $ (Just a,List.nub $ notes2)
#endif
                        Failed -> return (Nothing, notes)
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
                         \ ns -> ns ++ notes
        
            handle_error :: GhcMonad m => IORef [BWNote] -> SourceError -> m SuccessFlag
            handle_error ref e = do
               let errs = srcErrorMessages e
               add_warn_err ref emptyBag errs
--               warns <- getWarnings
--               add_warn_err ref warns errs
--               clearWarnings
               return Failed
               
            logAction :: IORef [BWNote] -> Severity -> SrcSpan -> PprStyle -> Message -> IO ()
            logAction ref s loc style msg
                | (Just status)<-bwSeverity s=do
                        let n=BWNote { bwn_location = ghcSpanToBWLocation base_dir loc
                                 , bwn_status = status
                                 , bwn_title = removeBaseDir base_dir $ removeStatus status $ showSDocForUser (qualName style,qualModule style) msg
                                 }
                        modifyIORef ref $  \ ns -> ns ++ [n]
                | otherwise=return ()
            
            bwSeverity :: Severity -> Maybe BWNoteStatus
            bwSeverity SevWarning = Just BWWarning       
            bwSeverity SevError   = Just BWError
            bwSeverity SevFatal   = Just BWError
            bwSeverity _          = Nothing
            
   
-- | Convert 'GHC.Messages' to '[BWNote]'.
--
-- This will mix warnings and errors, but you can split them back up
-- by filtering the '[BWNote]' based on the 'bw_status'.
ghcMessagesToNotes :: FilePath -- ^ base directory
        ->  Messages -- ^ GHC messages
        -> [BWNote]
ghcMessagesToNotes base_dir (warns, errs) = map_bag2ms (ghcWarnMsgToNote base_dir) warns ++
        map_bag2ms (ghcErrMsgToNote base_dir) errs
  where
    map_bag2ms f =  map f . Bag.bagToList   
   
   
-- | get all names in scope
getGhcNamesInScope  :: FilePath -- ^ source path
        -> FilePath -- ^ base directory
        -> String -- ^ module name
        -> [String] -- ^ build options
        -> IO [String]
getGhcNamesInScope f base_dir modul options=do
        names<-withAST (\_->do
                --c1<-GMU.liftIO getClockTime
                names<-getNamesInScope
                --c2<-GMU.liftIO getClockTime
                --GMU.liftIO $ putStrLn ("getNamesInScope: " ++ (timeDiffToString  $ diffClockTimes c2 c1))
                return $ map (showSDocDump . ppr ) names)  f base_dir modul options
        return $ fromMaybe[] names

-- | get the "thing" at a particular point (line/column) in the source
-- this is using the saved JSON info if available
getThingAtPointJSON :: Int -- ^ line
        -> Int -- ^ column
--        -> Bool  ^ do we want the result qualified by the module
--        -> Bool  ^ do we want the full type or just the haddock type
        -> FilePath -- ^ source file path
        -> FilePath -- ^ base directory
        -> String  -- ^ module name
        -> [String] -- ^  build flags
        -> IO (Maybe ThingAtPoint)
getThingAtPointJSON line col fp base_dir modul options= do
        mmf<-withJSONAST (\v->do
                let f=overlap line (scionColToGhcCol col)
                let mf=findInJSON f v
                --return $ findInJSONFormatted qual typed mf
                return $ findInJSONData mf  
            ) fp base_dir modul options
        return $ fromMaybe Nothing mmf
   
  
-- | convert a GHC SrcSpan to a Span,  ignoring the actual file info
ghcSpanToLocation ::GHC.SrcSpan
                  -> InFileSpan
ghcSpanToLocation sp
  | GHC.isGoodSrcSpan sp =let
      (stl,stc)=start sp
      (enl,enc)=end sp
      in mkFileSpan 
                 stl
                 (ghcColToScionCol stc)
                 enl
                 (ghcColToScionCol enc)
  | otherwise = mkFileSpan 0 0 0 0

   
-- | convert a GHC SrcSpan to a BWLocation   
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
                sfile = GHC.srcSpanFile
#else 
                sfile (RealSrcSpan ss)= GHC.srcSpanFile ss
#endif 
  
-- | convert a column info from GHC to our system (1 based)                      
ghcColToScionCol :: Int -> Int
#if __GLASGOW_HASKELL__ < 700
ghcColToScionCol c=c+1 -- GHC 6.x starts at 0 for columns
#else
ghcColToScionCol c=c -- GHC 7 starts at 1 for columns
#endif

-- | convert a column info from our system (1 based) to GHC      
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
                        POk _ toks      -> return $ Right $ filter ofInterest toks
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
ofInterest (L loc _) =
  let  (sl,sc) = start loc
       (el,ec) = end loc
  in (sl < el) || (sc < ec)

       
-- | Convert a GHC token to an interactive token (abbreviated token type)
tokenToType :: Located Token -> TokenDef
tokenToType (L sp t) = TokenDef (tokenType t) (ghcSpanToLocation sp)       
        
-- | Generate the interactive token list used by EclipseFP for syntax highlighting
tokenTypesArbitrary :: FilePath -> String -> Bool -> [String] -> IO (Either BWNote [TokenDef])
tokenTypesArbitrary projectRoot contents literate options = generateTokens projectRoot contents literate options convertTokens id
  where
    convertTokens = map tokenToType        
        
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
      mkToken (L sp t)=TokenDef (tokenValue qualif t) (ghcSpanToLocation sp)
  in generateTokens projectRoot contents literate options (map mkToken) tokensMatching        
        
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
                 let filterResult = filterFunc $ List.sortBy (comparing td_loc) (ppTs ++ xform toks)
                 --liftIO $ putStrLn $ show tokenList
                 in return $ Right filterResult
               Left n -> return $ Left n
               )
     
-- | Preprocess some source, returning the literate and Haskell source as tuple.
preprocessSource ::  String -- ^ the source contents
        -> Bool -- ^ is the source literate Haskell
        -> ([TokenDef],String) -- ^ the preprocessor tokens and the final valid Haskell source
preprocessSource contents literate=
        let 
                (ts1,s2)=if literate then ppSF contents ppSLit else ([],contents) 
                (ts2,s3)=ppSF s2 ppSCpp
        in (ts1++ts2,s3)
        where
                ppSF contents2 p= let
                        linesWithCount=zip (lines contents2) [1..]
                        (ts,nc,_)= List.foldl' p ([],[],Start) linesWithCount
                        in (reverse ts, unlines $ reverse nc)
                ppSCpp :: ([TokenDef],[String],PPBehavior) -> (String,Int) -> ([TokenDef],[String],PPBehavior)
                ppSCpp (ts2,l2,f) (l,c) 
                        | (Continue _)<-f = addPPToken "PP" (l,c) (ts2,l2,lineBehavior l f)
                        | ('#':_)<-l =addPPToken "PP" (l,c) (ts2,l2,lineBehavior l f) 
                        | "{-# " `List.isPrefixOf` l=addPPToken "D" (l,c) (ts2,"":l2,f) 
                        | (Indent n)<-f=(ts2,l:(replicate n (takeWhile (== ' ') l) ++ l2),Start)
                        | otherwise =(ts2,l:l2,Start)
                ppSLit :: ([TokenDef],[String],PPBehavior) -> (String,Int) -> ([TokenDef],[String],PPBehavior)
                ppSLit (ts2,l2,f) (l,c) 
                        | "\\begin{code}" `List.isPrefixOf` l=addPPToken "DL" ("\\begin{code}",c) (ts2,"":l2,Continue 1)
                        | "\\end{code}" `List.isPrefixOf` l=addPPToken "DL" ("\\end{code}",c) (ts2,"":l2,Start)
                        | (Continue n)<-f = (ts2,l:l2,Continue (n+1))
                        | ('>':lCode)<-l=(ts2, (' ':lCode ):l2,f)
                        | otherwise =addPPToken "DL" (l,c) (ts2,"":l2,f)  
                addPPToken :: T.Text -> (String,Int) -> ([TokenDef],[String],PPBehavior) -> ([TokenDef],[String],PPBehavior)
                addPPToken name (l,c) (ts2,l2,f) =(TokenDef name (mkFileSpan c 1 c (length l + 1)) : ts2 ,l2,f)
                lineBehavior l f 
                        | '\\' == last l = case f of
                                Continue n->Continue (n+1)
                                _ -> Continue 1
                        | otherwise = case f of
                                Continue n->Indent (n+1)
                                Indent n->Indent (n+1)
                                _ -> Indent 1

data PPBehavior=Continue Int | Indent Int | Start
        deriving Eq

-- | convert a GHC error message to our note type
ghcErrMsgToNote :: FilePath -> ErrMsg -> BWNote
ghcErrMsgToNote = ghcMsgToNote BWError

-- | convert a GHC warning message to our note type
ghcWarnMsgToNote :: FilePath -> WarnMsg -> BWNote
ghcWarnMsgToNote = ghcMsgToNote BWWarning

-- Note that we do *not* include the extra info, since that information is
-- only useful in the case where we do not show the error location directly
-- in the source.
ghcMsgToNote :: BWNoteStatus -> FilePath -> ErrMsg -> BWNote
ghcMsgToNote note_kind base_dir msg =
    BWNote { bwn_location = ghcSpanToBWLocation base_dir loc
         , bwn_status = note_kind
         , bwn_title = removeBaseDir base_dir $ removeStatus note_kind $ show_msg (errMsgShortDoc msg)
         }
  where
    loc | (s:_) <- errMsgSpans msg = s
        | otherwise                    = GHC.noSrcSpan
    unqual = errMsgContext msg
    show_msg = showSDocForUser unqual

-- | remove the initial status text from a message
removeStatus :: BWNoteStatus -> String -> String
removeStatus BWWarning s 
        | "Warning:" `List.isPrefixOf` s = List.dropWhile isSpace $ drop 8 s
        | otherwise = s
removeStatus BWError s 
        | "Error:" `List.isPrefixOf` s = List.dropWhile isSpace $ drop 6 s
        | otherwise = s        

#if CABAL_VERSION == 106
deriving instance Typeable StringBuffer
deriving instance Data StringBuffer
#endif

-- | make unqualified token
mkUnqualTokenValue :: FastString -- ^ the name
                   -> T.Text
mkUnqualTokenValue = T.pack . unpackFS

-- | make qualified token: join the qualifier and the name by a dot
mkQualifiedTokenValue :: FastString -- ^ the qualifier
                      -> FastString -- ^ the name
                      -> T.Text
mkQualifiedTokenValue q a = (T.pack . unpackFS . concatFS) [q, dotFS, a]

-- | Make a token definition from its source location and Lexer.hs token type.
--mkTokenDef :: Located Token -> TokenDef
--mkTokenDef (L sp t) = TokenDef (mkTokenName t) (ghcSpanToLocation sp)

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
#if __GLASGOW_HASKELL__ < 704
tokenType  ITvarQuote="TH"                  --  '
#endif
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

  -- 7.4 new token types 
#if __GLASGOW_HASKELL__ >= 704
tokenType ITcapiconv= "EK"
tokenType ITnounpack_prag= "P"
tokenType ITtildehsh= "S"
tokenType ITsimpleQuote="SS"
#endif

dotFS :: FastString
dotFS = fsLit "."

tokenValue :: Bool -> Token -> T.Text
tokenValue _ t | tokenType t `elem` ["K", "EK"] = T.drop 2 $ mkTokenName t
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
        
start, end :: SrcSpan -> (Int,Int)   
#if __GLASGOW_HASKELL__ < 702   
start ss= (srcSpanStartLine ss, srcSpanStartCol ss)
end ss= (srcSpanEndLine ss, srcSpanEndCol ss)
#else 
start (RealSrcSpan ss)= (srcSpanStartLine ss, srcSpanStartCol ss)
start (UnhelpfulSpan _)=error "UnhelpfulSpan in cmpOverlap start"
end (RealSrcSpan ss)= (srcSpanEndLine ss, srcSpanEndCol ss)   
end (UnhelpfulSpan _)=error "UnhelpfulSpan in cmpOverlap start"   
#endif
        
        
