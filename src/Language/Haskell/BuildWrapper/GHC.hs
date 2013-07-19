{-# LANGUAGE CPP, OverloadedStrings, TypeSynonymInstances,StandaloneDeriving,DeriveDataTypeable,ScopedTypeVariables, MultiParamTypeClasses, PatternGuards, NamedFieldPuns  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Language.Haskell.BuildWrapper.GHC
-- Copyright   : (c) JP Moresmau 2011
-- License     : BSD3
-- 
-- Maintainer  : jpmoresmau@gmail.com
-- Stability   : beta
-- Portability : portable
-- 
-- Load relevant module in the GHC AST and get GHC messages and thing at point info. Also use the GHC lexer for syntax highlighting.
module Language.Haskell.BuildWrapper.GHC where
import Language.Haskell.BuildWrapper.Base hiding (Target,ImportExportType(..))
import Language.Haskell.BuildWrapper.GHCStorage
import Language.Haskell.BuildWrapper.Src

import Prelude hiding (readFile, writeFile)
import Data.Char
import Data.Generics hiding (Fixity, typeOf, empty)
import Data.Maybe
import Data.Monoid
import Data.Aeson

import Data.IORef
import qualified Data.List as List
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.HashMap.Lazy as HM

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC

import DynFlags
#if __GLASGOW_HASKELL__ > 704
import ErrUtils ( ErrMsg(..), WarnMsg, mkPlainErrMsg,Messages,ErrorMessages,WarningMessages,MsgDoc)
#else
import ErrUtils ( ErrMsg(..), WarnMsg, mkPlainErrMsg,Messages,ErrorMessages,WarningMessages,Message)
#endif
import GHC
import GHC.Paths ( libdir )
import HscTypes ( srcErrorMessages, SourceError, GhcApiError)
import Outputable
import FastString (FastString,unpackFS,concatFS,fsLit,mkFastString, lengthFS)
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
import Name (isTyVarName,isDataConName,isVarName,isTyConName)
import Var (varType)
import PprTyThing (pprTypeForUser)
import Control.Monad (when, liftM, unless)
import qualified Data.Vector as V (foldr)
import Module (moduleNameFS)
-- import System.Time (getClockTime, diffClockTimes, timeDiffToString)
import System.IO (hFlush, stdout)
import System.Directory (getModificationTime)
#if __GLASGOW_HASKELL__ < 706
import System.Time (ClockTime(TOD))
import Unsafe.Coerce (unsafeCoerce)

#else
import Data.Time.Clock (UTCTime(UTCTime))
import Data.Time.Calendar (Day(ModifiedJulianDay))
#endif
import Control.Exception (SomeException)
import Debugger (showTerm)
import Exception (gtry)

type GHCApplyFunction a=FilePath -> TypecheckedModule -> Ghc a

-- | get the GHC typechecked AST
getAST :: FilePath -- ^ the source file
        -> FilePath -- ^ the base directory
        ->  String -- ^ the module name 
        -> [String] -- ^ the GHC options 
        -> IO (OpResult (Maybe TypecheckedSource))
getAST fp base_dir modul opts=do
        (a,n)<-withASTNotes (\_ -> return . tm_typechecked_source) id base_dir (SingleFile fp modul) opts
        return (listToMaybe a,n) 

-- | perform an action on the GHC Typechecked module
withAST ::  (TypecheckedModule -> Ghc a) -- ^ the action
        -> FilePath -- ^ the source file
        -> FilePath -- ^ the base directory
        ->  String -- ^ the module name
        -> [String] -- ^ the GHC options
        -> IO (Maybe a)
withAST f fp base_dir modul options= do
        (a,_)<-withASTNotes (\_ ->f) id base_dir (SingleFile fp modul) options
        return $ listToMaybe a

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
                        mv2<-withAST gen fp base_dir modul options
                        case mv2 of
                                Just v2->fmap Just (f v2) 
                                Nothing-> return Nothing
        where gen tc=do
                df<-getSessionDynFlags
                return $ generateGHCInfo df tc 

-- | the main method loading the source contents into GHC
withASTNotes ::  GHCApplyFunction a -- ^ the final action to perform on the result
        -> (FilePath -> FilePath) -- ^ transform given file path to find bwinfo path
        -> FilePath -- ^ the base directory
        ->  LoadContents -- ^ what to load
        -> [String] -- ^ the GHC options 
        -> IO (OpResult [a])
withASTNotes f ff base_dir contents options=initGHC (ghcWithASTNotes f ff base_dir contents True) options

--        do
--    -- http://hackage.haskell.org/trac/ghc/ticket/7380#comment:1     : -O2 is removed from the options  
--    let cleaned=filter (not . List.isInfixOf "-O") options
--    let lflags=map noLoc cleaned
--    -- print cleaned
--    (_leftovers, _) <- parseStaticFlags lflags
--    runGhc (Just libdir) $ do
--        flg <- getSessionDynFlags
--        (flg', _, _) <- parseDynamicFlags flg _leftovers
--        GHC.defaultCleanupHandler flg' $ do
--                -- our options here
--                -- if we use OneShot, we need the other modules to be built
--                -- so we can't use hscTarget = HscNothing
--                -- and it takes a while to actually generate the o and hi files for big modules
--                -- if we use CompManager, it's slower for modules with lots of dependencies but we can keep hscTarget= HscNothing which makes it better for bigger modules
--                -- we use target interpreted so that it works with TemplateHaskell
--                setSessionDynFlags flg' {hscTarget = HscInterpreted, ghcLink = NoLink , ghcMode = CompManager}  
--                ghcWithASTNotes f ff base_dir contents
  
initGHC ::  Ghc a  
        -> [String] -- ^ the GHC options
        -> IO a 
initGHC f options=  do
    -- http://hackage.haskell.org/trac/ghc/ticket/7380#comment:1     : -O2 is removed from the options  
    let cleaned=filter (not . List.isInfixOf "-O") options
    let lflags=map noLoc cleaned
    -- print cleaned
    (_leftovers, _) <- parseStaticFlags lflags
    runGhc (Just libdir) $ do
        flg <- getSessionDynFlags
        (flg', _, _) <- parseDynamicFlags flg _leftovers
        GHC.defaultCleanupHandler flg' $ do
                -- our options here
                -- if we use OneShot, we need the other modules to be built
                -- so we can't use hscTarget = HscNothing
                -- and it takes a while to actually generate the o and hi files for big modules
                -- if we use CompManager, it's slower for modules with lots of dependencies but we can keep hscTarget= HscNothing which makes it better for bigger modules
                -- we use target interpreted so that it works with TemplateHaskell
                setSessionDynFlags flg' {hscTarget = HscInterpreted, ghcLink = NoLink , ghcMode = CompManager}  
                f            
               
ghcWithASTNotes   ::  
        GHCApplyFunction a -- ^ the final action to perform on the result
        -> (FilePath -> FilePath) -- ^ transform given file path to find bwinfo path
        -> FilePath -- ^ the base directory
        -> LoadContents -- ^ what to load
        -> Bool -- ^ add the target?
        -> Ghc (OpResult [a])         
ghcWithASTNotes  f ff base_dir contents shouldAddTargets= do            
                ref <- GMU.liftIO $ newIORef []
                cflg <- getSessionDynFlags
#if __GLASGOW_HASKELL__ > 704  
                setSessionDynFlags cflg  {log_action = logAction ref }
#else                
                setSessionDynFlags cflg  {log_action = logAction ref cflg }
#endif
                --  $ dopt_set (flg' { ghcLink = NoLink , ghcMode = CompManager }) Opt_ForceRecomp
                let fps=getLoadFiles contents
                when shouldAddTargets
                        (mapM_ (\(fp,_)-> addTarget Target { targetId = TargetFile fp Nothing, targetAllowObjCode = False, targetContents = Nothing }) fps)
                --c1<-GMU.liftIO getClockTime
--                let howMuch=case contents of
--                        SingleFile{lmModule=m}->LoadUpTo $ mkModuleName m
--                        MultipleFile{}->LoadAllTargets
                let howMuch=LoadAllTargets
                -- GMU.liftIO $ putStrLn "Loading..."
                sf<-load howMuch
                           `gcatch` (\(e :: SourceError) -> handle_error ref e)
                           `gcatch` (\(ae :: GhcApiError) -> do
                                        dumpError ref contents ae
                                        return Failed)
                            `gcatch` (\(se :: SomeException) -> do
                                        dumpError ref contents se
                                        return Failed)            
                -- GMU.liftIO $ putStrLn "Loaded..."           
                --(warns, errs) <- GMU.liftIO $ readIORef ref
                --let notes = ghcMessagesToNotes base_dir (warns, errs)
                
                --c2<-GMU.liftIO getClockTime
                --GMU.liftIO $ putStrLn ("load all targets: " ++ (timeDiffToString  $ diffClockTimes c2 c1))
                -- GMU.liftIO $ print fps
                a<-case sf of
                        Failed-> return []
                        _  ->fmap catMaybes $ mapM (\(fp,m)->(do
                                modSum <- getModSummary $ mkModuleName m
                                fmap Just $ workOnResult f fp modSum)
                               `gcatch` (\(se :: SourceError) -> do
                                        dumpError ref contents se
                                        return Nothing)
                               `gcatch` (\(ae :: GhcApiError) -> do
                                        dumpError ref contents ae
                                        return Nothing)
                                `gcatch` (\(se :: SomeException) -> do
                                        dumpError ref contents se
                                        return Nothing)        
                                ) fps
                notes <- GMU.liftIO $ readIORef ref                
#if __GLASGOW_HASKELL__ < 702                           
                warns <- getWarnings
                df <- getSessionDynFlags
                return (a,List.nub $ notes ++ reverse (ghcMessagesToNotes df base_dir (warns, emptyBag)))
#else
                return $ (a,List.nub $ notes)
#endif
        where
            processError :: LoadContents -> String -> Bool
            processError MultipleFile{} "Module not part of module graph"=False -- we ignore the error when we process several files and some we can't find
            processError _ _=True
            dumpError :: (Show a)=> IORef [BWNote] -> LoadContents -> a -> Ghc ()
            dumpError ref conts ae= when (processError conts (show ae)) (do
                                                GMU.liftIO $ print conts
                                                GMU.liftIO $ print ae 
                                                case conts of
                                                        (SingleFile fp _)->do
                                                             let relfp=makeRelative base_dir $ normalise fp
                                                             let notes=[BWNote BWError (show ae) (BWLocation relfp 1 1 1 1)]
                                                             GMU.liftIO $ modifyIORef ref $
                                                                \ ns -> ns ++ notes
                                                        _->return () 
                                                )
       
            workOnResult :: GHCApplyFunction a -> FilePath -> ModSummary -> Ghc a
            workOnResult f2 fp modSum= do
                p <- parseModule modSum
                t <- typecheckModule p
                d <- desugarModule t -- to get warnings
                l <- loadModule d
                --c3<-GMU.liftIO getClockTime
#if __GLASGOW_HASKELL__ < 704
                setContext [ms_mod modSum] []
#else
#if __GLASGOW_HASKELL__ < 706
                setContext [IIModule $ ms_mod modSum]
#else
                setContext [IIModule $ moduleName  $ ms_mod modSum]       
#endif             
#endif                         
                let fullfp=ff fp
                opts<-getSessionDynFlags
                -- GMU.liftIO $ putStrLn ("writing " ++ fullfp)
                GMU.liftIO $ storeGHCInfo opts fullfp (dm_typechecked_module l)
                --GMU.liftIO $ putStrLn ("parse, typecheck load: " ++ (timeDiffToString  $ diffClockTimes c3 c2))
                f2 fp $ dm_typechecked_module l                
        
            add_warn_err :: GhcMonad m => IORef [BWNote] -> WarningMessages -> ErrorMessages -> m()
            add_warn_err ref warns errs = do
              df <- getSessionDynFlags
              let notes = ghcMessagesToNotes df base_dir (warns, errs)
              GMU.liftIO $ modifyIORef ref $
                         \ ns -> ns ++ notes
        
            handle_error :: GhcMonad m => IORef [BWNote] -> SourceError -> m SuccessFlag
            handle_error ref e = do
               let errs = srcErrorMessages e
               add_warn_err ref emptyBag errs
               return Failed
#if __GLASGOW_HASKELL__ > 704              
            logAction :: IORef [BWNote] -> DynFlags -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
#else
            logAction :: IORef [BWNote] -> DynFlags -> Severity -> SrcSpan -> PprStyle -> Message -> IO ()
#endif
            logAction ref df s loc style msg
                | (Just status)<-bwSeverity s=do
                        let n=BWNote { bwnLocation = ghcSpanToBWLocation base_dir loc
                                 , bwnStatus = status
                                 , bwnTitle = removeBaseDir base_dir $ removeStatus status $ showSDUser (qualName style,qualModule style) df msg
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
ghcMessagesToNotes :: DynFlags -> 
        FilePath -- ^ base directory
        ->  Messages -- ^ GHC messages
        -> [BWNote]
ghcMessagesToNotes df base_dir (warns, errs) = map_bag2ms (ghcWarnMsgToNote df base_dir) warns ++
        map_bag2ms (ghcErrMsgToNote df base_dir) errs
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
                df<-getSessionDynFlags
                --c2<-GMU.liftIO getClockTime
                --GMU.liftIO $ putStrLn ("getNamesInScope: " ++ (timeDiffToString  $ diffClockTimes c2 c1))
                return $ map (showSDDump df . ppr ) names)  f base_dir modul options
        return $ fromMaybe[] names

   
-- | get all names in scope, packaged in NameDefs
getGhcNameDefsInScope  :: FilePath -- ^ source path
        -> FilePath -- ^ base directory
        -> String -- ^ module name
        -> [String] -- ^ build options
        -> IO (OpResult (Maybe [NameDef]))
getGhcNameDefsInScope fp base_dir modul options=do
        -- c0<-getClockTime
        (nns,ns)<-withASTNotes (\_ _->do
                -- c1<-GMU.liftIO getClockTime
                -- GMU.liftIO $ putStrLn "getGhcNameDefsInScope"
                names<-getNamesInScope
                df<-getSessionDynFlags
                -- c2<-GMU.liftIO getClockTime
                -- GMU.liftIO $ putStrLn ("getNamesInScope: " ++ (timeDiffToString  $ diffClockTimes c2 c1))
                mapM (name2nd df) names) id base_dir (SingleFile fp modul) options
        -- c4<-getClockTime
        -- putStrLn ("getNamesInScopeAll: " ++ (timeDiffToString  $ diffClockTimes c4 c0))
        return $ case nns of
                (x:_)->(Just x,ns)
                _->(Nothing, ns)
                
-- | get all names in scope, packaged in NameDefs
getGhcNameDefsInScopeLongRunning  :: FilePath -- ^ source path
        -> FilePath -- ^ base directory
        -> String -- ^ module name
        -> [String] -- ^ build options
        -> IO ()
getGhcNameDefsInScopeLongRunning fp base_dir modul options=do

#if __GLASGOW_HASKELL__ < 706
        initGHC (go (TOD 0 0)) options
        where 
                go :: 
                        ClockTime
                        -> Ghc ()
                go t1 = do
                        t2<- GMU.liftIO $ getModificationTime fp
                        let hasLoaded=case t1 of
                                TOD 0 _ -> False
                                _ -> True
#else
        initGHC (go (UTCTime (ModifiedJulianDay 0) 0)) options
        where 
                go :: 
                        UTCTime
                        -> Ghc ()
                go t1 = do
                        t2<- GMU.liftIO $ getModificationTime fp
                        let hasLoaded=case t1 of
                                UTCTime (ModifiedJulianDay 0) _ -> False
                                _ -> True
#endif

                        (ns1,add2)<-if hasLoaded && t2==t1 then -- modification time is only precise to the second in GHC 7.6 or above, see http://hackage.haskell.org/trac/ghc/ticket/7473
                                (do 
                                        removeTarget (TargetFile fp Nothing)      
                                        load LoadAllTargets
                                        return ([],True)
                                ) `gcatch` (\(e :: SourceError) -> do
                                        let errs = srcErrorMessages e
                                        df <- getSessionDynFlags
                                        return (ghcMessagesToNotes df base_dir (emptyBag, errs),True)
                                        )
                             else return ([],not hasLoaded)
                        (nns,ns)<- ghcWithASTNotes (\_ _->do
                                names<-getNamesInScope
                                df<-getSessionDynFlags
                                mapM (name2nd df) names) id base_dir (SingleFile fp modul) add2    
                        let res=case nns of
                                (x:_) -> (Just x,ns1 ++ ns)
                                _ -> (Nothing,ns1 ++ ns)
                        GMU.liftIO $ BSC.putStrLn $ BS.append "build-wrapper-json:" $ encode res
                        GMU.liftIO $ hFlush stdout
                        r1 t2
                r1 t2=do
                        l<- GMU.liftIO getLine 
                        case l of
                                "q"->return ()
                                -- eval an expression
                                'e':' ':expr->do
                                        s<-handleSourceError (return . show)
                                               (do
                                                rr<- runStmt expr RunToCompletion
                                                case rr of
                                                        RunOk ns->do
                                                                df<-getSessionDynFlags
                                                                ls<-mapM (\n->do
                                                                        mty<-lookupName n
                                                                        case mty of
                                                                                Just (AnId aid)->do
                                                                                        t<-gtry $ GHC.obtainTermFromId 100 False aid
                                                                                        case t of
                                                                                            Right term -> showTerm term
                                                                                            Left  exn  -> return (text "*** Exception:" <+>
                                                                                                                    text (show (exn :: SomeException)))
                                                                                _->return empty
                                                                        ) ns
                                                                return $ showSDDump df $ vcat ls
                                                        RunException e ->return $ show e
                                                        _->return "")
                                        GMU.liftIO $ BSC.putStrLn $ BS.append "build-wrapper-json:" $ encode s
                                        GMU.liftIO $ hFlush stdout
                                        r1 t2
                                "t"->do
                                       input<- GMU.liftIO $ readFile fp
                                       ett<-tokenTypesArbitrary' fp input (".lhs" == takeExtension fp)
                                       let ret= case ett of
                                                Right tt-> (tt,[])
                                                Left bw -> ([],[bw])
                                       GMU.liftIO $ do
                                                BSC.putStrLn $ BS.append "build-wrapper-json:" $ encode ret
                                                hFlush stdout
                                       r1 t2  
                                'p':xs->do
                                       GMU.liftIO $ do
                                                let (line,col)=read xs
                                                mv<-readGHCInfo fp
                                                let mm=case mv of 
                                                        Just v->let 
                                                                        f=overlap line (scionColToGhcCol col)
                                                                        mf=findInJSON f v
                                                                in findInJSONData mf 
                                                        _-> Nothing      
                                                BSC.putStrLn $ BS.append "build-wrapper-json:" $ encode (mm,[]::[BWNote])
                                                hFlush stdout
                                       r1 t2                    
                                _ ->go t2
                       
name2nd :: GhcMonad m=> DynFlags -> Name -> m NameDef
name2nd df n=do
        m<- getInfo n
        let ty=case m of
                Just (tyt,_,_)->ty2t tyt
                Nothing->Nothing
        return $ NameDef (T.pack $ showSDDump df $ ppr n) (name2t n) ty
        where         
              name2t :: Name -> [OutlineDefType]
              name2t n2 
                        | isTyVarName n2=[Type]
                        | isTyConName n2=[Type]
                        | isDataConName n2 = [Constructor]
                        | isVarName n2 = [Function]
                        | otherwise =[]
              ty2t :: TyThing -> Maybe T.Text
              ty2t (AnId aid)=Just $ T.pack $ showSD False df $ pprTypeForUser True $ varType aid
              ty2t (ADataCon dc)=Just $ T.pack $ showSD False df $ pprTypeForUser True $ dataConUserType dc
              ty2t _ = Nothing

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
                return $ findInJSONData mf  
            ) fp base_dir modul options
        return $ fromMaybe Nothing mmf
   
-- | get the "thing" at a particular point (line/column) in the source
-- this is using the saved JSON info if available
getLocalsJSON ::Int  -- ^ start line
        -> Int -- ^ start column
        -> Int  -- ^ end line
        -> Int -- ^ end column
        -> FilePath -- ^ source file path
        -> FilePath -- ^ base directory
        -> String  -- ^ module name
        -> [String] -- ^  build flags
        -> IO [ThingAtPoint]
getLocalsJSON sline scol eline ecol fp base_dir modul options= do
        mmf<-withJSONAST (\v->do
                let cont=contains sline (scionColToGhcCol scol) eline (scionColToGhcCol ecol)
                let isVar=isGHCType "Var"
                let mf=findAllInJSON (\x->cont x && isVar x) v
                return $ mapMaybe (findInJSONData . Just) mf  
            ) fp base_dir modul options
        return $ fromMaybe [] mmf  
  
  
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
          (enl,enc)=end sp
      in BWLocation (makeRelative baseDir $ foldr f [] $ normalise $ unpackFS (sfile sp))
                 stl
                 (ghcColToScionCol stc)
                 enl
                 (ghcColToScionCol enc)
  | otherwise = mkEmptySpan "" 1 1                 
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
                let prTS = lexTokenStreamH sb lexLoc dflags1
                case prTS of
                        POk _ toks      -> do
                                -- GMU.liftIO $ print $ map (show . unLoc) toks
                                return $ Right $ filter ofInterest toks
                        PFailed loc msg -> return $ Left $ ghcErrMsgToNote dflags1 base_dir $ 
#if __GLASGOW_HASKELL__ < 706
                                mkPlainErrMsg loc msg
#else
                                mkPlainErrMsg dflags1 loc msg
#endif

-- | Get a stream of tokens generated by the GHC lexer from the current document
ghctokensArbitrary' :: FilePath -- ^ The file path to the document
                   -> String -- ^ The document's contents
                   -> Ghc (Either BWNote [Located Token])
ghctokensArbitrary' base_dir contents= do
#if __GLASGOW_HASKELL__ < 702
        sb <- stringToStringBuffer contents
#else
        let sb=stringToStringBuffer contents
#endif

        dflags1 <- getSessionDynFlags
        let prTS = lexTokenStreamH sb lexLoc dflags1
        case prTS of
                POk _ toks      -> do
                        -- GMU.liftIO $ print $ map (show . unLoc) toks
                        return $ Right $ filter ofInterest toks
                PFailed loc msg -> return $ Left $ ghcErrMsgToNote dflags1 base_dir $ 
#if __GLASGOW_HASKELL__ < 706
                                mkPlainErrMsg loc msg
#else
                                mkPlainErrMsg dflags1 loc msg
#endif

-- | like lexTokenStream, but keep Haddock flag
lexTokenStreamH :: StringBuffer -> RealSrcLoc -> DynFlags -> ParseResult [Located Token]
lexTokenStreamH buf loc dflags = unP go initState
    where dflags' = dopt_set (dopt_set dflags Opt_KeepRawTokenStream) Opt_Haddock
          initState = mkPState dflags' buf loc
          go = do
            ltok <- lexer return
            case ltok of
              L _ ITeof -> return []
              _ -> liftM (ltok:) go


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

tokenTypesArbitrary' :: FilePath -> String -> Bool -> Ghc (Either BWNote [TokenDef])
tokenTypesArbitrary' projectRoot contents literate  = generateTokens' projectRoot contents literate convertTokens id
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
generateTokens projectRoot contents literate options  xform filterFunc =do
     let (ppTs, ppC) = preprocessSource contents literate
     -- putStrLn ppC
     result<-  ghctokensArbitrary projectRoot ppC options
     case result of 
       Right toks ->do
         let filterResult = filterFunc $ List.sortBy (comparing tdLoc) (ppTs ++ xform toks)
         return $ Right filterResult
       Left n -> return $ Left n
               
        
-- | Parse the current document, generating a TokenDef list, filtered by a function
generateTokens' :: FilePath                        -- ^ The project's root directory
               -> String                          -- ^ The current document contents, to be parsed
               -> Bool                            -- ^ Literate Haskell flag
               -> ([Located Token] -> [TokenDef]) -- ^ Transform function from GHC tokens to TokenDefs
               -> ([TokenDef] -> a)               -- ^ The TokenDef filter function
               -> Ghc (Either BWNote a)
generateTokens' projectRoot contents literate xform filterFunc =do
     let (ppTs, ppC) = preprocessSource contents literate
     -- putStrLn ppC
     result<-  ghctokensArbitrary' projectRoot ppC
     case result of 
       Right toks ->do
         let filterResult = filterFunc $ List.sortBy (comparing tdLoc) (ppTs ++ xform toks)
         return $ Right filterResult
       Left n -> return $ Left n     
     
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
                        | (ContinuePragma f2) <-f= addPPToken "P" (l,c) (ts2,"":l2,pragmaBehavior l f2)
                        | ('#':_)<-l =addPPToken "PP" (l,c) (ts2,l2,lineBehavior l f) 
                        | "{-# " `List.isPrefixOf` l=addPPToken "P" (l,c) (ts2,"":l2,pragmaBehavior l f) 
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
                                ContinuePragma p->p
                                Indent n->Indent (n+1)
                                _ -> Indent 1
                pragmaBehavior l f
                        | "-}" `List.isInfixOf` l = f
                        | otherwise = ContinuePragma f

data PPBehavior=Continue Int | Indent Int | Start | ContinuePragma PPBehavior
        deriving Eq

-- | convert a GHC error message to our note type
ghcErrMsgToNote :: DynFlags -> FilePath -> ErrMsg -> BWNote
ghcErrMsgToNote df= ghcMsgToNote df BWError

-- | convert a GHC warning message to our note type
ghcWarnMsgToNote :: DynFlags -> FilePath -> WarnMsg -> BWNote
ghcWarnMsgToNote df= ghcMsgToNote df BWWarning

-- Note that we do *not* include the extra info, since that information is
-- only useful in the case where we do not show the error location directly
-- in the source.
ghcMsgToNote :: DynFlags -> BWNoteStatus -> FilePath -> ErrMsg -> BWNote
ghcMsgToNote df note_kind base_dir msg =
    BWNote { bwnLocation = ghcSpanToBWLocation base_dir loc
         , bwnStatus = note_kind
         , bwnTitle = removeBaseDir base_dir $ removeStatus note_kind $ show_msg (errMsgShortDoc msg)
         }
  where
    loc | (s:_) <- errMsgSpans msg = s
        | otherwise                    = GHC.noSrcSpan
    unqual = errMsgContext msg
    show_msg = showSDUser unqual df

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
#if __GLASGOW_HASKELL__ < 706   
tokenType  ITocurlybar="SS"                 -- "{|", for type applications
tokenType  ITccurlybar="SS"                 -- "|}", for type applications
#endif
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
tokenType  (ITvarsym {})="VS"
tokenType  (ITconsym {})="IC"
tokenType  (ITqvarid {})="IV"
tokenType  (ITqconid {})="IC"
tokenType  (ITqvarsym {})="VS"
tokenType  (ITqconsym {})="IC"
tokenType  (ITprefixqvarsym {})="VS"
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
tokenType  (ITlineComment {})="C"     -- comment starting by "--"
tokenType  (ITblockComment {})="C"     -- comment in {- -}

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

-- 7.6 new token types 
#if __GLASGOW_HASKELL__ >= 706
tokenType ITctype= "P"
tokenType ITlcase= "S"
tokenType (ITqQuasiQuote {}) = "TH" -- [Qual.quoter| quote |]
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
       
type AliasMap=DM.Map ModuleName [ModuleName]


ghcImportToUsage :: T.Text -> LImportDecl Name ->  ([Usage],AliasMap) -> Ghc ([Usage],AliasMap)
ghcImportToUsage myPkg (L _ imp) (ls,moduMap)=(do
        let L src modu=ideclName imp
        pkg<-lookupModule modu (ideclPkgQual imp)
        df<-getSessionDynFlags
        let tmod=T.pack $ showSD True df $ ppr modu
            tpkg=T.pack $ showSD True df $ ppr $ modulePackageId pkg
            nomain=if tpkg=="main" then myPkg else tpkg
            subs=concatMap (ghcLIEToUsage df (Just nomain) tmod "import") $ maybe [] snd $ ideclHiding imp
            moduMap2=maybe moduMap (\alias->let
                mlmods=DM.lookup alias moduMap
                newlmods=case mlmods of
                        Just lmods->modu:lmods
                        Nothing->[modu]
                in DM.insert alias newlmods moduMap) $ ideclAs imp
            usg =Usage (Just nomain) tmod "" "import" False (toJSON $ ghcSpanToLocation src) False
        return (usg:subs++ls,moduMap2)
        )
        `gcatch` (\(se :: SourceError) -> do
                GMU.liftIO $ print se
                return ([],moduMap))
         
ghcLIEToUsage :: DynFlags -> Maybe T.Text -> T.Text -> T.Text -> LIE Name -> [Usage]
ghcLIEToUsage df tpkg tmod tsection (L src (IEVar nm))=[ghcNameToUsage df tpkg tmod tsection nm src False]
ghcLIEToUsage df tpkg tmod tsection (L src (IEThingAbs nm))=[ghcNameToUsage df tpkg tmod tsection nm src True ] 
ghcLIEToUsage df tpkg tmod tsection (L src (IEThingAll nm))=[ghcNameToUsage df tpkg tmod tsection nm src True] 
ghcLIEToUsage df tpkg tmod tsection (L src (IEThingWith nm cons))=ghcNameToUsage df tpkg tmod tsection nm src True :
        map (\ x -> ghcNameToUsage df tpkg tmod tsection x src False) cons 
ghcLIEToUsage _ tpkg tmod tsection (L src (IEModuleContents _))= [Usage tpkg tmod "" tsection False (toJSON $ ghcSpanToLocation src) False]              
ghcLIEToUsage _ _ _ _ _=[]
        
ghcExportToUsage :: DynFlags -> T.Text -> T.Text ->AliasMap -> LIE Name -> Ghc [Usage]        
ghcExportToUsage df myPkg myMod moduMap lie@(L _ name)=(do
        ls<-case name of
                (IEModuleContents modu)-> do
                        let realModus=fromMaybe [modu] (DM.lookup modu moduMap)
                        mapM (\modu2->do
                                pkg<-lookupModule modu2 Nothing
                                let tpkg=T.pack $ showSD True df $ ppr $ modulePackageId pkg
                                let tmod=T.pack $ showSD True df $ ppr modu2
                                return (tpkg,tmod)
                                ) realModus
                _ -> return [(myPkg,myMod)]
        return $ concatMap (\(tpkg,tmod)->ghcLIEToUsage df (Just tpkg) tmod "export" lie) ls
        )
        `gcatch` (\(se :: SourceError) -> do
                GMU.liftIO $ print se
                return [])
        
ghcNameToUsage ::  DynFlags -> Maybe T.Text -> T.Text -> T.Text -> Name -> SrcSpan -> Bool -> Usage 
ghcNameToUsage df tpkg tmod tsection nm src typ=Usage tpkg tmod (T.pack $ showSD False df $ ppr nm) tsection typ (toJSON $ ghcSpanToLocation src) False

type ImportMap=DM.Map T.Text (LImportDecl Name,[T.Text])

ghcImportMap :: LImportDecl Name -> Ghc ImportMap
ghcImportMap l@(L _ imp)=(do
        let L _ modu=ideclName imp
        let moduS=T.pack $ moduleNameString modu
        --GMU.liftIO $ putStrLn $ show moduS
        let mm=DM.singleton moduS (l,[])
        m<-lookupModule modu Nothing
        mmi<-getModuleInfo m
        df <- getSessionDynFlags
        let maybeHiding=ideclHiding imp
        let hidden=case maybeHiding of
                Just(True,ns)->map (T.pack . showSD False df . ppr . unLoc) ns
                _ ->[]
        let fullM =case mmi of
                Nothing -> mm
                Just mi->let
                        exps=modInfoExports mi
                        -- extExps=filter (\x->(nameModule x) /= m) exps
                        in foldr insertImport mm exps
                        where   insertImport :: Name -> ImportMap -> ImportMap
                                insertImport x mmx=
                                        let
                                                expM=T.pack $ moduleNameString $ moduleName $ nameModule x
                                                nT=T.pack $ showSD False df $ ppr x
                                        in if nT `elem` hidden 
                                                then  mmx 
                                                else DM.insertWith (\(_,xs1) (_,xs2)->(l,xs1++xs2)) expM (l,[nT]) mmx
        return $ if ideclImplicit imp
                then DM.insert "" (l,(concatMap snd $ DM.elems fullM)) fullM
                else fullM
        )
        `gcatch` (\(se :: SourceError) -> do
                GMU.liftIO $ print se
                return DM.empty)  
       
--getGHCOutline :: ParsedSource
--        -> [OutlineDef]
--getGHCOutline (L src mod)=concatMap ldeclOutline (hsmodDecls mod)
--        where 
--                ldeclOutline :: LHsDecl RdrName -> [OutlineDef]
--                ldeclOutline  (L src1 (TyClD decl))=ltypeOutline decl
--                ldeclOutline _ = []
--                ltypeOutline :: TyClDecl RdrName -> [OutlineDef]
--                ltypeOutline (TyFamily{tcdLName})=[mkOutlineDef (nameDecl $ unLoc tcdLName) [Type,Family] (ghcSpanToLocation $ getLoc tcdLName)]
--                ltypeOutline (TyData{tcdLName,tcdCons})=[mkOutlineDef (nameDecl $ unLoc tcdLName) [Data] (ghcSpanToLocation $ getLoc tcdLName)]
--                        ++ concatMap lconOutline tcdCons
--                lconOutline :: LConDecl RdrName -> [OutlineDef]
--                lconOutline (L src ConDecl{con_name,con_doc,con_details})=[(mkOutlineDef (nameDecl $ unLoc con_name) [Constructor] (ghcSpanToLocation $ getLoc con_name)){od_comment=commentDecl con_doc}]
--                        ++ detailOutline con_details
--                detailOutline (RecCon fields)=concatMap lfieldOutline fields
--                detailOutline _=[]
--                lfieldOutline (ConDeclField{cd_fld_name,cd_fld_doc})=[(mkOutlineDef (nameDecl $ unLoc cd_fld_name) [Function] (ghcSpanToLocation $ getLoc cd_fld_name)){od_comment=commentDecl cd_fld_doc}]
--                nameDecl:: RdrName -> T.Text
--                nameDecl (Unqual occ)=T.pack $ showSDoc $ ppr occ
--                nameDecl (Qual _ occ)=T.pack $ showSDoc $ ppr occ
--                commentDecl :: Maybe LHsDocString -> Maybe T.Text
--                commentDecl (Just st)=Just $ T.pack $ showSDoc $ ppr st
--                commentDecl _=Nothing
                -- ghcSpanToLocation

-- | module, function/type, constructors
type TypeMap=DM.Map T.Text (DM.Map T.Text (DS.Set T.Text))
type FinalImportValue=(LImportDecl Name,DM.Map T.Text (DS.Set T.Text))
type FinalImportMap=DM.Map T.Text FinalImportValue
     
     
-- | clean imports 
ghcCleanImports  :: FilePath -- ^ source path
        -> FilePath -- ^ base directory
        -> String -- ^ module name
        -> [String] -- ^ build options
        -> Bool -- ^ format?
        -> IO (OpResult [ImportClean])                 
ghcCleanImports f base_dir modul options doFormat  =  do
        (m,bwns)<-withASTNotes clean (base_dir </>) base_dir (SingleFile f modul) options
        return (if null m then [] else head m,bwns)
        where
                -- | main clean method: get the usage, the existing imports, and retrieve only the needed names for each import
                clean :: GHCApplyFunction [ImportClean]
                clean _ tm=do
                        let (_,imps,_,_)=fromJust $ tm_renamed_source tm
                        df <- getSessionDynFlags
                        let modu=T.pack $ showSD True df $ ppr $ moduleName $ ms_mod $ pm_mod_summary $ tm_parsed_module tm
                        let (Array vs)= generateGHCInfo df tm
                        impMaps<-mapM ghcImportMap imps
                        -- let impMap=DM.unions impMaps
                        let implicit=DS.fromList $ concatMap (maybe [] snd . (DM.lookup "")) impMaps
                        let allImps=concatMap DM.assocs impMaps
                        -- GMU.liftIO $ putStrLn $ show $ map (\(n,(_,ns))->(n,ns)) allImps
                        -- GMU.liftIO $ print implicit
                        let usgMap=V.foldr ghcValToUsgMap DM.empty vs
                        let usgMapWithoutMe=DM.delete modu usgMap
                        -- GMU.liftIO $ print usgMapWithoutMe
                        -- GMU.liftIO $ putStrLn $ show $ usgMapWithoutMe
                        --let ics=foldr (buildImportClean usgMapWithoutMe df) [] (DM.assocs impMap)
                        let ftm=foldr (buildImportCleanMap usgMapWithoutMe implicit) DM.empty allImps
                        
                        let missingCleans=getRemovedImports allImps ftm
                        let formatF=if doFormat then formatImports  else map (dumpImportMap df)
                        -- GMU.liftIO $ putStrLn $ show $ DM.keys ftm
                        let allCleans=formatF (DM.elems ftm) ++ missingCleans
                        return allCleans
                -- | all used names by module        
                ghcValToUsgMap :: Value -> TypeMap -> TypeMap
                ghcValToUsgMap (Object m) um |
                        Just (String n)<-HM.lookup "Name" m,
                        Just (String mo)<-HM.lookup "Module" m,
                        not $ T.null mo, -- ignore local objects
                        mst<-HM.lookup "Type" m,
                        Just (String ht)<-HM.lookup "HType" m
                                =let
                                        mm=DM.lookup mo um
                                        isType=ht=="t"
                                        isConstructor=not isType && isUpper (T.head n) && isJust mst
                                        key=if isConstructor
                                                then let
                                                        Just (String t)=mst
                                                     in fst $ T.breakOn " " $ T.strip $ snd $ T.breakOnEnd "->" t
                                                else n
                                        val=if isConstructor
                                                then DS.singleton n
                                                else DS.empty
                                 in case mm of
                                        Just usgM1->DM.insert mo (DM.insertWith DS.union key val usgM1) um
                                        Nothing->DM.insert mo (DM.singleton key val) um
                ghcValToUsgMap _ um=um
                -- | reconcile the usage map and the import to generate the final import map: module -> names to import
                buildImportCleanMap :: TypeMap -> DS.Set T.Text ->(T.Text,(LImportDecl Name,[T.Text])) -> FinalImportMap -> FinalImportMap
                buildImportCleanMap usgMap implicit (cmod,(l@(L _ imp),ns)) tm |
                          Just namesMap<-DM.lookup cmod usgMap, -- used names for module
                          namesMapFiltered<-foldr (keepKeys namesMap) DM.empty ns, -- only names really exported by the import name
                          namesWithoutImplicit<-if ideclQualified imp 
                                then namesMapFiltered
                                else DM.map (`DS.difference` implicit) $ foldr DM.delete namesMapFiltered $ DS.elems implicit,
                          not $ DM.null namesWithoutImplicit,
                          not $ ideclImplicit imp = let  -- ignore implicit prelude
                                L _ modu=ideclName imp
                                moduS=T.pack $ moduleNameString modu
                                in DM.insertWith mergeTypeMap moduS (l,namesWithoutImplicit) tm
                buildImportCleanMap _ _ _ tm = tm      
                -- | copy the key and value from one map to the other
                keepKeys :: Ord k => DM.Map k v -> k -> DM.Map k v -> DM.Map k v
                keepKeys m1 k m2=case DM.lookup k m1 of
                        Nothing -> m2
                        Just v1->DM.insert k v1 m2    
                -- | merge the map containing the set of names         
                mergeTypeMap :: FinalImportValue -> FinalImportValue -> FinalImportValue
                mergeTypeMap (l1,m1) (_,m2)= (l1,DM.unionWith DS.union m1 m2)        
                -- | generate final import string from names map    
                dumpImportMap :: DynFlags -> FinalImportValue -> ImportClean
                dumpImportMap df (L loc imp,ns)=let
                         txt= T.pack $ showSDDump df $ ppr (imp{ideclHiding=Nothing} :: ImportDecl Name)  -- rely on GHC for the initial bit of the import, without the names
                         nameList= T.intercalate ", " $ List.sortBy (comparing T.toLower) $ map buildName $ DM.assocs ns -- build explicit import list
                         full=txt `mappend` " (" `mappend` nameList `mappend` ")"
                         in ImportClean (ghcSpanToLocation loc) full
                pprName :: T.Text -> T.Text
                pprName n | T.null n =n
                          | isAlpha $ T.head n=n
                          | otherwise=T.concat ["(",n,")"]
                -- build the name with the constructors list if any
                buildName :: (T.Text,DS.Set T.Text)->T.Text
                buildName (n,cs) 
                        | DS.null cs=pprName n
                        | otherwise =let
                                nameList= T.intercalate ", " $ List.sortBy (comparing T.toLower) $ map pprName $ DS.toList cs
                                in (pprName n) `mappend` " (" `mappend` nameList `mappend` ")" 
                getRemovedImports :: [(T.Text,(LImportDecl Name,[T.Text]))] -> FinalImportMap -> [ImportClean]
                getRemovedImports allImps ftm= let 
                        cleanedLines=DS.fromList $ map (\(L l _,_)->iflLine $ifsStart $ ghcSpanToLocation l) $ DM.elems ftm
                        missingImps=filter (\(_,(L l imp,_))->not $ ideclImplicit imp || DS.member (iflLine $ifsStart $ ghcSpanToLocation l) cleanedLines) allImps
                        in nubOrd $ map (\(_,(L l _,_))-> ImportClean (ghcSpanToLocation l) "") missingImps
                getFormatInfo :: FinalImportValue -> (Int,Int,Int,Int,Int)->(Int,Int,Int,Int,Int)
                getFormatInfo (L _ imp,_) (szSafe,szQualified,szPkg,szName,szAs)=let
                        szSafe2=if ideclSafe imp then 5 else szSafe
                        szQualified2=if ideclQualified imp then 10 else szQualified
                        szPkg2=maybe szPkg (\p->max szPkg (3 + lengthFS p)) $ ideclPkgQual imp
                        L _ mo=ideclName imp
                        szName2=max szName (1 + lengthFS (moduleNameFS mo))
                        szAs2=maybe szAs (\m->max szAs (3 + lengthFS (moduleNameFS m))) $ ideclAs imp
                        in (szSafe2,szQualified2,szPkg2,szName2,szAs2)
                formatImport :: (Int,Int,Int,Int,Int)-> FinalImportValue -> ImportClean
                formatImport (szSafe,szQualified,szPkg,szName,szAs) (L loc imp,ns) =let
                        st="import "
                        saf=if ideclSafe imp then "safe " else T.justifyLeft szSafe ' ' ""
                        qual=if ideclQualified imp then "qualified " else T.justifyLeft szQualified ' ' ""
                        pkg=maybe (T.justifyLeft szPkg ' ' "") (\p->"\"" `mappend` T.pack (unpackFS p) `mappend` "\" ") $ ideclPkgQual imp
                        L _ mo=ideclName imp
                        nm=T.justifyLeft szName ' ' $ T.pack $ moduleNameString mo
                        ast=maybe (T.justifyLeft szAs ' ' "") (\m->"as " `mappend` T.pack (moduleNameString m)) $ ideclAs imp
                        nameList= T.intercalate ", " $ List.sortBy (comparing T.toLower) $ map buildName $ DM.assocs ns -- build explicit import list
                        full=st `mappend` saf `mappend` qual `mappend` pkg `mappend` nm `mappend` ast `mappend` " (" `mappend` nameList `mappend` ")"
                        in ImportClean (ghcSpanToLocation loc) full
                formatImports :: [FinalImportValue] -> [ImportClean]
                formatImports fivs = let
                        formatInfo=foldr getFormatInfo (0,0,0,0,0) fivs
                        in map (formatImport formatInfo) fivs
                        
                       