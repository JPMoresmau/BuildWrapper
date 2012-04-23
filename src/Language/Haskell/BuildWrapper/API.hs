{-# LANGUAGE CPP, ScopedTypeVariables, OverloadedStrings, PatternGuards #-}
-- |
-- Module      : Language.Haskell.BuildWrapper.API
-- Author      : JP Moresmau
-- Copyright   : (c) JP Moresmau 2011
-- License     : BSD3
-- 
-- Maintainer  : jpmoresmau@gmail.com
-- Stability   : beta
-- Portability : portable
-- 
-- API entry point, with all exposed methods
module Language.Haskell.BuildWrapper.API where

import Distribution.Simple.LocalBuildInfo (localPkgDescr)
import Distribution.Package (packageId)
import Distribution.Text (display)
import qualified Data.Aeson.Types as Data.Aeson.Types (parse)
import Data.Aeson.Types (Parser)
import Language.Haskell.BuildWrapper.Base
import Language.Haskell.BuildWrapper.Cabal
import qualified Language.Haskell.BuildWrapper.GHC as BwGHC
import Language.Haskell.BuildWrapper.GHCStorage
import Language.Haskell.BuildWrapper.Src

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import qualified MonadUtils as GMU
import Prelude hiding (readFile, writeFile)
import qualified Data.Vector as V
import Data.Attoparsec.Number (Number(I))

import System.IO.UTF8

import Control.Monad.State
import Language.Haskell.Exts.Annotated hiding (String)
import Language.Preprocessor.Cpphs
import Data.Maybe
import System.Directory
import System.FilePath
import GHC (RenamedSource, TypecheckedSource, TypecheckedModule(..), Ghc, ms_mod, pm_mod_summary, moduleName)
import qualified GHC  as GHC (Module)
import Data.Tuple (swap)
import Data.Aeson
import Outputable (showSDoc,ppr)

-- | copy all files from the project to the temporary folder
synchronize ::  Bool -- ^ if true copy all files, if false only copy files newer than their corresponding temp files
        -> BuildWrapper(OpResult ([FilePath],[FilePath])) -- ^ return the list of files copied, the list of files deleted
synchronize force =do
        cf<-gets cabalFile
        (fileList,ns)<-getFilesToCopy
        let fullFileList=takeFileName cf :
                "Setup.hs":
                "Setup.lhs":
                fileList
        m1<-mapM (copyFromMain force) fullFileList
        del<-deleteGhosts fullFileList
        return ((catMaybes m1,del), ns)

-- | synchronize one file only
synchronize1 ::  Bool -- ^ always copy the file, if false only copy the file if it is newer than its corresponding temp file
        -> FilePath -- ^ the source file in the project folder
        -> BuildWrapper(Maybe FilePath) -- ^ return Nothing if no copy or Just file if copied
synchronize1 force fp = do
        m1<-mapM (copyFromMain force) [fp]
        return $ head m1

-- | write contents to temporary file
write ::  FilePath -- ^ the source file in the project folder 
        -> String -- ^ the contents 
        -> BuildWrapper()
write fp s= do
        real<-getTargetPath fp
        --liftIO $ putStrLn ("contents:"++s)
        liftIO $ writeFile real s

-- | run cabal configure
configure ::  WhichCabal -- ^ use the source or temp cabal 
        -> BuildWrapper (OpResult Bool) -- ^ True if configure succeeded
configure which= do
        (mlbi,msgs)<-cabalConfigure which
        return (isJust mlbi,msgs)

-- | run cabal build
build :: Bool -- ^ do we want output (True) or just compilation without linking?
        -> WhichCabal -- ^ use original cabal or temp cabal file
        -> BuildWrapper (OpResult BuildResult)
build = cabalBuild

generateAST :: CabalComponent -> BuildWrapper()
generateAST cc= do
        (_,ns)<-withCabal Source (\lbi -> do 
                cbis<-getAllFiles lbi
                cf<-gets cabalFile
                temp<-getFullTempDir
                let dir=takeDirectory cf
                let pkg=T.pack $ display $ packageId $ localPkgDescr lbi
                mapM_ (\cbi->do
                        let 
                                mps=map (\(m,f)->(f,moduleToString m)) $ cbiModulePaths cbi
                        opts<-fmap snd $ fileGhcOptions (lbi,cbi)        
                        modules<-liftIO $ do
                                cd<-getCurrentDirectory
                                setCurrentDirectory dir
                                (mods,ns)<-BwGHC.withASTNotes (getModule pkg) (temp </>) dir (MultipleFile mps) opts     
                                setCurrentDirectory cd 
                                return mods
                        mapM_ (generate pkg) modules
                        ) $ filter (\cbi->cbiComponent cbi==cc) cbis
                )
        -- liftIO $ Prelude.print ns        
        return ()
        where
                getModule :: T.Text ->  FilePath -> TypecheckedModule -> Ghc(FilePath,T.Text,RenamedSource,[Usage])
                getModule pkg f tm=do
                        let rs@(_,imps,mexps,_)=fromJust $ tm_renamed_source tm
                        ius<-mapM (BwGHC.ghcImportToUsage pkg) imps
                        let modu=T.pack $ showSDoc $ ppr $ moduleName $ ms_mod $ pm_mod_summary $ tm_parsed_module tm
                        eus<-mapM (BwGHC.ghcExportToUsage pkg modu) (fromMaybe [] mexps)
                        --ms_mod $ pm_mod_summary $ tm_parsed_module tm
                        return (f,modu,rs,concat $ ius ++ eus)
                generate :: T.Text -> (FilePath,T.Text,RenamedSource,[Usage]) -> BuildWrapper()
                generate pkg (fp,modu,(hsg,_,_,_),ius)=do
                        tgt<-getTargetPath fp
                        --mv<-liftIO $ readGHCInfo tgt
                        let v = dataToJSON hsg
                        -- liftIO $ Prelude.putStrLn tgt
                        -- liftIO $ Prelude.putStrLn $ formatJSON $ BSC.unpack $ encode v
                        --case mv of
                        --        Just v->do
                        let vals=catMaybes $ extractUsages v
                        --liftIO $ mapM_ (Prelude.putStrLn . formatJSON . BSC.unpack . encode) vals
                        (mast,ns)<-getAST fp
                        case mast of
                                Just (ParseOk ast)->do
                                        let ods=getHSEOutline ast
                                        let (es,_)=getHSEImportExport ast
                                        let val=reconcile pkg vals ods es ius
                                        let valWithModule=Array $ V.fromList [toJSON pkg,toJSON modu,val]
                                        liftIO $ setUsageInfo tgt valWithModule
                                        return ()
                                _ -> return ()
                        return ()
                        --        Nothing -> do
                        --               liftIO $ Prelude.putStrLn "no ghc info"
                        --                return ()
                        return ()
                reconcile :: T.Text ->  [Value] -> [OutlineDef] -> [ExportDef] -> [Usage] -> Value
                reconcile pkg vals ods es ius=foldr usageToJSON (object []) 
                        (ius ++ (concatMap (ghcValToUsage pkg) vals))
                usageToJSON :: Usage -> Value -> Value
                usageToJSON u v@(Object pkgs) | Just pkg<-usagePackage u v=
                        let 
                                
                                (Object mods) = HM.lookupDefault (object []) pkg pkgs
                                (Object types)= HM.lookupDefault (object []) (usModule u) mods
                                typeKey=if usType u
                                        then "types"
                                        else "vars"
                                (Object names)= HM.lookupDefault (object []) typeKey  types     
                                nameKey=usName u
                                --  , ",", (usType u)
                                (Array lines)= HM.lookupDefault (Array V.empty) nameKey names
                                lineV= usLoc u  -- Number $ I $ usLine u
                                lines2=if V.elem lineV lines
                                        then lines
                                        else V.cons lineV lines
                                names2=HM.insert nameKey (Array lines2) names
                                types2=HM.insert typeKey (Object names2) types
                                mods2=HM.insert (usModule u) (Object types2) mods
                       in Object $ HM.insert pkg (Object mods2) pkgs
                        
--                        let r=Data.Aeson.Types.parse (\(Object pkgs)-> do
--                                (Object mods) <- pkgs .:? (usPackage u) .!= object []
--                                (Object types)<- mods .:? (usModule u) .!= object []
--                                let typeKey=if usType u
--                                        then "types"
--                                        else "vars"
--                                (Object names)<- types .:? typeKey .!= object []        
--                                let nameKey=usName u
--                                --  , ",", (usType u)
--                                (Array lines)<- names .:?  nameKey .!= (Array V.empty)
--                                let lineV= usLoc u  -- Number $ I $ usLine u
--                                let lines2=if V.elem lineV lines
--                                        then lines
--                                        else V.cons lineV lines
--                                let names2=HM.insert nameKey (Array lines2) names
--                                let types2=HM.insert typeKey (Object names2) types
--                                let mods2=HM.insert (usModule u) (Object types2) mods
--                                return $ Object $ HM.insert (usPackage u) (Object mods2) pkgs
--                                ) o
--                        in case r of
--                                Error st->object ["error" .= st]
--                                Success a->a
                usageToJSON _ a=a
                usagePackage ::  Usage -> Value -> Maybe T.Text
                usagePackage u (Object pkgs)=case usPackage u of
                        Just p->Just p
                        Nothing->let
                                modu=usModule u
                                matchingpkgs=HM.foldrWithKey (\k (Object mods) l->if (HM.member modu mods) then k : l else l) [] pkgs
                                in listToMaybe matchingpkgs
                usagePackage _ _=Nothing
                ghcValToUsage ::  T.Text -> Value -> [Usage]
                ghcValToUsage pkg (Object m) |
                        Just (String s)<-HM.lookup "Name" m,
                        Just (String mo)<-HM.lookup "Module" m,
                        Just (String p)<-HM.lookup "Package" m,
                        Just (String ht)<-HM.lookup "HType" m,
                        Just arr<-HM.lookup "Pos" m= [Usage (Just (if p=="main" then pkg else p)) mo s (ht=="t") arr]
                ghcValToUsage _ _=[]
                importToUsage :: ImportDef -> [Usage]
                importToUsage imd=[Usage (i_package imd) (i_module imd) "" False (toJSON $ i_loc imd)]
                


-- | build one source file in GHC
build1 :: FilePath -- ^ the source file
        -> BuildWrapper (OpResult Bool) -- ^ True if build is successful
build1 fp=do
        (mtm,msgs)<-getGHCAST fp
        return (isJust mtm,msgs)

-- | preprocess a file
preproc :: BuildFlags  -- ^ the build flags       
        -> FilePath -- ^ the file to preprocess
        -> IO String -- ^ the resulting code
preproc bf tgt= do
        inputOrig<-readFile tgt
        let epo=parseOptions $ bf_preproc bf
        case epo of
                    Right opts2->runCpphs opts2 tgt inputOrig
                    Left _->return inputOrig

-- | get the build flags for a source file
getBuildFlags :: FilePath -- ^ the source file
        -> BuildWrapper (OpResult BuildFlags)
getBuildFlags fp=do
        tgt<-getTargetPath fp
        src<-getCabalFile Source
        modSrc<-liftIO $ getModificationTime src
        mbf<-liftIO $ readBuildFlagsInfo tgt modSrc
        case mbf of
                Just bf-> return bf
                Nothing -> do
                        (mcbi,bwns)<-getBuildInfo fp
                        ret<-case mcbi of
                                Just cbi->do
                                        (_,opts2)<-fileGhcOptions cbi
                                        let 
                                                (modName,opts)=cabalExtensions $ snd cbi
                                                lit=".lhs" == takeExtension fp
                                                cppo=fileCppOptions (snd cbi) ++ ["-D__GLASGOW_HASKELL__=" ++ show (__GLASGOW_HASKELL__ :: Int)] ++ ["--unlit" | lit]
                                                modS=moduleToString modName
                                        return (BuildFlags  (opts ++ opts2) cppo  (Just modS),bwns)
                                Nothing -> return (BuildFlags knownExtensionNames  []  Nothing,[])
                        liftIO $ storeBuildFlagsInfo tgt ret
                        return ret

-- | get haskell-src-exts commented AST for source file
getAST :: FilePath -- ^  the source file
        -> BuildWrapper (OpResult (Maybe (ParseResult (Module SrcSpanInfo, [Comment]))))
getAST fp=do
        (bf,ns)<-getBuildFlags fp
        tgt<-getTargetPath fp
        input<-liftIO $ preproc bf tgt
        pr<- liftIO $ getHSEAST input (bf_ast bf)
        return (Just pr,ns)

-- | get GHC typechecked AST for source file
getGHCAST :: FilePath -- ^ the source file
        -> BuildWrapper (OpResult (Maybe TypecheckedSource))
getGHCAST fp = withGHCAST' fp (\_->BwGHC.getAST)

-- | perform an action on the GHC AST
withGHCAST ::  FilePath -- ^ the source file
        -> (FilePath --  ^ the source file
                -> FilePath --  ^ the base directory
                ->  String --  ^ the module name
                -> [String] --  ^ the GHC options
                -> IO a)
        -> BuildWrapper (OpResult (Maybe a))
withGHCAST fp f=withGHCAST' fp (\n a b c d->do
        r<- f a b c d
        return (Just r,n))

withGHCAST' ::  FilePath -- ^ the source file
        -> ([BWNote] --  ^ the notes from getting the flags
        -> FilePath --  ^ the source file
        -> FilePath --  ^ the base directory
        ->  String --  ^ the module name
        -> [String] --  ^ the GHC options
        ->  IO (OpResult (Maybe a))) -> BuildWrapper (OpResult (Maybe a))
withGHCAST'  fp f= do
        (bf,ns)<-getBuildFlags fp
        case bf of 
                (BuildFlags opts _ (Just modS))-> do
                        tgt<-getTargetPath fp
                        temp<-getFullTempDir
                        liftIO $ do
                                cd<-getCurrentDirectory
                                setCurrentDirectory temp
                                (pr,bwns2)<- f [] tgt temp modS opts
                                setCurrentDirectory cd
                                return (pr,ns ++ bwns2)
                _ -> return (Nothing,ns)

-- | get outline for source file
getOutline :: FilePath -- ^ source file
        -> BuildWrapper (OpResult OutlineResult)
getOutline fp=do
       (mast,bwns)<-getAST fp
       case mast of
        Just (ParseOk ast)->do
                --liftIO $ Prelude.print ast
                let ods=getHSEOutline ast
                let (es,is)=getHSEImportExport ast
                return (OutlineResult ods es is,bwns)
        Just (ParseFailed failLoc err)->return (OutlineResult [] [] [],BWNote BWError err (BWLocation fp (srcLine failLoc) (srcColumn failLoc)) :bwns)
        _ -> return (OutlineResult [] [] [],bwns)
 
-- | get lexer token types for source file 
getTokenTypes :: FilePath -- ^ the source file
        -> BuildWrapper (OpResult [TokenDef])
getTokenTypes fp=do
        tgt<-getTargetPath fp
        ett<-liftIO $ do
                input<-readFile tgt
                BwGHC.tokenTypesArbitrary tgt input (".lhs" == takeExtension fp) knownExtensionNames
        case ett of
                Right tt->return (tt,[])
                Left bw -> return ([],[bw])
                 
                
-- ^ get all occurrences of a token in the file
getOccurrences :: FilePath -- ^ the source file
        -> String -- ^ the token to search for
        -> BuildWrapper (OpResult [TokenDef])
getOccurrences fp query=do
        (BuildFlags opts _ _, _)<-getBuildFlags fp
        tgt<-getTargetPath fp
        input<-liftIO $ readFile tgt
        ett<-liftIO $ BwGHC.occurrences tgt input (T.pack query) (".lhs" == takeExtension fp) opts
        case ett of
                Right tt->return (tt,[])
                Left bw -> return ([],[bw])


-- | get thing at point
getThingAtPoint :: FilePath -- ^ the source file
        -> Int -- ^ the line
        -> Int -- ^ the column
--        -> Bool -- ^ do we want the result qualified?
--        -> Bool -- ^ do we want the result typed?
        -> BuildWrapper (OpResult (Maybe ThingAtPoint))
getThingAtPoint fp line col=do
        mm<-withGHCAST fp $ BwGHC.getThingAtPointJSON line col
        return $ case mm of 
                (Just m,ns)->(m,ns)
                (Nothing,ns)-> (Nothing,ns)
                
-- | get all names in scope (GHC API)                
getNamesInScope :: FilePath-> BuildWrapper (OpResult (Maybe [String]))
getNamesInScope fp=withGHCAST fp BwGHC.getGhcNamesInScope

-- | get cabal dependencies
getCabalDependencies :: BuildWrapper (OpResult [(FilePath,[CabalPackage])])
getCabalDependencies = cabalDependencies

-- | get cabal components
getCabalComponents :: BuildWrapper (OpResult [CabalComponent])
getCabalComponents = cabalComponents
