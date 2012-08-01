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
import Language.Haskell.BuildWrapper.Base
import Language.Haskell.BuildWrapper.Cabal
import qualified Language.Haskell.BuildWrapper.GHC as BwGHC
import Language.Haskell.BuildWrapper.GHCStorage
import Language.Haskell.BuildWrapper.Src


import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as DM
import Data.List (sortBy)

import Prelude hiding (readFile, writeFile)
import qualified Data.Vector as V

import System.IO.UTF8

import Control.Monad.State
import Language.Haskell.Exts.Annotated hiding (String)
import Language.Preprocessor.Cpphs
import Data.Maybe
import System.Directory
import System.FilePath
import GHC (RenamedSource, TypecheckedSource, TypecheckedModule(..), Ghc, ms_mod, pm_mod_summary, moduleName)
import Data.Aeson
import Outputable (showSDoc,ppr)
import Data.Foldable (foldrM)

import qualified MonadUtils as GMU

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

-- | generate usage information files
generateUsage :: Bool -- ^ should we return all files or only the changed ones?
        -> String -- ^ the cabal component name
        -> BuildWrapper(OpResult (Maybe [FilePath]))
generateUsage returnAll ccn= 
        withCabal Source (\lbi -> do 
                cbis<-getAllFiles lbi
                cf<-gets cabalFile
                temp<-getFullTempDir
                
                let dir=takeDirectory cf
                let pkg=T.pack $ display $ packageId $ localPkgDescr lbi
                allMps<-mapM (\cbi->do
                        let 
                                mps1=map (\(m,f)->(f,moduleToString $ fromJust m)) $ filter (isJust . fst) $ cbiModulePaths cbi
                        mps<-filterM (\(f,_)->do
                                fullSrc<-getFullSrc f
                                fullTgt<-getTargetPath f
                                let fullUsage=getUsageFile fullTgt
                                liftIO $ isSourceMoreRecent fullSrc fullUsage
                                        ) $ filter (\(f,_)->fitForUsage f
                                                )
                                                mps1
                        opts<-fileGhcOptions (lbi,cbi)    
                        modules<-liftIO $ do
                                cd<-getCurrentDirectory
                                setCurrentDirectory dir
                                (mods,_)<-BwGHC.withASTNotes (getModule pkg) (temp </>) dir (MultipleFile mps) opts     
                                setCurrentDirectory cd 
                                return mods
                        mapM_ (generate pkg) modules
                        return $ if returnAll then mps1 else mps
                        ) $ filter (\cbi->cabalComponentName (cbiComponent cbi) == ccn) cbis
                return $ map fst $ concat allMps
                )
        where
                -- | fitForUsage
                fitForUsage :: FilePath -> Bool
                fitForUsage f 
                        | takeDirectory f == "." && takeBaseName f == "Setup"=False -- exclude Setup
                        | otherwise=let ext=takeExtension f
                           in ext `elem` [".hs",".lhs"] -- only keep haskell files
                -- | get module name and import/export usage information
                getModule :: T.Text -- ^ the current package name
                        ->  FilePath -- ^ the file to process
                        -> TypecheckedModule -- ^ the GHC typechecked module
                        -> Ghc(FilePath,T.Text,RenamedSource,[Usage])
                getModule pkg f tm=do
                        let rs@(_,imps,mexps,_)=fromJust $ tm_renamed_source tm
                        (ius,aliasMap)<-foldrM (BwGHC.ghcImportToUsage pkg) ([],DM.empty) imps
                        -- GMU.liftIO $ Prelude.print $ showSDoc $ ppr aliasMap
                        let modu=T.pack $ showSDoc $ ppr $ moduleName $ ms_mod $ pm_mod_summary $ tm_parsed_module tm
                        eus<-mapM (BwGHC.ghcExportToUsage pkg modu aliasMap) (fromMaybe [] mexps)
                        --ms_mod $ pm_mod_summary $ tm_parsed_module tm
                        return (f,modu,rs,ius ++ concat eus)
                -- | generate all usage information and stores it to file
                generate :: T.Text  -- ^ the current package name
                        -> (FilePath,T.Text,RenamedSource,[Usage]) -> BuildWrapper()
                generate pkg (fp,modu,(hsg,_,_,_),ius) 
                        | modu=="Main" && ccn=="" = return () -- Main inside a library: do nothing
                        | otherwise = do
                                -- liftIO $ Prelude.putStrLn (show modu ++ ":" ++ show ccn) 
                                tgt<-getTargetPath fp
                                --mv<-liftIO $ readGHCInfo tgt
                                let v = dataToJSON hsg
                                -- liftIO $ Prelude.putStrLn tgt
                                -- liftIO $ Prelude.putStrLn $ formatJSON $ BSC.unpack $ encode v
                                --case mv of
                                --        Just v->do
                                let vals=extractUsages v
                                --liftIO $ mapM_ (Prelude.putStrLn . formatJSON . BSC.unpack . encode) vals
                                (mast,_)<-getAST fp
                                case mast of
                                        Just (ParseOk ast)->do
                                                let ods=getHSEOutline ast
                                                --liftIO $ Prelude.print ods
                                                let val=reconcile pkg vals ods ius
                                                let (es,is)=getHSEImportExport ast
                                                let modLoc=maybe Null toJSON (getModuleLocation ast) 
                                                let valWithModule=Array $ V.fromList [toJSON pkg,toJSON modu,modLoc,val,toJSON $ OutlineResult ods es is]
                                                liftIO $ setUsageInfo tgt valWithModule
                                                return ()
                                        _ -> return ()
                                return ()
                -- | reconcile AST, usage information and outline into one final usage object        
                reconcile :: T.Text  -- ^ the current package name
                        ->  [Value] -> [OutlineDef] ->  [Usage] -> Value
                reconcile pkg vals ods ius=let
                        mapOds=foldr mapOutline DM.empty ods
                        in foldr usageToJSON (object []) 
                                (ius ++ concatMap (ghcValToUsage pkg mapOds) vals)
                -- | store outline def by line
                mapOutline :: OutlineDef -> DM.Map Int [OutlineDef] -> DM.Map Int [OutlineDef]
                mapOutline od m=let
                        ifs=odLoc od
                        lins=[(iflLine $ ifsStart ifs) .. (iflLine $ ifsEnd ifs)]
                        m2=foldr (addOutline od) m lins
                        in foldr mapOutline m2 (odChildren od)
                -- | add one outline def by line
                addOutline :: OutlineDef -> Int -> DM.Map Int [OutlineDef] -> DM.Map Int [OutlineDef]
                addOutline od l m=let
                        mods=DM.lookup l m
                        newOds=case mods of
                                Just ods->od:ods
                                Nothing->[od]
                        in DM.insert l newOds m   
                -- | translate Usage structure to JSON        
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
                                (Array lins)= HM.lookupDefault (Array V.empty) nameKey names
                                lineV= usLoc u  -- Number $ I $ usLine u
                                objectV=object ["s" .= usSection u, "d" .= usDef u, "l" .= lineV]
                                lins2=if objectV `V.elem` lins
                                        then lins
                                        else V.cons objectV lins
                                names2=HM.insert nameKey (Array lins2) names
                                types2=HM.insert typeKey (Object names2) types
                                mods2=HM.insert (usModule u) (Object types2) mods
                       in Object $ HM.insert pkg (Object mods2) pkgs
                usageToJSON _ a=a
                -- | get the package for a given usage module
                usagePackage ::  Usage -> Value -> Maybe T.Text
                usagePackage u (Object pkgs)=case usPackage u of
                        Just p->Just p
                        Nothing->let
                                modu=usModule u
                                matchingpkgs=HM.foldrWithKey (listPkgs modu) [] pkgs
                                in listToMaybe matchingpkgs
                usagePackage _ _=Nothing
                -- | list packages possible for module
                listPkgs :: T.Text -> T.Text -> Value -> [T.Text] -> [T.Text] 
                listPkgs modu k (Object mods) l=if HM.member modu mods then k : l else l        
                listPkgs _ _ _ l=l      
                -- | translate GHC AST to Usages
                ghcValToUsage ::  T.Text -> DM.Map Int [OutlineDef] -> Value -> [Usage]
                ghcValToUsage pkg mapOds (Object m) |
                        Just (String s)<-HM.lookup "Name" m,
                        Just (String mo)<-HM.lookup "Module" m,
                        not $ T.null mo, -- ignore local objects
                        Just (String p)<-HM.lookup "Package" m,
                        Just (String ht)<-HM.lookup "HType" m,
                        Just arr<-HM.lookup "Pos" m,
                        Success ifs <- fromJSON arr= let
                                mods=DM.lookup (iflLine $ ifsStart ifs) mapOds
                                (section,def)=getSection mods s ifs
                                in [Usage (Just (if p=="main" then pkg else p)) mo s section (ht=="t") arr def]
                ghcValToUsage _ _ _=[]
                -- | retrieve section name for given location, and whether what we reference is actually a reference, and in that case the comment
                getSection :: Maybe [OutlineDef]  -> T.Text -> InFileSpan -> (T.Text,Bool)
                getSection (Just ods) objName ifs =let
                        matchods=filter (\od-> ifsOverlap (odLoc od) ifs) ods
                        bestods=sortBy (\od1 od2->let
                                l1=iflLine $ ifsStart $ odLoc od1
                                l2=iflLine $ ifsStart $ odLoc od2
                                in case compare l2 l1 of
                                   EQ -> let
                                        c1=iflColumn $ ifsStart $ odLoc od1
                                        c2=iflColumn $ ifsStart $ odLoc od2
                                        in compare c2 c1
                                   a-> a
                                ) matchods
                        in case bestods of
                                (x:_)->let 
                                        def=odName x == objName && 
                                                ((iflColumn (ifsStart $ odLoc x) == iflColumn (ifsStart ifs))
                                                || (
                                                        (Data `elem` odType x)
                                                    && (iflColumn (ifsStart $ odLoc x) + 5==iflColumn (ifsStart ifs))    
                                                    )
                                                || (
                                                        (Type `elem` odType x)
                                                    && (iflColumn (ifsStart $ odLoc x) + 5 == iflColumn (ifsStart ifs))    
                                                    ))  
                                       in (odName x,def)
                                _->("",False)
                getSection _ _ _=("",False)
--                importToUsage :: ImportDef -> [Usage]
--                importToUsage imd=[Usage (i_package imd) (i_module imd) "" False (toJSON $ i_loc imd)]
                


-- | build one source file in GHC
build1 :: FilePath -- ^ the source file
        -> BuildWrapper (OpResult (Maybe [NameDef])) -- ^ True if build is successful
build1 fp=withGHCAST' fp BwGHC.getGhcNameDefsInScope


-- | preprocess a file
preproc :: BuildFlags  -- ^ the build flags       
        -> FilePath -- ^ the file to preprocess
        -> IO String -- ^ the resulting code
preproc bf tgt= do
        inputOrig<-readFile tgt
        let epo=parseOptions $ bfPreproc bf
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
                                        opts2<-fileGhcOptions cbi
                                        -- liftIO $ Prelude.print fp
                                        -- liftIO $ Prelude.print $ cbiModulePaths $ snd cbi
                                        -- liftIO $ Prelude.print opts2
                                        let 
                                                fullFp=(takeDirectory src) </> fp
                                                modName=listToMaybe $ mapMaybe fst (filter (\ (_, f) -> f == fullFp) $ cbiModulePaths $ snd cbi)
                                                -- (modName,_)=cabalExtensions $ snd cbi
                                                cppo=fileCppOptions (snd cbi) ++ unlitF
                                                modS=fmap moduleToString modName
                                        -- liftIO $ Prelude.print opts
                                        -- ghcOptions is sufficient, contains extensions and such
                                        -- opts ++
                                        return (BuildFlags opts2 cppo modS,bwns)
                                Nothing -> return (BuildFlags [] unlitF Nothing,[])
                        liftIO $ storeBuildFlagsInfo tgt ret
                        return ret
        where unlitF=let
                lit=".lhs" == takeExtension fp
                in ("-D__GLASGOW_HASKELL__=" ++ show (__GLASGOW_HASKELL__ :: Int)) : ["--unlit" | lit]

-- | get haskell-src-exts commented AST for source file
getAST :: FilePath -- ^  the source file
        -> BuildWrapper (OpResult (Maybe (ParseResult (Module SrcSpanInfo, [Comment]))))
getAST fp=do
        (bf,ns)<-getBuildFlags fp
        tgt<-getTargetPath fp
        input<-liftIO $ preproc bf tgt
        pr<- liftIO $ getHSEAST input (bfAst bf)
        return (Just pr,ns)

-- | get GHC typechecked AST for source file
getGHCAST :: FilePath -- ^ the source file
        -> BuildWrapper (OpResult (Maybe TypecheckedSource))
getGHCAST fp = withGHCAST' fp BwGHC.getAST

-- | perform an action on the GHC AST
withGHCAST ::  FilePath -- ^ the source file
        -> (FilePath --  ^ the source file
                -> FilePath --  ^ the base directory
                ->  String --  ^ the module name
                -> [String] --  ^ the GHC options
                -> IO a)
        -> BuildWrapper (OpResult (Maybe a))
withGHCAST fp f=withGHCAST' fp (\a b c d->do
        r<- f a b c d
        return (Just r,[]))

withGHCAST' ::  FilePath -- ^ the source file
        -> (FilePath --  ^ the source file
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
                                (pr,bwns2)<- f tgt temp modS opts
                                setCurrentDirectory cd
                                return (pr,ns ++ bwns2)
                _ -> return (Nothing,ns)

-- | get outline for source file
getOutline :: FilePath -- ^ source file
        -> BuildWrapper (OpResult OutlineResult)
getOutline fp=do
       tgt<-getTargetPath fp
       let usageFile=getUsageFile tgt
       usageStale<-liftIO $ isSourceMoreRecent tgt usageFile 
       mods<-if not usageStale
          then do
               mv<-liftIO $ getUsageInfo tgt
               return $ case mv of
                        (Array arr) | V.length arr==5->let
                                (Success r)= fromJSON (arr V.! 4)
                                in (Just r) 
                        _->Nothing
          else return Nothing
       case mods of
                Just ods-> return (ods,[])
                _ -> do   
                       (mast,bwns)<-getAST fp
                       case mast of
                        Just (ParseOk ast)->do
                                --liftIO $ Prelude.print ast
                                let ods=getHSEOutline ast
                                let (es,is)=getHSEImportExport ast
                                return (OutlineResult ods es is,bwns)
                        Just (ParseFailed failLoc err)->return (OutlineResult [OutlineDef (T.pack " Outline not available") [Function] (mkFileSpan 0 0 0 0) [] Nothing Nothing] [] []
                                                               ,{-BWNote BWError err (mkEmptySpan fp (srcLine failLoc) (srcColumn failLoc)) : -}
                                                                bwns)
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
