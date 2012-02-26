{-# LANGUAGE CPP, ScopedTypeVariables #-}
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

import Language.Haskell.BuildWrapper.Base
import Language.Haskell.BuildWrapper.Cabal
import qualified Language.Haskell.BuildWrapper.GHC as BwGHC
import Language.Haskell.BuildWrapper.GHCStorage
import Language.Haskell.BuildWrapper.Src

import qualified Data.Text as T

import Prelude hiding (readFile, writeFile)

import System.IO.UTF8

import Control.Monad.State
import Language.Haskell.Exts.Annotated
import Language.Preprocessor.Cpphs
import Data.Maybe
import System.Directory
import System.FilePath
import GHC (TypecheckedSource)

-- | copy all files from the project to the temporary folder
synchronize ::  Bool -- ^ if true copy all files, if false only copy files newer than their corresponding temp files
        -> BuildWrapper(OpResult [FilePath]) -- ^ return the list of files copied
synchronize force =do
        cf<-gets cabalFile
        m<-copyFromMain force $ takeFileName cf
        (fileList,ns)<-getFilesToCopy
        m1<-mapM (copyFromMain force)(
                "Setup.hs":
                "Setup.lhs":
                fileList)
        return (catMaybes (m : m1), ns)

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
        -> (FilePath -- ^ the source file
                -> FilePath -- ^ the base directory
                ->  String -- ^ the module name 
                -> [String] -- ^ the GHC options 
                -> IO a) 
        -> BuildWrapper (OpResult (Maybe a))
withGHCAST fp f=withGHCAST' fp (\n a b c d->do
        r<- f a b c d
        return (Just r,n))

withGHCAST' ::  FilePath -- ^ the source file
        -> ([BWNote] -- ^ the notes from getting the flags
        -> FilePath -- ^ the source file
        -> FilePath -- ^ the base directory
        ->  String -- ^ the module name 
        -> [String] -- ^ the GHC options 
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
                liftIO $ Prelude.print ast
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
