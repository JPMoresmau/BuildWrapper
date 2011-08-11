{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Language.Haskell.BuildWrapper.API where

import Language.Haskell.BuildWrapper.Base
import Language.Haskell.BuildWrapper.Cabal
import qualified Language.Haskell.BuildWrapper.GHC as BwGHC
import Language.Haskell.BuildWrapper.Src

import Control.Monad.State
import Language.Haskell.Exts.Annotated
import Language.Preprocessor.Cpphs
import Data.Maybe
import System.FilePath
import GHC (TypecheckedSource)
import Outputable

--class ToBWNote a where
--        toBWNote :: a -> BWNote

--peErrorToBWNote :: FilePath -> PError -> BWNote
--peErrorToBWNote cf (AmbigousParse t ln)= BWNote BWError "AmbigousParse" t (BWLocation cf ln 1)
--peErrorToBWNote cf (NoParse t ln)      = BWNote BWError "NoParse" t (BWLocation cf ln 1)
--peErrorToBWNote cf (TabsError ln)      = BWNote BWError "TabsError" "" (BWLocation cf ln 1)    
--peErrorToBWNote cf (FromString t mln)  = BWNote BWError "FromString" t (BWLocation cf (fromMaybe 1 mln) 1)    



synchronize ::  BuildWrapper([FilePath])
synchronize =do
        cf<-gets cabalFile
        m<-copyFromMain $ takeFileName cf
        (fileList,_)<-getFilesToCopy
        --let fileList=case motherFiles of
        --       Nothing ->[]
        --        Just fps->fps
        m1<-mapM copyFromMain (
                "Setup.hs":
                "Setup.lhs":
                fileList)
        return $ catMaybes (m:m1)


synchronize1 ::  FilePath -> BuildWrapper(Maybe FilePath)
synchronize1 fp = do
        m1<-mapM copyFromMain [fp]
        return $ head m1

write ::  FilePath -> String -> BuildWrapper()
write fp s= do
        real<-getTargetPath fp
        liftIO $ writeFile real s

configure ::  WhichCabal -> BuildWrapper (OpResult Bool)
configure which= do
        --synchronize
        (mlbi,msgs)<-cabalConfigure which
        return $ (isJust mlbi,msgs)

build :: Bool -> BuildWrapper (OpResult Bool)
build = cabalBuild
--        (bool,bwns)<-configure
--        if bool
--                then do
--                        (ret,bwns2)<-cabalBuild
--                        return (ret,(bwns++bwns2))
--                else
--                        return (bool,bwns)

-- ppContents :: String -> String
-- ppContents = unlines . (map f) . lines
--  where f ('#':_) = ""
--        f x = x     

preproc :: CabalBuildInfo -> FilePath -> IO String
preproc cbi tgt= do
        inputOrig<-readFile tgt
        let cppo=fileCppOptions cbi
        --putStrLn $ "cppo=" ++ (show cppo)
        if not $ null cppo 
            then do
                let epo=parseOptions cppo
                case epo of
                    Right opts2->liftIO $ runCpphs opts2 tgt inputOrig
                    Left _->return inputOrig
            else return inputOrig


getAST :: FilePath -> BuildWrapper (OpResult (Maybe (ParseResult (Module SrcSpanInfo, [Comment]))))
getAST fp = do
        (mcbi,bwns)<-getBuildInfo fp
        case mcbi of
                Just(cbi)->do
                        let (modName,opts)=cabalExtensions $ snd  cbi
                        tgt<-getTargetPath fp
                        let modS=moduleToString modName
                        input<-liftIO $ preproc (snd cbi) tgt
                        pr<- liftIO $ getHSEAST input modS opts
                        --let json=makeObj  [("parse" , (showJSON $ pr))]
                        return (Just pr,bwns)
                Nothing-> return (Nothing,bwns)

getGHCAST :: FilePath -> BuildWrapper (OpResult (Maybe TypecheckedSource))
getGHCAST fp = do
        (mcbi,bwns)<-getBuildInfo fp
        case mcbi of
                Just(cbi)->do
                        let (modName,opts)=cabalExtensions $ snd  cbi
                        let (_,opts2)=fileGhcOptions cbi
                        tgt<-getTargetPath fp
                        let modS=moduleToString modName
                        pr<- liftIO $ BwGHC.getAST tgt modS (opts++opts2)
                        return (Just pr,bwns)
                Nothing-> return (Nothing,bwns)

withGHCAST :: FilePath -> (FilePath -> String -> [String] -> IO a) -> BuildWrapper (OpResult (Maybe a))
withGHCAST fp f= do
        (mcbi,bwns)<-getBuildInfo fp
        case mcbi of
                Just(cbi)->do
                        let (modName,opts)=cabalExtensions $ snd  cbi
                        let (_,opts2)=fileGhcOptions cbi
                        tgt<-getTargetPath fp
                        let modS=moduleToString modName
                        pr<- liftIO $ f tgt modS (opts++opts2)
                        return (Just pr,bwns)
                Nothing-> return (Nothing,bwns)

getOutline :: FilePath -> BuildWrapper (OpResult [OutlineDef])
getOutline fp=do
       (mast,bwns)<-getAST fp
       -- liftIO $ putStrLn $ show mast
       case mast of
        Just (ParseOk ast)->do
                return (getHSEOutline ast,bwns)
        _ -> return ([],bwns)
 
getTokenTypes :: FilePath -> BuildWrapper (OpResult [TokenDef])
getTokenTypes fp=do
        (mcbi,bwns)<-getBuildInfo fp
        case mcbi of
                Just(cbi)->do
                        let (_,opts)=cabalExtensions $ snd  cbi
                        tgt<-getTargetPath fp
                        input<-liftIO $ readFile tgt
                        ett<-liftIO $ BwGHC.tokenTypesArbitrary tgt input (".lhs" == (takeExtension fp)) opts
                        case ett of
                                Right tt->return (tt,bwns)
                                Left bw -> return ([],bw:bwns)
                Nothing-> return ([],bwns)
                
getOccurrences :: FilePath -> String -> BuildWrapper (OpResult [TokenDef])
getOccurrences fp query=do
        (mcbi,bwns)<-getBuildInfo fp
        case mcbi of
                Just(cbi)->do
                        let (_,opts)=cabalExtensions $ snd  cbi
                        tgt<-getTargetPath fp
                        input<-liftIO $ readFile tgt
                        ett<-liftIO $ BwGHC.occurrences tgt input query (".lhs" == (takeExtension fp)) opts
                        case ett of
                                Right tt->return (tt,bwns)
                                Left bw -> return ([],bw:bwns)
                Nothing-> return ([],bwns)

getThingAtPoint :: FilePath -> Int -> Int -> BuildWrapper (OpResult [String])
getThingAtPoint fp line col=do
        (mts,bwns)<-getGHCAST fp
        case mts of
                Just ts->do
                        liftIO $ putStrLn "ghc ast ok"
                        let ls=BwGHC.getThingAtPoint ts line col
                        liftIO $ putStrLn $ show $ length ls
                        return (ls ,bwns)
                _ -> return ([],bwns)
                
getNamesInScope :: FilePath-> BuildWrapper (OpResult (Maybe [String]))
getNamesInScope fp=withGHCAST fp BwGHC.getGhcNamesInScope
