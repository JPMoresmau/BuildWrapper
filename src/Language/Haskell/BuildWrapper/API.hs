{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Language.Haskell.BuildWrapper.API where

import Language.Haskell.BuildWrapper.Base
import Language.Haskell.BuildWrapper.Cabal
import Language.Haskell.BuildWrapper.GHC
import Language.Haskell.BuildWrapper.Src



import Control.Monad.State

import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.SrcLoc

import Text.JSON
import Data.Maybe


import System.FilePath




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

build :: BuildWrapper (OpResult Bool)
build = do
        cabalBuild
--        (bool,bwns)<-configure
--        if bool
--                then do
--                        (ret,bwns2)<-cabalBuild
--                        return (ret,(bwns++bwns2))
--                else
--                        return (bool,bwns)

getAST :: FilePath -> BuildWrapper (OpResult (Maybe (ParseResult (Module SrcSpanInfo, [Comment]))))
getAST fp = do
        (m,bwns)<-fileGhcOptions fp
        case m of
                Just(mod,opts)->do
                        --liftIO $ putStrLn ("options:"++(show opts))
                        let modS=moduleToString mod
                        --liftIO $ putStrLn ("mod:"++modS)
                        tgt<-getTargetPath fp
                        --liftIO $ putStrLn ("file:"++tgt)
        --(bool,bwns)<-build
                        pr<-liftIO $ getHSEAST tgt modS opts
                        --let json=makeObj  [("parse" , (showJSON $ pr))]
                        return (Just pr,bwns)
                Nothing-> return (Nothing,bwns)

getOutline :: FilePath -> BuildWrapper (OpResult [OutlineDef])
getOutline fp=do
       (mast,bwns)<-getAST fp
       case mast of
        Just (ParseOk ast)->return (getHSEOutline ast,bwns)
        _ -> return ([],bwns)
 