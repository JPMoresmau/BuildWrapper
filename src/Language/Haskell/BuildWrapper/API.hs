{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Language.Haskell.BuildWrapper.API where

import Language.Haskell.BuildWrapper.Base
import Language.Haskell.BuildWrapper.Cabal
import qualified Language.Haskell.BuildWrapper.GHC as BwGHC
import Language.Haskell.BuildWrapper.Src

import qualified Data.Text as T

import Control.Monad.State
import Language.Haskell.Exts.Annotated
import Language.Preprocessor.Cpphs
import Data.Maybe
import System.FilePath
import System.Time
import GHC (TypecheckedSource)

synchronize ::  Bool -> BuildWrapper(OpResult [FilePath])
synchronize force =do
        cf<-gets cabalFile
        m<-copyFromMain force $ takeFileName cf
        (fileList,ns)<-getFilesToCopy
        --liftIO $ putStrLn  ("filelist:" ++ (show fileList))
        --let fileList=case motherFiles of
        --       Nothing ->[]
        --        Just fps->fps
        m1<-mapM (copyFromMain force)(
                "Setup.hs":
                "Setup.lhs":
                fileList)
        return $ (catMaybes (m:m1),ns)


synchronize1 ::  Bool -> FilePath -> BuildWrapper(Maybe FilePath)
synchronize1 force fp = do
        m1<-mapM (copyFromMain force) [fp]
        return $ head m1

write ::  FilePath -> String -> BuildWrapper()
write fp s= do
        real<-getTargetPath fp
        liftIO $ putStrLn ("contents:"++s)
        liftIO $ writeFile real s

configure ::  WhichCabal -> BuildWrapper (OpResult Bool)
configure which= do
        --synchronize
        (mlbi,msgs)<-cabalConfigure which
        return $ (isJust mlbi,msgs)

build :: Bool -> WhichCabal -> BuildWrapper (OpResult Bool)
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
        let cppo=fileCppOptions cbi ++ ["-D__GLASGOW_HASKELL__=" ++ show (__GLASGOW_HASKELL__::Int)]
        putStrLn $ "cppo=" ++ (show cppo)
        if not $ null cppo 
            then do
                let epo=parseOptions cppo
                case epo of
                    Right opts2->liftIO $ runCpphs opts2 tgt inputOrig
                    Left _->return inputOrig
            else return inputOrig


getAST :: FilePath -> BuildWrapper (OpResult (Maybe (ParseResult (Module SrcSpanInfo, [Comment]))))
getAST fp = do
--        (mcbi,bwns)<-getBuildInfo fp
--        case mcbi of
--                Just(cbi)->do
--                        let (_,opts)=cabalExtensions $ snd  cbi
--                        tgt<-getTargetPath fp
--                        --let modS=moduleToString modName
--                        input<-liftIO $ preproc (snd cbi) tgt
--                        pr<- liftIO $ getHSEAST input opts
--                        --let json=makeObj  [("parse" , (showJSON $ pr))]
--                        return (Just pr,bwns)
--                Nothing-> do
                        -- cf<-gets cabalFile
                        tgt<-getTargetPath fp
                        -- let dir=(takeDirectory cf)
                        --liftIO $ putStrLn "not in cabal"
                        input<-liftIO $ readFile tgt -- (dir </> fp)
                        pr<- liftIO $ getHSEAST input knownExtensionNames
                        --let json=makeObj  [("parse" , (showJSON $ pr))]
                        return (Just pr,[])

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
       --liftIO $ putStrLn $ show mast
       case mast of
        Just (ParseOk ast)->do
                return (getHSEOutline ast,bwns)
        _ -> return ([],bwns)
 
getTokenTypes :: FilePath -> BuildWrapper (OpResult [TokenDef])
getTokenTypes fp=do
        c1<-liftIO $ getClockTime
--        (mcbi,bwns)<-getBuildInfo fp
--        case mcbi of
--                Just(cbi)->do
--                        let (_,opts)=cabalExtensions $ snd cbi
--                        tgt<-getTargetPath fp
--                        ett<-liftIO $ do
--                                input<-readFile tgt
--                                putStrLn $ show $ length input
--                                ett2<-BwGHC.tokenTypesArbitrary tgt input (".lhs" == (takeExtension fp)) opts
--                                c2<-getClockTime
--                                putStrLn ("getTokenTypes: " ++ (show $  tdPicosec $ diffClockTimes c2 c1))
--                                return ett2
--                        case ett of
--                                Right tt->return (tt,bwns)
--                                Left bw -> return ([],bw:bwns)
--                Nothing-> do
        tgt<-getTargetPath fp
        ett<-liftIO $ do
                --let dir=takeDirectory cf
                --liftIO $ putStrLn "not in cabal"
                input<-readFile tgt --(dir </> fp)
                ett2<-BwGHC.tokenTypesArbitrary tgt input (".lhs" == (takeExtension fp)) knownExtensionNames
                case ett2 of 
                        Right tt-> putStrLn ("getTokenTypes: " ++ (show $ length tt))
                        Left _->return()
                c2<-getClockTime
                putStrLn ("getTokenTypes: " ++ (show $ (\x->div x 1000000000) $ tdPicosec $ diffClockTimes c2 c1) ++"ms")
                return ett2
        case ett of
                Right tt->return (tt,[])  -- bwns
                Left bw -> return ([],bw:[])  -- bwns
                 
                
getOccurrences :: FilePath -> String -> BuildWrapper (OpResult [TokenDef])
getOccurrences fp query=do
        (mcbi,bwns)<-getBuildInfo fp
        case mcbi of
                Just(cbi)->do
                        let (_,opts)=cabalExtensions $ snd  cbi
                        tgt<-getTargetPath fp
                        input<-liftIO $ readFile tgt
                        ett<-liftIO $ BwGHC.occurrences tgt input (T.pack query) (".lhs" == (takeExtension fp)) opts
                        case ett of
                                Right tt->return (tt,bwns)
                                Left bw -> return ([],bw:bwns)
                Nothing-> return ([],bwns)

getThingAtPoint :: FilePath -> Int -> Int -> Bool -> Bool -> BuildWrapper (OpResult (Maybe String))
getThingAtPoint fp line col qual typed=withGHCAST fp $ BwGHC.getThingAtPoint line col qual typed
                
getNamesInScope :: FilePath-> BuildWrapper (OpResult (Maybe [String]))
getNamesInScope fp=withGHCAST fp BwGHC.getGhcNamesInScope

getCabalDependencies :: BuildWrapper (OpResult [(FilePath,[CabalPackage])])
getCabalDependencies = cabalDependencies

getCabalComponents :: BuildWrapper (OpResult [CabalComponent])
getCabalComponents = cabalComponents
