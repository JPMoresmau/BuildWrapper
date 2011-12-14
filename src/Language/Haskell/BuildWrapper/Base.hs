{-# LANGUAGE DeriveDataTypeable,OverloadedStrings,PatternGuards #-}
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
-- Data types, State Monad, utility functions
module Language.Haskell.BuildWrapper.Base where

import Control.Applicative
import Control.Monad
import Control.Monad.State

import Data.Data
import Data.Aeson
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as M
import qualified Data.Vector as V

import System.Directory
import System.FilePath

type BuildWrapper=StateT BuildWrapperState IO

data BuildWrapperState=BuildWrapperState{
        tempFolder::String,
        cabalPath::FilePath,
        cabalFile::FilePath,
        verbosity::Verbosity,
        cabalFlags::String
        }

data BWNoteStatus=BWError | BWWarning
        deriving (Show,Read,Eq)
 
instance ToJSON BWNoteStatus  where
    toJSON = toJSON . drop 2 . show 
 
instance FromJSON BWNoteStatus where
    parseJSON (String t) =return $ read $ T.unpack $ T.append "BW" t
    parseJSON _= mzero  
 
data BWLocation=BWLocation {
        bwl_src::FilePath,
        bwl_line::Int,
        bwl_col::Int
        }
        deriving (Show,Read,Eq)

instance ToJSON BWLocation  where
    toJSON (BWLocation s l c)=object ["f" .= s, "l" .= l , "c" .= c] 

instance FromJSON BWLocation where
    parseJSON (Object v) =BWLocation <$>
                         v .: "f" <*>
                         v .: "l" <*>
                         v .: "c"
    parseJSON _= mzero

data BWNote=BWNote {
        bwn_status :: BWNoteStatus,
        bwn_title :: String,
        bwn_location :: BWLocation
        }
        deriving (Show,Read,Eq)
        
instance ToJSON BWNote  where
    toJSON (BWNote s t l)= object ["s" .= s, "t" .= t, "l" .= l]       
        
instance FromJSON BWNote where
    parseJSON (Object v) =BWNote <$>
                         v .: "s" <*>
                         v .: "t" <*>
                         v .: "l"
    parseJSON _= mzero        
        
type OpResult a=(a,[BWNote])
        
data BuildResult=BuildResult Bool [FilePath]
        deriving (Show,Read,Eq)
  
instance ToJSON BuildResult  where
    toJSON (BuildResult b fps)= object ["r" .= b, "fps" .= map toJSON fps]       

instance FromJSON BuildResult where
    parseJSON (Object v) =BuildResult <$>
                         v .: "r" <*>
                         v .: "fps" 
    parseJSON _= mzero    

data WhichCabal=Source | Target
        deriving (Show,Read,Eq,Enum,Data,Typeable)        
        
data OutlineDefType =
                Class |
                Data |
                Family |
                Function |
                Pattern |
                Syn |
                Type |
                Instance |
                Field |
                Constructor |
                Splice
        deriving (Show,Read,Eq,Ord,Enum)
 
instance ToJSON OutlineDefType  where
    toJSON = toJSON . show
 
instance FromJSON OutlineDefType where
    parseJSON (String s) =return $ read $ T.unpack s
    parseJSON _= mzero
 
data InFileLoc=InFileLoc {ifl_line::Int,ifl_column::Int}
        deriving (Show,Read,Eq,Ord)

data InFileSpan=InFileSpan {ifs_start::InFileLoc,ifs_end::InFileLoc}
        deriving (Show,Read,Eq,Ord)

instance ToJSON InFileSpan  where
    toJSON  (InFileSpan (InFileLoc sr sc) (InFileLoc er ec))=toJSON $ map toJSON [sr,sc,er,ec]   

instance FromJSON InFileSpan where
    parseJSON (Array v) |
        Success v0 <- fromJSON $ (v V.! 0),
        Success v1 <- fromJSON $ (v V.! 1),
        Success v2 <- fromJSON $ (v V.! 2),
        Success v3 <- fromJSON $ (v V.! 3)=return $ InFileSpan (InFileLoc v0 v1) (InFileLoc v2 v3)
    parseJSON _= mzero        


mkFileSpan :: Int -> Int -> Int -> Int -> InFileSpan
mkFileSpan sr sc er ec=InFileSpan (InFileLoc sr sc) (InFileLoc er ec)

data OutlineDef = OutlineDef
  { od_name       :: T.Text,
    od_type       :: [OutlineDefType],
    od_loc        :: InFileSpan,
    od_children   :: [OutlineDef]
  }
  deriving (Show,Read,Eq,Ord)
     
instance ToJSON OutlineDef where
        toJSON (OutlineDef n tps l c)=  object ["n" .= n , "t" .= map toJSON tps, "l" .= l, "c" .= map toJSON c]
     
instance FromJSON OutlineDef where
    parseJSON (Object v) =OutlineDef <$>
                         v .: "n" <*>
                         v .: "t" <*>
                         v .: "l" <*>
                         v .: "c"
    parseJSON _= mzero          
     
data TokenDef = TokenDef {
        td_name :: T.Text,
        td_loc :: InFileSpan
    }
        deriving (Show,Eq)     
    
instance ToJSON TokenDef where
  toJSON  (TokenDef n s)=
    object [n .= s]
  
instance FromJSON TokenDef where
    parseJSON (Object o) |
        ((a,b):[])<-M.toList o,
        Success v0 <- fromJSON b=return $ TokenDef a v0
    parseJSON _= mzero          
--withCabal :: (GenericPackageDescription -> BuildWrapper a) -> BuildWrapper (Either BWNote a)
--withCabal f =do
--        cf<-gets cabalFile
--        pr<-parseCabal
--        case pr of
--                ParseOk _ a  ->(liftM Right) $ f a
--                ParseFailed p->return $ Left $ peErrorToBWNote (takeFileName cf) p  
--
--parseCabal :: BuildWrapper(ParseResult GenericPackageDescription)
--parseCabal = do
--        cf<-gets cabalFile
--        return $ parsePackageDescription cf

getFullTempDir ::  BuildWrapper(FilePath)
getFullTempDir = do
        cf<-gets cabalFile
        temp<-gets tempFolder
        let dir=(takeDirectory cf)
        return $ (dir </> temp)

getDistDir ::  BuildWrapper(FilePath)
getDistDir = do
       temp<-getFullTempDir
       return $ (temp </> "dist")

getTargetPath :: FilePath -> BuildWrapper(FilePath)
getTargetPath src=do
        temp<-getFullTempDir
        let path=temp </> src
        liftIO $ createDirectoryIfMissing True (takeDirectory path)
        return path

getFullSrc :: FilePath -> BuildWrapper(FilePath)
getFullSrc src=do
        cf<-gets cabalFile
        let dir=(takeDirectory cf)
        return $ (dir </> src)

copyFromMain :: Bool -> FilePath -> BuildWrapper(Maybe FilePath)
copyFromMain force src=do
        fullSrc<-getFullSrc src
        exSrc<-liftIO $ doesFileExist fullSrc
        if exSrc 
                then do
                        fullTgt<-getTargetPath src
                        ex<-liftIO $ doesFileExist fullTgt
                        shouldCopy<- if (force || (not ex))
                                then return True
                                else do
                                        modSrc<-liftIO $ getModificationTime fullSrc
                                        modTgt<-liftIO $ getModificationTime fullTgt
                                        return (modSrc>=modTgt)
                        if shouldCopy
                                then do
                                        liftIO $ copyFileFull fullSrc fullTgt
                                        return $ Just src
                                else return Nothing
                 else return Nothing

copyFileFull :: FilePath -> FilePath -> IO()
copyFileFull src tgt=do
        --createDirectoryIfMissing True (takeDirectory tgt)
        --putStrLn tgt
        copyFile src tgt
      
fileToModule :: FilePath -> String
fileToModule fp=map rep (dropExtension fp)
        where   rep '/'  = '.'
                rep '\\' = '.'
                rep a = a  
                
data Verbosity = Silent | Normal | Verbose | Deafening
    deriving (Show, Read, Eq, Ord, Enum, Bounded,Data,Typeable)
    
data CabalComponent
  = CCLibrary { cc_buildable :: Bool}
  | CCExecutable { cc_exe_name :: String
        , cc_buildable :: Bool}
  | CCTestSuite { cc_test_name :: String
        , cc_buildable :: Bool}      
  deriving (Eq, Show)

instance ToJSON CabalComponent where
        toJSON (CCLibrary b)=  object ["Library" .= b]
        toJSON (CCExecutable e b)=  object ["Executable" .= b,"e" .= e]
        toJSON (CCTestSuite t b)=  object ["TestSuite" .= b,"t" .= t]

instance FromJSON CabalComponent where
    parseJSON (Object v)
        | Just b <- M.lookup "Library" v =CCLibrary <$> parseJSON b
        | Just b <- M.lookup "Executable" v =CCExecutable <$> v .: "e" <*> parseJSON b
        | Just b <- M.lookup "TestSuite" v =CCTestSuite <$> v .: "t" <*> parseJSON b
        | otherwise = mzero
    parseJSON _= mzero

data CabalPackage=CabalPackage {
        cp_name::String,
        cp_version::String,
        cp_exposed::Bool,
        cp_dependent::[CabalComponent],
        cp_exposedModules::[String]
        }
   deriving (Eq, Show)

instance ToJSON CabalPackage where
        toJSON (CabalPackage n v e d em)=object ["n" .= n,"v" .= v, "e" .= e, "d" .= map toJSON d, "m" .= map toJSON em]

instance FromJSON CabalPackage where
    parseJSON (Object v) =CabalPackage <$>
                         v .: "n" <*>
                         v .: "v" <*>
                         v .: "e" <*>
                         v .: "d" <*>
                         v .: "m"
    parseJSON _= mzero

-- |  http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)         
  
  
fromJustDebug :: String -> Maybe a -> a
fromJustDebug s Nothing=error ("fromJust:" ++ s)
fromJustDebug _ (Just a)=a


removeBaseDir :: FilePath -> String -> String
removeBaseDir base_dir s= loop s
  where
    loop [] = []
    loop str =
      let (prefix, rest) = splitAt n str
      in
        if base_dir_sep == prefix                -- found an occurrence?
        then loop rest                       -- yes: drop it
        else head str : loop (tail str)      -- no: keep looking
    n = length base_dir_sep
    base_dir_sep=base_dir ++ [pathSeparator] 