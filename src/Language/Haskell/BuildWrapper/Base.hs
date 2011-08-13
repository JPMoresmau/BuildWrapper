{-# LANGUAGE DeriveDataTypeable,OverloadedStrings #-}
module Language.Haskell.BuildWrapper.Base where


import Control.Monad.State
import Data.Data
import Data.Aeson
import qualified Data.Text as T

import System.Directory
import System.FilePath

type BuildWrapper=StateT BuildWrapperState IO

data BuildWrapperState=BuildWrapperState{
        tempFolder::String,
        cabalPath::FilePath,
        cabalFile::FilePath,
        verbosity::Verbosity
        }

data BWNoteStatus=BWError | BWWarning
        deriving (Show,Read,Eq)
 
instance ToJSON BWNoteStatus  where
    toJSON = toJSON . drop 2 . show 
 
data BWLocation=BWLocation {
        bwl_src::FilePath,
        bwl_line::Int,
        bwl_col::Int
        }
        deriving (Show,Read,Eq)

instance ToJSON BWLocation  where
    toJSON (BWLocation s l c)=object ["f" .= s, "l" .= l , "c" .= c] 

data BWNote=BWNote {
        bwn_status :: BWNoteStatus,
        bwn_title :: String,
        bwn_location :: BWLocation
        }
        deriving (Show,Read,Eq)
        
instance ToJSON BWNote  where
    toJSON (BWNote s t l)= object ["s" .= s, "t" .= t, "l" .= l]       
        
type OpResult a=(a,[BWNote])
        
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
                Constructor
        deriving (Show,Read,Eq,Ord,Enum)
 
instance ToJSON OutlineDefType  where
    toJSON = toJSON . show
 
data InFileLoc=InFileLoc {ifl_line::Int,ifl_column::Int}
        deriving (Show,Read,Eq,Ord)

data InFileSpan=InFileSpan {ifs_start::InFileLoc,ifs_end::InFileLoc}
        deriving (Show,Read,Eq,Ord)

instance ToJSON InFileSpan  where
    toJSON  (InFileSpan (InFileLoc sr sc) (InFileLoc er ec))=toJSON $ map toJSON [sr,sc,er,ec]   

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
     
data TokenDef = TokenDef {
        td_name :: T.Text,
        td_loc :: InFileSpan
    }
        deriving (Show,Eq)     
    
instance ToJSON TokenDef where
  toJSON  (TokenDef n s)=
    object [n .= s]
  
        
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

getDistDir ::  BuildWrapper(FilePath)
getDistDir = do
        cf<-gets cabalFile
        temp<-gets tempFolder
        let dir=(takeDirectory cf)
        return $ (dir </> temp </> "dist")

getTargetPath :: FilePath -> BuildWrapper(FilePath)
getTargetPath src=do
        cf<-gets cabalFile
        temp<-gets tempFolder
        let dir=(takeDirectory cf)
        let path=dir </> temp </> src
        liftIO $ createDirectoryIfMissing True (takeDirectory path)
        return path

getFullSrc :: FilePath -> BuildWrapper(FilePath)
getFullSrc src=do
        cf<-gets cabalFile
        let dir=(takeDirectory cf)
        return $ (dir </> src)

copyFromMain :: FilePath -> BuildWrapper(Maybe FilePath)
copyFromMain src=do
        fullSrc<-getFullSrc src
        exSrc<-liftIO $ doesFileExist fullSrc
        if exSrc 
                then do
                        fullTgt<-getTargetPath src
                        ex<-liftIO $ doesFileExist fullTgt
                        shouldCopy<-if (not ex )
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
        where   rep '/' = '.'
                rep a = a  
                
data Verbosity = Silent | Normal | Verbose | Deafening
    deriving (Show, Read, Eq, Ord, Enum, Bounded,Data,Typeable)                