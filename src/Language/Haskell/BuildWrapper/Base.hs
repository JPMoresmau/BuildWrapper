
module Language.Haskell.BuildWrapper.Base where

import qualified Distribution.Verbosity as V 
                        ( Verbosity )

import Control.Monad.State

import Text.JSON

import System.Directory
import System.FilePath

type BuildWrapper=StateT BuildWrapperState IO

data BuildWrapperState=BuildWrapperState{
        tempFolder::String,
        cabalPath::FilePath,
        cabalFile::FilePath,
        cabalVerbosity::V.Verbosity
        }

data BWNoteStatus=BWError | BWWarning
        deriving (Show,Read,Eq)
 
data BWLocation=BWLocation {
        bwl_src::FilePath,
        bwl_line::Int,
        bwl_col::Int
        }
        deriving (Show,Read,Eq)

data BWNote=BWNote {
        bwn_status :: BWNoteStatus,
        bwn_title :: String,
        bwn_description :: String,
        bwn_location :: BWLocation
        }
        deriving (Show,Read,Eq)
        
type OpResult a=(a,[BWNote])
        
data WhichCabal=Source | Target        
        
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
 
data InFileLoc=InFileLoc {ifl_line::Int,ifl_column::Int}
        deriving (Show,Read,Eq,Ord)

data InFileSpan=InFileSpan {ifs_start::InFileLoc,ifs_end::InFileLoc}
        deriving (Show,Read,Eq,Ord)

mkFileSpan :: Int -> Int -> Int -> Int -> InFileSpan
mkFileSpan sr sc er ec=InFileSpan (InFileLoc sr sc) (InFileLoc er ec)

data OutlineDef = OutlineDef
  { od_name       :: String,
    od_type       :: [OutlineDefType],
    od_loc        :: InFileSpan,
    od_children   :: [OutlineDef]
  }
  deriving (Show,Read,Eq,Ord)
     
data TokenDef = TokenDef {
        td_name :: String,
        td_loc :: InFileSpan
    }
        deriving (Show,Eq)     
    
instance JSON TokenDef where
  showJSON (TokenDef n (InFileSpan (InFileLoc sr sc) (InFileLoc er ec)))=
    JSArray ((JSString $ toJSString n): (map showJSON [sr,sc,er,ec]))
  
        
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