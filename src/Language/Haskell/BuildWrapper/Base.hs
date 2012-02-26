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

-- | State type
type BuildWrapper=StateT BuildWrapperState IO

-- | the state we keep
data BuildWrapperState=BuildWrapperState{
        tempFolder::String -- ^ name of temporary folder
        ,cabalPath::FilePath  -- ^ path to the cabal executable
        ,cabalFile::FilePath -- ^ path of the project cabal file
        ,verbosity::Verbosity -- ^ verbosity of logging
        ,cabalFlags::String -- ^ flags to pass cabal
        }

-- | status of notes: error or warning
data BWNoteStatus=BWError | BWWarning
        deriving (Show,Read,Eq)
 
instance ToJSON BWNoteStatus  where
    toJSON = toJSON . drop 2 . show 
 
instance FromJSON BWNoteStatus where
    parseJSON (String t) =return $ read $ T.unpack $ T.append "BW" t
    parseJSON _= mzero  
 
-- | location of a note/error
data BWLocation=BWLocation {
        bwl_src::FilePath -- ^ source file 
        ,bwl_line::Int -- ^ line
        ,bwl_col::Int -- ^ column
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

-- | a note on a source file
data BWNote=BWNote {
        bwn_status :: BWNoteStatus -- ^ status of the note
        ,bwn_title :: String -- ^ message
        ,bwn_location :: BWLocation -- ^ where the note is
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
        
-- | simple type encapsulating the fact the operations return along with notes generated on files        
type OpResult a=(a,[BWNote])
        
-- | result: success + files impacted
data BuildResult=BuildResult Bool [FilePath]
        deriving (Show,Read,Eq)
  
instance ToJSON BuildResult  where
    toJSON (BuildResult b fps)= object ["r" .= b, "fps" .= map toJSON fps]       

instance FromJSON BuildResult where
    parseJSON (Object v) =BuildResult <$>
                         v .: "r" <*>
                         v .: "fps" 
    parseJSON _= mzero    

-- | which cabal file to use operations
data WhichCabal=
        Source   -- ^ use proper file
        | Target -- ^ use temporary file that was saved in temp folder
        deriving (Show,Read,Eq,Enum,Data,Typeable)        
        
-- | type of elements for the outline        
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
 
-- | Location inside a file, the file is known and doesn't need to be repeated 
data InFileLoc=InFileLoc {ifl_line::Int -- ^ line
        ,ifl_column::Int -- ^ column
        }
        deriving (Show,Read,Eq,Ord)

-- | Span inside a file, the file is known and doesn't need to be repeated 
data InFileSpan=InFileSpan {ifs_start::InFileLoc -- ^ start location
        ,ifs_end::InFileLoc  -- ^ end location
        }
        deriving (Show,Read,Eq,Ord)

instance ToJSON InFileSpan  where
    toJSON  (InFileSpan (InFileLoc sr sc) (InFileLoc er ec))=toJSON $ map toJSON [sr,sc,er,ec]   

instance FromJSON InFileSpan where
    parseJSON (Array v) |
        Success v0 <- fromJSON (v V.! 0),
        Success v1 <- fromJSON (v V.! 1),
        Success v2 <- fromJSON (v V.! 2),
        Success v3 <- fromJSON (v V.! 3)=return $ InFileSpan (InFileLoc v0 v1) (InFileLoc v2 v3)
    parseJSON _= mzero        

-- | construct a file span
mkFileSpan :: Int -- ^ start line
        -> Int -- ^ start column
        -> Int -- ^ end line
        -> Int -- ^ end column
        -> InFileSpan
mkFileSpan sr sc er ec=InFileSpan (InFileLoc sr sc) (InFileLoc er ec)

-- | element of the outline result
data OutlineDef = OutlineDef
  { od_name       :: T.Text -- ^  name
  ,od_type       :: [OutlineDefType] -- ^ types: can have several to combine
  ,od_loc        :: InFileSpan -- ^ span in source
  ,od_children   :: [OutlineDef] -- ^ children (constructors...)
  ,od_signature  :: Maybe T.Text -- ^ type signature if any
  ,od_comment    :: Maybe T.Text -- ^ comment if any
  }
  deriving (Show,Read,Eq,Ord)
     
-- | constructs an OutlineDef with no children and no type signature     
mkOutlineDef :: T.Text -- ^  name
        -> [OutlineDefType] -- ^ types: can have several to combine
        -> InFileSpan  -- ^ span in source
        -> OutlineDef
mkOutlineDef n t l=  mkOutlineDefWithChildren n t l []

-- | constructs an OutlineDef with children and no type signature     
mkOutlineDefWithChildren :: T.Text -- ^  name
        -> [OutlineDefType] -- ^ types: can have several to combine
        -> InFileSpan  -- ^ span in source
        -> [OutlineDef] -- ^ children (constructors...)
        -> OutlineDef
mkOutlineDefWithChildren n t l c=  OutlineDef n t l c Nothing Nothing
     
instance ToJSON OutlineDef where
        toJSON (OutlineDef n tps l c ts d)=  object ["n" .= n , "t" .= map toJSON tps, "l" .= l, "c" .= map toJSON c, "s" .= ts, "d" .= d]
     
instance FromJSON OutlineDef where
    parseJSON (Object v) =OutlineDef <$>
                         v .: "n" <*>
                         v .: "t" <*>
                         v .: "l" <*>
                         v .: "c" <*>
                         v .: "s" <*>
                         v .: "d"
    parseJSON _= mzero          
     
-- | Lexer token
data TokenDef = TokenDef {
        td_name :: T.Text -- ^ type of token
        ,td_loc :: InFileSpan -- ^ location
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
    
-- | Type of import/export directive    
data ImportExportType = IEVar -- ^ Var
        | IEAbs  -- ^ Abs
        | IEThingAll -- ^ import/export everythin
        | IEThingWith -- ^ specific import/export list
        | IEModule -- ^ reexport module
      deriving (Show,Read,Eq,Ord,Enum)
 
instance ToJSON ImportExportType  where
    toJSON = toJSON . show
 
instance FromJSON ImportExportType where
    parseJSON (String s) =return $ read $ T.unpack s
    parseJSON _= mzero
    
-- | definition of export
data ExportDef = ExportDef {
        e_name :: T.Text -- ^ name
        ,e_type :: ImportExportType -- ^ type
        ,e_loc  :: InFileSpan -- ^ location in source file
        ,e_children :: [T.Text] -- ^ children (constructor names, etc.)
    }   deriving (Show,Eq)     
     
instance ToJSON ExportDef where
        toJSON (ExportDef n t l c)=  object ["n" .= n , "t" .= t, "l" .= l, "c" .= map toJSON c]
     
instance FromJSON ExportDef where
    parseJSON (Object v) =ExportDef <$>
                         v .: "n" <*>
                         v .: "t" <*>
                         v .: "l" <*>
                         v .: "c"
    parseJSON _= mzero          
     
-- | definition of an import element   
data ImportSpecDef = ImportSpecDef {
        is_name :: T.Text -- ^ name
        ,is_type :: ImportExportType -- ^ type
        ,is_loc  :: InFileSpan -- ^ location in source file
        ,is_children :: [T.Text] -- ^ children (constructor names, etc.)
        }  deriving (Show,Eq)        
     
instance ToJSON ImportSpecDef where
        toJSON (ImportSpecDef n t l c)=  object ["n" .= n , "t" .= t, "l" .= l, "c" .= map toJSON c]
     
instance FromJSON ImportSpecDef where
    parseJSON (Object v) =ImportSpecDef <$>
                         v .: "n" <*>
                         v .: "t" <*>
                         v .: "l" <*>
                         v .: "c"
    parseJSON _= mzero     
     
-- | definition of an import statement     
data ImportDef = ImportDef {
        i_module :: T.Text -- ^ module name
        ,i_loc  :: InFileSpan -- ^ location in source file
        ,i_qualified :: Bool -- ^ is the import qualified
        ,i_hiding :: Bool -- ^ is the import element list for hiding or exposing 
        ,i_alias :: T.Text -- ^ alias name
        ,i_children :: Maybe [ImportSpecDef]  -- ^ specific import elements
        }  deriving (Show,Eq)    
    
instance ToJSON ImportDef where
        toJSON (ImportDef m l q h a c)=  object ["m" .= m , "l" .= l, "q" .= q, "h" .= h, "a" .= a, "c" .=  c]
     
instance FromJSON ImportDef where
    parseJSON (Object v) =ImportDef <$>
                         v .: "m" <*>
                         v .: "l" <*>
                         v .: "q" <*>
                         v .: "h" <*>
                         v .: "a" <*>
                         v .: "c"
    parseJSON _= mzero     

-- | complete result for outline    
data OutlineResult = OutlineResult {
        or_outline :: [OutlineDef] -- ^ outline contents
        ,or_exports :: [ExportDef] -- ^ exports
        ,or_imports :: [ImportDef] -- ^ imports
        }    
        deriving (Show,Eq)
        
instance ToJSON OutlineResult where
        toJSON (OutlineResult o e i)=  object ["o" .= map toJSON o,"e" .= map toJSON e,"i" .= map toJSON i]
     
instance FromJSON OutlineResult where
    parseJSON (Object v) =OutlineResult <$>
                         v .: "o" <*>
                         v .: "e" <*>
                         v .: "i"
    parseJSON _= mzero  
      
-- | build flags for a specific file        
data BuildFlags = BuildFlags {
        bf_ast :: [String] -- ^ flags for GHC
        ,bf_preproc :: [String] -- ^ flags for preprocessor
        ,bf_modName :: Maybe String -- ^ module name if known
        }  
        deriving (Show,Read,Eq,Data,Typeable)
        
instance ToJSON BuildFlags where
        toJSON (BuildFlags ast preproc modName)=  object ["a" .= map toJSON ast, "p" .=  map toJSON preproc, "m" .= toJSON modName]

instance FromJSON BuildFlags where
   parseJSON (Object v)=BuildFlags <$>
                         v .: "a" <*>
                         v .: "p" <*>
                         v .: "m"
   parseJSON _= mzero  
   
data ThingAtPoint = ThingAtPoint {
        tapName :: String,
        tapModule :: Maybe String,
        tapType :: Maybe String,
        tapQType :: Maybe String,
        tapHType :: Maybe String,
        tapGType :: Maybe String
        }   
        deriving (Show,Read,Eq,Data,Typeable)
 
instance ToJSON ThingAtPoint where
        toJSON (ThingAtPoint name modu stype qtype htype gtype)=object ["Name" .= name, "Module" .= modu, "Type" .= stype, "QType" .= qtype, "HType" .= htype, "GType" .= gtype]
        
instance FromJSON ThingAtPoint where
   parseJSON (Object v)=ThingAtPoint <$>
                         v .: "Name" <*>
                         v .: "Module" <*>
                         v .: "Type" <*>
                         v .: "QType" <*>
                         v .: "HType" <*>
                         v .: "GType"
   parseJSON _= mzero         
   
-- | get the full path for the temporary directory
getFullTempDir ::  BuildWrapper FilePath
getFullTempDir = do
        cf<-gets cabalFile
        temp<-gets tempFolder
        let dir=(takeDirectory cf)
        return (dir </> temp)

-- | get the full path for the temporary dist directory (where cabal will write its output)
getDistDir ::  BuildWrapper FilePath
getDistDir = do
       temp<-getFullTempDir
       return (temp </> "dist")

-- | get full path in temporary folder for source file (i.e. where we're going to write the temporary contents of an edited file)
getTargetPath :: FilePath  -- ^ relative path of source file
        -> BuildWrapper FilePath
getTargetPath src=do
        temp<-getFullTempDir
        let path=temp </> src
        liftIO $ createDirectoryIfMissing True (takeDirectory path)
        return path

-- | get the full, canonicalized path of a source
canonicalizeFullPath :: FilePath -- ^ relative path of source file
        -> BuildWrapper FilePath
canonicalizeFullPath fp =do
        full<-getFullSrc fp 
        ex<-liftIO $ doesFileExist full -- on OSX with GHC 7.0, canonicalizePath fails on non existing paths, so let's be defensive
        if ex 
                then liftIO $ canonicalizePath full
                else return full
                
-- | get the full path of a source
getFullSrc :: FilePath -- ^ relative path of source file
        -> BuildWrapper FilePath
getFullSrc src=do
        cf<-gets cabalFile
        let dir=(takeDirectory cf)
        return (dir </> src)

-- | copy a file from the normal folders to the temp folder
copyFromMain :: Bool -- ^ copy even if temp file is newer
        -> FilePath -- ^ relative path of source file
        -> BuildWrapper(Maybe FilePath) -- ^ return Just the file if copied, Nothing if no copy was done 
copyFromMain force src=do
        fullSrc<-getFullSrc src
        exSrc<-liftIO $ doesFileExist fullSrc
        if exSrc 
                then do
                        fullTgt<-getTargetPath src
                        ex<-liftIO $ doesFileExist fullTgt
                        shouldCopy<- if force || not ex
                                then return True 
                                else
                                  do modSrc <- liftIO $ getModificationTime fullSrc
                                     modTgt <- liftIO $ getModificationTime fullTgt
                                     return (modSrc >= modTgt) -- if same date, we may thing precision is not good enough to be 100% sure tgt is newer, so we copy
                        if shouldCopy
                                then do
                                        liftIO $ copyFile fullSrc fullTgt
                                        return $ Just src
                                else return Nothing
                 else return Nothing

-- | replace relative file path by module name      
fileToModule :: FilePath -> String
fileToModule fp=map rep (dropExtension fp)
        where   rep '/'  = '.'
                rep '\\' = '.'
                rep a = a  
   
-- | Verbosity settings                
data Verbosity = Silent | Normal | Verbose | Deafening
    deriving (Show, Read, Eq, Ord, Enum, Bounded,Data,Typeable)
    
-- | component in cabal file    
data CabalComponent
  = CCLibrary  -- ^ library
        { cc_buildable :: Bool -- ^ is the library buildable
        }
  | CCExecutable -- executable
        { cc_exe_name :: String -- ^ executable name
        , cc_buildable :: Bool -- ^ is the executable buildable
       }
  | CCTestSuite -- est suite
        { cc_test_name :: String -- ^ test suite name
        , cc_buildable :: Bool -- ^ is the test suite buildable
        }      
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

-- | a cabal package
data CabalPackage=CabalPackage {
        cp_name::String -- ^ name of package
        ,cp_version::String -- ^ version
        ,cp_exposed::Bool -- ^ is the package exposed or hidden
        ,cp_dependent::[CabalComponent] -- ^ components in the cabal file that use this package
        ,cp_exposedModules::[String] -- ^ exposed modules
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
  
-- | debug method: fromJust with a message to display when we get Nothing 
fromJustDebug :: String -> Maybe a -> a
fromJustDebug s Nothing=error ("fromJust:" ++ s)
fromJustDebug _ (Just a)=a

-- | remove a base directory from a string representing a full path
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