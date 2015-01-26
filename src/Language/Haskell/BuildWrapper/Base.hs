{-# LANGUAGE DeriveDataTypeable,OverloadedStrings,PatternGuards, NamedFieldPuns #-}
-- | Module      : Language.Haskell.BuildWrapper.Base
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
import Control.Exception (bracket)
import           Control.Concurrent.Async (Concurrently (..))
import qualified Data.ByteString          as BS
import           Data.Conduit             (($=),($$))
import qualified Data.Conduit.List        as CL
import           Data.Conduit.Process     (Inherited (..), proc,
                                           streamingProcess,
                                           waitForStreamingProcess)
import           System.Exit              (ExitCode)

import Data.Data
import Data.Aeson
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as M
import qualified Data.Vector as V
import qualified Data.Set as S

import System.Directory
import System.FilePath
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)

import           Data.ByteString.UTF8     (toString)
import System.IO (IOMode, openBinaryFile, IOMode(..), Handle, hClose, hFlush, stdout, hPutStr, hGetContents, hSetEncoding, utf8)
import Control.DeepSeq (rnf, NFData)

-- | State type
type BuildWrapper=StateT BuildWrapperState IO

-- | the state we keep
data BuildWrapperState=BuildWrapperState{
        tempFolder::String -- ^ name of temporary folder
        ,cabalPath::FilePath  -- ^ path to the cabal executable
        ,cabalFile::FilePath -- ^ path of the project cabal file
        ,verbosity::Verbosity -- ^ verbosity of logging
        ,cabalFlags::String -- ^ flags to pass cabal
        ,cabalOpts::[String] -- ^ extra arguments to cabal configure
        ,logCabalArgs :: Bool -- ^ log call to cabal
        }

-- | status of notes: error or warning
data BWNoteStatus=BWError | BWWarning
        deriving (Show,Read,Eq)
 
instance ToJSON BWNoteStatus  where
    toJSON = toJSON . drop 2 . show 
 
instance FromJSON BWNoteStatus where
    parseJSON (String t) =return $ readObj "BWNoteStatus" $ T.unpack $ T.append "BW" t
    parseJSON _= mzero  

-- | read an object from a String, with a given error message if it fails 
readObj :: Read a=> String -> String -> a
readObj msg s=let parses=reads s -- :: [(a,String)]
        in if null parses 
                then error (msg ++ ": " ++ s ++ ".")
                else fst $ head parses 
 
-- | location of a note/error (lines and columns start at 1)
data BWLocation=BWLocation {
        bwlSrc::FilePath -- ^ source file 
        ,bwlLine::Int -- ^ line
        ,bwlCol::Int -- ^ column
        ,bwlEndLine::Int -- ^ end line
        ,bwlEndCol::Int -- ^ end line
        }
        deriving (Show,Read,Eq)

instance NFData BWLocation
  where rnf (BWLocation src sl sc el ec)=rnf src `seq` rnf sl `seq` rnf sc `seq` rnf el `seq` rnf ec

-- | build an empty span in a given file at a given location
mkEmptySpan :: FilePath -> Int -> Int -> BWLocation
mkEmptySpan src line col = BWLocation src line col line col

instance ToJSON BWLocation  where
    toJSON (BWLocation s l c el ec)=object ["f" .= s, "l" .= l , "c" .= c, "el" .= el , "ec" .= ec] 

instance FromJSON BWLocation where
    parseJSON (Object v) =BWLocation <$>
                         v .: "f" <*>
                         v .: "l" <*>
                         v .: "c" <*>
                         v .: "el" <*>
                         v .: "ec" 
    parseJSON _= mzero

-- | a note on a source file
data BWNote=BWNote {
        bwnStatus :: BWNoteStatus -- ^ status of the note
        ,bwnTitle :: String -- ^ message
        ,bwnLocation :: BWLocation -- ^ where the note is
        }
        deriving (Show,Read,Eq)

instance NFData BWNote
  where rnf (BWNote _ t l)=rnf t `seq` rnf l

-- | is a note an error?      
isBWNoteError :: BWNote -> Bool
isBWNoteError bw=bwnStatus bw == BWError
        
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

-- | result for building one file: success + names
--data Build1Result=Build1Result Bool [NameDef]
--        deriving (Show,Read,Eq)
--  
--instance ToJSON Build1Result  where
--    toJSON (Build1Result b ns)= object ["r" .= b, "ns" .= map toJSON ns]       
--
--instance FromJSON Build1Result where
--    parseJSON (Object v) =Build1Result <$>
--                         v .: "r" <*>
--                         v .: "ns" 
--    parseJSON _= mzero    


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
    parseJSON (String s) =return $ readObj "OutlineDefType" $ T.unpack s
    parseJSON _= mzero
 
-- | Location inside a file, the file is known and doesn't need to be repeated 
data InFileLoc=InFileLoc {iflLine::Int -- ^ line
        ,iflColumn::Int -- ^ column
        }
        deriving (Show,Read,Eq,Ord)

-- | Span inside a file, the file is known and doesn't need to be repeated 
data InFileSpan=InFileSpan {ifsStart::InFileLoc -- ^ start location
        ,ifsEnd::InFileLoc  -- ^ end location
        }
        deriving (Show,Read,Eq,Ord)

-- | do spans overlap?
ifsOverlap :: InFileSpan -> InFileSpan -> Bool
ifsOverlap ifs1 ifs2 = iflOverlap ifs1 $ ifsStart ifs2

-- | does span overlap location?
iflOverlap :: InFileSpan -> InFileLoc -> Bool
iflOverlap ifs1 ifs2 =let
        l11=iflLine $ ifsStart ifs1
        l12=iflLine $ ifsEnd ifs1
        c11=iflColumn $ ifsStart ifs1
        c12=iflColumn $ ifsEnd ifs1
        l21=iflLine ifs2
        c21=iflColumn ifs2
        in (l11<l21 || (l11==l21 && c11<=c21)) && (l12>l21 || (l12==l21 && c12>=c21))

instance ToJSON InFileSpan  where
    toJSON  (InFileSpan (InFileLoc sr sc) (InFileLoc er ec))
        | sr==er = if ec==sc+1 
                then toJSON $ map toJSON [sr,sc]
                else toJSON $ map toJSON [sr,sc,ec]  
        | otherwise = toJSON $ map toJSON [sr,sc,er,ec]   

instance FromJSON InFileSpan where
    parseJSON (Array v) =do
        let
                l=V.length v
        case l of
                2->do 
                     let 
                        Success v0 = fromJSON (v V.! 0)
                        Success v1 = fromJSON (v V.! 1)
                     return $ InFileSpan (InFileLoc v0 v1) (InFileLoc v0 (v1+1))
                3->do
                     let 
                        Success v0 = fromJSON (v V.! 0)
                        Success v1 = fromJSON (v V.! 1)
                        Success v2 = fromJSON (v V.! 2)
                     return $ InFileSpan (InFileLoc v0 v1) (InFileLoc v0 v2)  
                4->do 
                     let 
                        Success v0 = fromJSON (v V.! 0)
                        Success v1 = fromJSON (v V.! 1)
                        Success v2 = fromJSON (v V.! 2)
                        Success v3 = fromJSON (v V.! 3)
                     return $  InFileSpan (InFileLoc v0 v1) (InFileLoc v2 v3)                            
                _ -> mzero
    parseJSON _= mzero        

-- | construct a file span
mkFileSpan :: Int -- ^ start line
        -> Int -- ^ start column
        -> Int -- ^ end line
        -> Int -- ^ end column
        -> InFileSpan
mkFileSpan sr sc er ec=InFileSpan (InFileLoc sr sc) (InFileLoc er ec)

-- | definition of a name
data NameDef = NameDef
        { ndName  :: T.Text -- ^  name
        , ndType  :: [OutlineDefType] -- ^ types: can have several to combine
        , ndSignature  :: Maybe T.Text -- ^ type signature if any
        }
        deriving (Show,Read,Eq,Ord)

instance ToJSON NameDef where
        toJSON (NameDef n tps ts)=  object ["n" .= n , "t" .= map toJSON tps,"s" .= ts]
     
instance FromJSON NameDef where
    parseJSON (Object v) =NameDef <$>
                         v .: "n" <*>
                         v .: "t" <*>
                         v .:? "s"
    parseJSON _= mzero    

-- | element of the outline result
data OutlineDef = OutlineDef
  { odName       :: T.Text -- ^  name
  ,odType       :: [OutlineDefType] -- ^ types: can have several to combine
  ,odLoc        :: InFileSpan -- ^ span in source
  ,odChildren   :: [OutlineDef] -- ^ children (constructors...)
  ,odSignature  :: Maybe T.Text -- ^ type signature if any
  ,odComment    :: Maybe T.Text -- ^ comment if any,
  ,odStartLineComment    :: Maybe Int -- ^ comment start line if any,
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
mkOutlineDefWithChildren n t l c=  OutlineDef n t l c Nothing Nothing Nothing 
     
instance ToJSON OutlineDef where
        toJSON (OutlineDef n tps l c ts d sl)=  object ["n" .= n , "t" .= map toJSON tps, "l" .= l, "c" .= map toJSON c, "s" .= ts, "d" .= d, "sl" .= sl]
     
instance FromJSON OutlineDef where
    parseJSON (Object v) =OutlineDef <$>
                         v .: "n" <*>
                         v .: "t" <*>
                         v .: "l" <*>
                         v .: "c" <*>
                         v .:? "s" <*>
                         v .:? "d" <*>
                         v .:? "sl"
    parseJSON _= mzero          
     
-- | Lexer token
data TokenDef = TokenDef {
        tdName :: T.Text -- ^ type of token
        ,tdLoc :: InFileSpan -- ^ location
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
        | IEThingAll -- ^ import/export everything
        | IEThingWith -- ^ specific import/export list
        | IEModule -- ^ reexport module
      deriving (Show,Read,Eq,Ord,Enum)
 
instance ToJSON ImportExportType  where
    toJSON = toJSON . show
 
instance FromJSON ImportExportType where
    parseJSON (String s) =return $ readObj "ImportExportType" $ T.unpack s
    parseJSON _= mzero
    
-- | definition of export
data ExportDef = ExportDef {
        eName :: T.Text -- ^ name
        ,eType :: ImportExportType -- ^ type
        ,eLoc  :: InFileSpan -- ^ location in source file
        ,eChildren :: [T.Text] -- ^ children (constructor names, etc.)
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
        isName :: T.Text -- ^ name
        ,isType :: ImportExportType -- ^ type
        ,isLoc  :: InFileSpan -- ^ location in source file
        ,isChildren :: [T.Text] -- ^ children (constructor names, etc.)
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
        iModule :: T.Text -- ^ module name
        ,iPackage :: Maybe T.Text -- ^ package name
        ,iLoc  :: InFileSpan -- ^ location in source file
        ,iQualified :: Bool -- ^ is the import qualified
        ,iHiding :: Bool -- ^ is the import element list for hiding or exposing 
        ,iAlias :: T.Text -- ^ alias name
        ,iChildren :: Maybe [ImportSpecDef]  -- ^ specific import elements
        }  deriving (Show,Eq)    
    
instance ToJSON ImportDef where
        toJSON (ImportDef m p l q h a c)=  object ["m" .= m , "p" .= p, "l" .= l, "q" .= q, "h" .= h, "a" .= a, "c" .=  c]
     
instance FromJSON ImportDef where
    parseJSON (Object v) =ImportDef <$>
                         v .: "m" <*>
                         v .:? "p" <*>
                         v .: "l" <*>
                         v .: "q" <*>
                         v .: "h" <*>
                         v .: "a" <*>
                         v .:? "c"
    parseJSON _= mzero     

-- | complete result for outline    
data OutlineResult = OutlineResult {
        orOutline :: [OutlineDef] -- ^ outline contents
        ,orExports :: [ExportDef] -- ^ exports
        ,orImports :: [ImportDef] -- ^ imports
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
        bfAst :: [String] -- ^ flags for GHC
        ,bfPreproc :: [String] -- ^ flags for preprocessor
        ,bfModName :: Maybe String -- ^ module name if known
        ,bfComponent :: Maybe String -- ^ component used to get flags, if known
        }  
        deriving (Show,Read,Eq,Data,Typeable)
        
instance ToJSON BuildFlags where
        toJSON (BuildFlags ast preproc modName comp)=  object ["a" .= map toJSON ast, "p" .=  map toJSON preproc, "m" .= toJSON modName, "c" .= toJSON comp]

instance FromJSON BuildFlags where
   parseJSON (Object v)=BuildFlags <$>
                         v .: "a" <*>
                         v .: "p" <*>
                         v .:? "m" <*>
                         v .:? "c"
   parseJSON _= mzero  
   
-- | information about the thing at a given point in the source   
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
                         v .:? "Module" <*>
                         v .:? "Type" <*>
                         v .:? "QType" <*>
                         v .:? "HType" <*>
                         v .:? "GType"
   parseJSON _= mzero         
   
-- | get the full path for the temporary directory
getFullTempDir ::  BuildWrapper FilePath
getFullTempDir = do
        cf<-gets cabalFile
        temp<-gets tempFolder
        let dir=takeDirectory cf
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
        liftIO $ getTargetPath' src temp

-- | get full path in temporary folder for source file (i.e. where we're going to write the temporary contents of an edited file)
getTargetPath' :: FilePath  -- ^ relative path of source file
        -> FilePath
        -> IO FilePath
getTargetPath' src temp=do
        let path=temp </> src
        createDirectoryIfMissing True (takeDirectory path)
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
        let dir=takeDirectory cf
        return (dir </> src)

-- | copy a file from the normal folders to the temp folder
copyFromMain :: Bool -- ^ copy even if temp file is newer
        -> FilePath -- ^ relative path of source file
        -> BuildWrapper(Maybe FilePath) -- ^ return Just the file if copied, Nothing if no copy was done 
copyFromMain force src=do
        fullSrc<-getFullSrc src
        fullTgt<-getTargetPath src
        exSrc<-liftIO $ doesFileExist fullSrc
        if exSrc 
                then do
                        moreRecent<-liftIO $ isSourceMoreRecent fullSrc fullTgt
                        if force || moreRecent
                                then do
                                        liftIO $ copyFile fullSrc fullTgt
                                        return $ Just src
                                else return Nothing
                else return Nothing

-- | is the source file more recent than the target file?
isSourceMoreRecent :: FilePath -> FilePath -> IO Bool
isSourceMoreRecent fullSrc fullTgt=do
        ex<-doesFileExist fullTgt
        if not ex 
                then return True
                else 
                  do modSrc <- getModificationTime fullSrc
                     modTgt <- getModificationTime fullTgt
                     return (modSrc >= modTgt)

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
  = CCLibrary
        { ccBuildable :: Bool -- ^ is the library buildable
        } -- ^ library
  | CCExecutable
        { ccExeName :: String -- ^ executable name
        , ccBuildable :: Bool -- ^ is the executable buildable
       }  -- ^ executable
  | CCTestSuite 
        { ccTestName :: String -- ^ test suite name
        , ccBuildable :: Bool -- ^ is the test suite buildable
        } -- ^ test suite
  | CCBenchmark 
        { ccBenchName :: String -- ^ benchmark name
        , ccBuildable :: Bool -- ^ is the be buildabnchmarkle
        } -- ^ test suite
  deriving (Eq, Show, Read,Ord)

instance ToJSON CabalComponent where
        toJSON (CCLibrary b)=  object ["Library" .= b]
        toJSON (CCExecutable e b)=  object ["Executable" .= b,"e" .= e]
        toJSON (CCTestSuite t b)=  object ["TestSuite" .= b,"t" .= t]
        toJSON (CCBenchmark t b)=  object ["Benchmark" .= b,"b" .= t]

instance FromJSON CabalComponent where
    parseJSON (Object v)
        | Just b <- M.lookup "Library" v =CCLibrary <$> parseJSON b
        | Just b <- M.lookup "Executable" v =CCExecutable <$> v .: "e" <*> parseJSON b
        | Just b <- M.lookup "TestSuite" v =CCTestSuite <$> v .: "t" <*> parseJSON b
        | Just b <- M.lookup "Benchmark" v =CCBenchmark <$> v .: "b" <*> parseJSON b
        | otherwise = mzero
    parseJSON _= mzero

-- | get the cabal component name
cabalComponentName :: CabalComponent -> String
cabalComponentName CCLibrary{}=""
cabalComponentName CCExecutable{ccExeName}=ccExeName
cabalComponentName CCTestSuite{ccTestName}=ccTestName
cabalComponentName CCBenchmark{ccBenchName}=ccBenchName

-- | a cabal package
data CabalPackage=CabalPackage {
        cpName::String -- ^ name of package
        ,cpVersion::String -- ^ version
        ,cpExposed::Bool -- ^ is the package exposed or hidden
        ,cpDependent::[CabalComponent] -- ^ components in the cabal file that use this package
        ,cpModules::[String] -- ^ all modules. We keep all modules so that we can try to open non exposed but imported modules directly
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

--data ImportClean=ImportClean {
--        imports :: DM.Map Int T.Text 
--        }
--        deriving (Show,Read)
--        
--instance ToJSON ImportClean where
--        toJSON (ImportClean i)=object["i" .= (map (\(k,v)->toJSON $ object ["l" .= k,"t" .= v]) $ DM.assocs i)]        
--        
--instance FromJSON ImportClean where
--    parseJSON (Object v)| Just (Array m) <- M.lookup "i" v =do
--        is<-foldrM f DM.empty m
--        return $ ImportClean is
--        where 
--                f :: Value -> DM.Map Int T.Text -> Parser (DM.Map Int T.Text) 
--                f (Object v2) m |
--                        Just l<- M.lookup "l" v2,
--                        Just t<- M.lookup "t" v2=DM.insert <$> parseJSON l <*> parseJSON t <*> return m
--                f _ m=return m
--    parseJSON _= mzero

-- | import clean operation: the span of text to change, the new text
data ImportClean = ImportClean {
        icSpan :: InFileSpan,
        icText :: T.Text
        }
        deriving (Show,Read,Eq,Ord)

instance ToJSON ImportClean where
        toJSON (ImportClean sp txt)=object ["l" .= sp, "t" .= txt]
        
instance FromJSON ImportClean where
        parseJSON (Object v)=ImportClean <$>
                v .: "l" <*>
                v .: "t"
        parseJSON _=mzero

-- | information about files to load (single file or multiple files)
data LoadContents = SingleFile {
                lmFile :: FilePath
                ,lmModule :: String
        } 
        | MultipleFile {
                lmFiles :: [(FilePath,String)]
        }
        deriving (Show,Read)

-- | get files to load
getLoadFiles :: LoadContents -> [(FilePath,String)]
getLoadFiles SingleFile{lmFile=f,lmModule=m}=[(f,m)]
getLoadFiles MultipleFile{lmFiles=fs}=fs

-- |  <http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html>
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  ex<-doesDirectoryExist topdir
  if ex 
        then do
          names <- getDirectoryContents topdir
          let properNames = filter (not . isPrefixOf ".") names
          paths <- forM properNames $ \name -> do
            let path = topdir </> name
            isDirectory <- doesDirectoryExist path
            if isDirectory
              then getRecursiveContents path
              else return [path]
          return (concat paths) 
        else return []      

-- |  <http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html>
getRecursiveContentsHidden :: FilePath -> IO [FilePath]
getRecursiveContentsHidden topdir = do
  ex<-doesDirectoryExist topdir
  if ex 
        then do
          names <- getDirectoryContents topdir
          let properNames = filter (not . flip elem [".",".."]) names
          paths <- forM properNames $ \name -> do
            let path = topdir </> name
            isDirectory <- doesDirectoryExist path
            if isDirectory
              then getRecursiveContentsHidden path
              else return [path]
          return (concat paths) 
        else return [] 

-- | delete files in temp folder but not in real folder anymore
deleteGhosts :: [FilePath] -> BuildWrapper [FilePath]
deleteGhosts copied=do
        root<-getFullSrc ""
        temp<-getFullTempDir
        fs<-liftIO $ getRecursiveContents temp
        let copiedS=S.fromList copied
        del<-liftIO $ mapM (deleteIfGhost root temp copiedS) fs
        return $ catMaybes del
        where
                deleteIfGhost :: FilePath -> FilePath -> S.Set FilePath -> FilePath -> IO (Maybe FilePath)
                deleteIfGhost rt tmp cs f=do
                        let rel=makeRelative tmp f
                        let cabalDist="dist"
                        let cabalDevDist="cabal-dev"
                        if cabalDist `isPrefixOf` rel || cabalDevDist `isPrefixOf` rel || S.member rel cs
                                then return Nothing
                                else do
                                        let fullSrc=rt </> rel
                                        ex<-doesFileExist fullSrc
                                        if ex 
                                                then return Nothing
                                                else do
                                                        removeFile (tmp </> f)
                                                        return $ Just rel
 
-- | delete all temporary files
deleteTemp ::  BuildWrapper()
deleteTemp = do
        temp<-getFullTempDir
        liftIO $ removeDirectoryRecursive temp

-- | delete generated files  
deleteGenerated ::  BuildWrapper()
deleteGenerated = do
        temp<-getFullTempDir  
        fs<-liftIO $ getRecursiveContentsHidden temp
        liftIO $ mapM_ deleteIfGenerated fs
        where
                deleteIfGenerated :: FilePath -> IO()
                deleteIfGenerated f=do
                        let del=case takeExtension f of
                                ".bwinfo"->True
                                ".bwusage"->True
                                _->False
                        when del (removeFile f)
  
-- | debug method: fromJust with a message to display when we get Nothing 
fromJustDebug :: String -> Maybe a -> a
fromJustDebug s Nothing=error ("fromJust:" ++ s)
fromJustDebug _ (Just a)=a

-- | remove a base directory from a string representing a full path
removeBaseDir :: FilePath -> String -> String
removeBaseDir base_dir = loop
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
    
-- | nub for Ord objects: use a set    
nubOrd :: Ord a => [a] -> [a]
nubOrd=S.toList . S.fromList

-- | debug method to vaguely format JSON result to dump them
formatJSON :: String -> String
formatJSON s1=snd $ foldl f (0,"") s1
        where 
                f :: (Int,String) -> Char -> (Int,String)
                f (i,s) '['=(i + 4, s ++ "\n" ++ map (const ' ') [0 .. i] ++ "[")
                f (i,s) ']'  =(i - 4, s ++ "\n" ++ map (const ' ') [0 .. i] ++ "]")
                f (i,s) c =(i,s++[c])
   
-- | Usage structure                
data Usage = Usage {
        usPackage::Maybe T.Text,
        usModule::T.Text,
        usName::T.Text,
        usSection::T.Text,
        usType::Bool,
        usLoc::Value,
        usDef::Bool
        } 
        deriving (Show,Eq)
 
-- | read a string from a file, forcing the read and closing the handle
readFile :: FilePath -> IO String
-- readFile n=hGetContents =<< openBinaryFile n ReadMode
readFile n =  do
        inFile<- openBinaryFile n ReadMode
        hSetEncoding inFile utf8
        contents <- hGetContents inFile
        rnf contents `seq` hClose inFile -- force the whole file to be read, then close http://stackoverflow.com/a/297630/827593
        return contents

-- | write string to file
writeFile :: FilePath -> String -> IO ()
writeFile n s = withBinaryFile n WriteMode (\h -> hSetEncoding h utf8 >> h `hPutStr` s)      

-- | perform operation on a binary opened file
withBinaryFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withBinaryFile n m = bracket (openBinaryFile n m) hClose

-- | Evaluation of result
-- using String since we get them from GHC API
-- this can be fully evaluated via deepseq to make sure any side effect are evaluated
data EvalResult = EvalResult {
  erType :: Maybe String
  ,erResult :: Maybe String
  ,erError :: Maybe String
  } deriving (Show,Read,Eq,Ord)

instance NFData EvalResult
  where rnf (EvalResult t r e)=rnf t `seq` rnf r `seq` rnf e

instance ToJSON EvalResult where
        toJSON (EvalResult mt mr me)=object ["t" .= mt, "r" .= mr, "e" .= me]
        
instance FromJSON EvalResult where
        parseJSON (Object v)=EvalResult <$>
                v .: "t" <*>
                v .: "r" <*>
                v .: "e"
        parseJSON _=mzero
        
-- | splits a string at the first occurence of prefix       
splitString :: Eq a => [a] -> [a] -> ([a],[a])
splitString prf str=go str []
  where 
    go [] a=(reverse a,[])
    go s@(x:xs) a=
            if prf `isPrefixOf` s
              then (reverse a,s) 
              else go xs (x:a)
              
-- | run a program, writing the output/error to standard output as we go
runAndPrint :: FilePath -> [String] -> IO (ExitCode,String,String)
runAndPrint prog args = do
  (Inherited,outP,errP,cph) <- streamingProcess (proc prog args)
  let
      bsw bs = do
        BS.putStr bs
        hFlush stdout
        return bs
      output = outP $$ CL.mapM bsw $= CL.consume
      err    = errP $$ CL.mapM bsw $= CL.consume

  (outb,errb,ec) <- runConcurrently $ (,,)
      <$> Concurrently output
      <*> Concurrently err
      <*> Concurrently (waitForStreamingProcess cph)
  return (ec,toString $ BS.concat outb,toString $ BS.concat errb)
