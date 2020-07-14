{-# LANGUAGE OverloadedStrings,StandaloneDeriving,DeriveGeneric,DeriveDataTypeable #-}
module Fragnix.Slice
  ( SliceID
  , Slice(..)
  , Use(..)
  , Reference(..)
  , OriginalModule
  , Language(..)
  , Fragment(..)
  , Qualification
  , Name(..)
  , UsedName(..)
  , Instance(..)
  , InstancePart(..)
  , InstanceID
  , readSlice
  , writeSlice
  , getSlices
  ) where

import Prelude hiding (writeFile,readFile)

import Data.Aeson (
    ToJSON(toJSON),object,(.=),
    FromJSON(parseJSON),withObject,(.:),withText,
    encode,eitherDecode)

import GHC.Generics (Generic)
import Data.Hashable (Hashable)

import Data.Text (Text, unpack)
import qualified Data.Text as Text

import Control.Applicative ((<$>),(<*>),(<|>),empty)

import Control.Monad (forM, filterM)
import Control.Exception (Exception,throwIO)
import Data.Typeable(Typeable)

import Data.ByteString.Lazy (writeFile,readFile)
import System.FilePath ((</>),dropFileName)
import System.Directory (createDirectoryIfMissing, doesFileExist, doesDirectoryExist, listDirectory)

data Slice = Slice SliceID Language Fragment [Use] [Instance]

data Language = Language [GHCExtension]

data Fragment = Fragment [SourceCode]

data Use = Use (Maybe Qualification) UsedName Reference

data Instance = Instance InstancePart InstanceID

data InstancePart =
    OfThisClass |
    OfThisClassForUnknownType |
    ForThisType |
    ForThisTypeOfUnknownClass

type InstanceID = SliceID

data Reference = OtherSlice SliceID | Builtin OriginalModule

data UsedName =
    ValueName Name |
    TypeName Name |
    ConstructorName TypeName Name

data Name = Identifier Text | Operator Text

type TypeName = Name

type SliceID = Text
type SourceCode = Text
type Qualification = Text
type OriginalModule = Text
type GHCExtension = Text


-- Slice instances

deriving instance Show Slice
deriving instance Eq Slice
deriving instance Ord Slice
deriving instance Generic Slice

instance ToJSON Slice where
    toJSON (Slice sliceID language fragment uses instances) = object [
        "sliceID" .= sliceID,
        "language" .= language,
        "fragment" .= fragment,
        "uses" .= uses,
        "instances" .= instances]

instance FromJSON Slice where
    parseJSON = withObject "slice" (\o ->
        Slice <$>
            o .: "sliceID" <*>
            o .: "language" <*>
            o .: "fragment" <*>
            o .: "uses" <*>
            o .: "instances")


-- Language instances

deriving instance Show Language
deriving instance Eq Language
deriving instance Ord Language
deriving instance Generic Language

instance ToJSON Language where
    toJSON (Language ghcExtensions) = object [
        "extensions" .= toJSON ghcExtensions]

instance FromJSON Language where
    parseJSON = withObject "language" (\o ->
        Language <$>
            o .: "extensions")

instance Hashable Language


-- Fragment instances

deriving instance Show Fragment
deriving instance Eq Fragment
deriving instance Ord Fragment
deriving instance Generic Fragment

instance ToJSON Fragment where
    toJSON (Fragment declarations) = toJSON declarations

instance FromJSON Fragment where
    parseJSON = fmap Fragment . parseJSON

instance Hashable Fragment


-- Use instances

deriving instance Show Use
deriving instance Eq Use
deriving instance Ord Use
deriving instance Generic Use

instance ToJSON Use where
    toJSON (Use qualification usedName reference) = object [
        "qualification" .= qualification,
        "usedName" .= usedName,
        "reference" .= reference]

instance FromJSON Use where
    parseJSON = withObject "use" (\o ->
        Use <$> o .: "qualification" <*> o .: "usedName" <*> o .: "reference")

instance Hashable Use


-- Used name instances

deriving instance Show UsedName
deriving instance Eq UsedName
deriving instance Ord UsedName
deriving instance Generic UsedName

instance ToJSON UsedName  where
    toJSON (ValueName name) = object ["valueName" .= name]
    toJSON (TypeName name) = object ["typeName" .= name]
    toJSON (ConstructorName typeName name) = object [
        "constructorTypeName" .= typeName,
        "constructorName" .= name]

instance FromJSON UsedName where
    parseJSON = withObject "used name" (\o ->
        ValueName <$> o .: "valueName" <|>
        TypeName <$> o .: "typeName" <|>
        ConstructorName <$> o .: "constructorTypeName" <*> o .: "constructorName")

instance Hashable UsedName


-- Reference instances

deriving instance Show Reference
deriving instance Eq Reference
deriving instance Ord Reference
deriving instance Generic Reference

instance ToJSON Reference where
    toJSON (OtherSlice sliceID) = object ["otherSlice" .= sliceID]
    toJSON (Builtin originalModule) = object ["builtinModule" .= originalModule]

instance FromJSON Reference where
    parseJSON = withObject "reference" (\o ->
        OtherSlice <$> o .: "otherSlice" <|>
        Builtin <$> o .: "builtinModule")

instance Hashable Reference


-- Name instances

deriving instance Show Name
deriving instance Eq Name
deriving instance Ord Name
deriving instance Generic Name

instance ToJSON Name where
    toJSON (Identifier name) = object ["identifier" .= name]
    toJSON (Operator name) = object ["operator" .= name]

instance FromJSON Name where
    parseJSON = withObject "name" (\o ->
        Identifier <$> o .: "identifier" <|>
        Operator <$> o .: "operator")

instance Hashable Name


-- Instance instances

deriving instance Show Instance
deriving instance Eq Instance
deriving instance Ord Instance
deriving instance Generic Instance

instance ToJSON Instance where
    toJSON (Instance instancePart instanceID) =
        object ["instancePart" .= instancePart,"instanceID" .= instanceID]

instance FromJSON Instance where
    parseJSON = withObject "instance" (\o ->
        Instance <$> o .: "instancePart" <*> o .: "instanceID")

instance Hashable Instance


-- InstancePart instances

deriving instance Show InstancePart
deriving instance Eq InstancePart
deriving instance Ord InstancePart
deriving instance Generic InstancePart

instance ToJSON InstancePart where
    toJSON OfThisClass = toJSON ("OfThisClass" :: Text)
    toJSON OfThisClassForUnknownType = toJSON ("OfThisClassForUnknownType" :: Text)
    toJSON ForThisType = toJSON ("ForThisType" :: Text)
    toJSON ForThisTypeOfUnknownClass = toJSON ("ForThisTypeOfUnknownClass" :: Text)

instance FromJSON InstancePart where
    parseJSON = withText "instancePart" (\s -> case s of
        "OfThisClass" -> return OfThisClass
        "OfThisClassForUnknownType" -> return OfThisClassForUnknownType
        "ForThisType" -> return ForThisType
        "ForThisTypeOfUnknownClass" -> return ForThisTypeOfUnknownClass
        _ -> empty)

instance Hashable InstancePart

-- Slice parse errors

data SliceParseError = SliceParseError FilePath String

deriving instance Typeable SliceParseError
deriving instance Show SliceParseError

instance Exception SliceParseError

-- Reading and writing slices to disk

sliceDirectory :: FilePath
sliceDirectory = "fragnix" </> "slices"

writeSlice :: Slice -> IO ()
writeSlice slice@(Slice sliceID _ _ _ _) = writeSlice' (sliceDefaultPath sliceID) slice
  where
    writeSlice' slicePath slice = do
      createDirectoryIfMissing True (dropFileName slicePath)
      writeFile slicePath (encode slice)

readSlice :: SliceID -> IO Slice
readSlice sliceID = readSlice' (sliceDefaultPath sliceID)
  where
    readSlice' slicePath = do
      sliceFile <- readFile slicePath
      either (throwIO . SliceParseError slicePath) return (eitherDecode sliceFile)

-- Map the SliceID "12345" to the FilePath "sliceDirectory </> 1 </> 2 </> 12345"
sliceDefaultPath :: SliceID -> FilePath
sliceDefaultPath sliceID | Text.length sliceID < 2 = error $ "sliceID \"" <> unpack sliceID <> "\" has less than 2 characters"
                         | otherwise =
                             let
                                 a = Text.head sliceID
                                 b = Text.head (Text.tail sliceID)
                             in
                               sliceDirectory </> [a] </> [b] </> (unpack sliceID)

-- | Return all slices in sliceDirectory
getSlices :: IO [Slice]
getSlices = do
  sliceIDs <- getSliceIDs
  forM sliceIDs readSlice

-- | Return a list of all subdirectories of a given directory.
getSubDirs :: FilePath -> IO [FilePath]
getSubDirs fp = do
  filesAndDirs <- map (fp </>) <$> listDirectory fp
  filterM doesDirectoryExist filesAndDirs

-- | Return a list of all files of a given directory
getDirFiles :: FilePath -> IO [FilePath]
getDirFiles fp = do
  filesAndDirs <- map (fp </>) <$> listDirectory fp
  filterM doesFileExist filesAndDirs

-- | Return all sliceIDs in the sliceDirectory
getSliceIDs :: IO [SliceID]
getSliceIDs = do
  fstLvlDirs <- getSubDirs sliceDirectory
  sndLvlDirs <- concat <$> forM fstLvlDirs getSubDirs
  files <- concat <$> forM sndLvlDirs getDirFiles
  return $ map Text.pack files

