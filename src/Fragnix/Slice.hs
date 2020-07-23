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
  , sliceModuleName
  , moduleNameSliceID
  , usedSliceIDs
  , readSlice
  , writeSlice
  , loadSlicesTransitive
  , getSlices
  ) where

import Prelude hiding (writeFile,readFile)

import Data.Aeson (
    ToJSON(toJSON),object,(.=),
    FromJSON(parseJSON),withObject,(.:),withText,
    eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)

import GHC.Generics (Generic)
import Data.Hashable (Hashable)

import Data.Text (Text)
import qualified Data.Text as Text (unpack,pack,length,index)

import Control.Monad.Trans.State.Strict (StateT,execStateT,get,put)
import Control.Monad.IO.Class (liftIO)

import Control.Applicative ((<|>),empty)

import Control.Monad (forM, forM_, unless)
import Control.Exception (Exception,throwIO)
import Data.Typeable(Typeable)

import Data.ByteString.Lazy (writeFile,readFile)
import System.FilePath ((</>),dropFileName)
import System.Directory (createDirectoryIfMissing)
import Data.Char (isDigit)
import Fragnix.Utils (listFilesRecursive)


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

-- | The name we give to the module generated for a slice with the given ID.
sliceModuleName :: SliceID -> String
sliceModuleName sliceID = "F" ++ Text.unpack sliceID

-- | Is the module name from a fragnix generated module
moduleNameSliceID :: String -> Maybe SliceID
moduleNameSliceID ('F':rest)
  | all isDigit rest = Just (Text.pack rest)
  | otherwise = Nothing
moduleNameSliceID _ = Nothing

-- | Write the given slice to the given directory
writeSlice :: FilePath -> Slice -> IO ()
writeSlice slicesPath slice@(Slice sliceID _ _ _ _) = do
  let slicePath = slicesPath </> sliceNestedPath sliceID
  createDirectoryIfMissing True (dropFileName slicePath)
  writeFile slicePath (encodePretty slice)

-- | Read the slice with the given slice ID from the given directory
readSlice :: FilePath -> SliceID -> IO Slice
readSlice slicesPath sliceID = do
  let slicePath = slicesPath </> sliceNestedPath sliceID
  sliceFile <- readFile slicePath
  either (throwIO . SliceParseError slicePath) return (eitherDecode sliceFile)

-- | Given slice IDs load all slices and all instance slices nedded
-- for compilation.
loadSlicesTransitive :: FilePath -> [SliceID] -> IO [Slice]
loadSlicesTransitive slicesPath sliceIDs = do
    sliceIDs <- loadSliceIDsTransitive slicesPath sliceIDs
    forM sliceIDs (readSlice slicesPath)

-- | Given slice IDs find all IDs of all the slices needed
-- for compilation.
loadSliceIDsTransitive :: FilePath -> [SliceID] -> IO [SliceID]
loadSliceIDsTransitive slicesPath sliceIDs = execStateT (forM sliceIDs (loadSliceIDsStateful slicesPath)) []

-- | Given a slice ID load all IDs of all the slices needed for
-- compilation. Keep track of visited slice IDs to avoid loops.
loadSliceIDsStateful :: FilePath -> SliceID -> StateT [SliceID] IO ()
loadSliceIDsStateful slicesPath sliceID = do
    seenSliceIDs <- get
    unless (elem sliceID seenSliceIDs) (do
        put (sliceID : seenSliceIDs)
        slice <- liftIO (readSlice slicesPath sliceID)
        let recursiveSliceIDs = usedSliceIDs slice
            recursiveInstanceSliceIDs = sliceInstanceIDs slice
        forM_ recursiveSliceIDs (loadSliceIDsStateful slicesPath)
        forM_ recursiveInstanceSliceIDs (loadSliceIDsStateful slicesPath))

usedSliceIDs :: Slice -> [SliceID]
usedSliceIDs (Slice _ _ _ uses _) = do
    Use _ _ (OtherSlice sliceID) <- uses
    return sliceID

sliceInstanceIDs :: Slice -> [InstanceID]
sliceInstanceIDs (Slice _ _ _ _ instances) = do
    Instance _ instanceID <- instances
    return instanceID


-- | Return all slices in the given directory
getSlices :: FilePath -> IO [Slice]
getSlices path = do
  slicePaths <- listFilesRecursive path
  forM slicePaths (\slicePath -> do
    sliceFile <- readFile slicePath
    either (throwIO . SliceParseError slicePath) return (eitherDecode sliceFile))

-- | Map the SliceID "12345" to the FilePath "1 </> 2 </> 12345"
sliceNestedPath :: SliceID -> FilePath
sliceNestedPath sliceID
  | Text.length sliceID < 2 = error $ "sliceID \"" <> Text.unpack sliceID <> "\" has less than 2 characters"
  | otherwise = [Text.index sliceID 0] </> [Text.index sliceID 1] </> (Text.unpack sliceID)

