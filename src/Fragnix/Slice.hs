{-# LANGUAGE OverloadedStrings,StandaloneDeriving,DeriveGeneric,DeriveDataTypeable #-}
module Fragnix.Slice where

import Prelude hiding (writeFile,readFile)

import Data.Aeson (
    ToJSON(toJSON),object,(.=),
    FromJSON(parseJSON),withObject,(.:),
    encode,eitherDecode)

import GHC.Generics (Generic)
import Data.Hashable (Hashable)

import Data.Text (Text)

import Control.Applicative ((<$>),(<*>),(<|>))

import Control.Exception (Exception,throwIO)
import Data.Typeable(Typeable)

import Data.ByteString.Lazy (writeFile,readFile)
import System.FilePath ((</>),dropFileName)
import System.Directory (createDirectoryIfMissing)

data Slice = Slice SliceID Language Fragment [Use]

data Language = Language [GHCExtension]

data Fragment = Fragment [SourceCode]

data Use = Use (Maybe Qualification) UsedName Reference

data Reference = OtherSlice SliceID | Primitive OriginalModule

data UsedName =
    ValueName Name |
    TypeName Name |
    ConstructorName TypeName Name |
    Instance

data Name = Identifier Text | Operator Text

type TypeName = Name

type SliceID = Integer
type SourceCode = Text
type Qualification = Text
type OriginalModule = Text
type GHCExtension = Text


-- Slice instances

deriving instance Show Slice

instance ToJSON Slice where
    toJSON (Slice sliceID language fragment uses) = object [
        "sliceID" .= sliceID,
        "language" .= language,
        "fragment" .= fragment,
        "uses" .= uses]

instance FromJSON Slice where
    parseJSON = withObject "slice" (\o ->
        Slice <$> o .: "sliceID" <*> o .: "language" <*> o .: "fragment" <*> o .: "uses")


-- Language instances

deriving instance Show Language
deriving instance Generic Language

instance ToJSON Language where
    toJSON (Language ghcextensions) = toJSON ghcextensions

instance FromJSON Language where
    parseJSON = fmap Language . parseJSON

instance Hashable Language


-- Fragment instances

deriving instance Show Fragment
deriving instance Generic Fragment

instance ToJSON Fragment where
    toJSON (Fragment declarations) = toJSON declarations

instance FromJSON Fragment where
    parseJSON = fmap Fragment . parseJSON

instance Hashable Fragment


-- Use instances

deriving instance Show Use
deriving instance Eq Use
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
    toJSON Instance = object [
        "instance" .= ("" :: Text)]

instance FromJSON UsedName where
    parseJSON = withObject "used name" (\o ->
        ValueName <$> o .: "valueName" <|>
        TypeName <$> o .: "typeName" <|>
        ConstructorName <$> o .: "constructorTypeName" <*> o .: "constructorName" <|>
        (const Instance :: Text -> UsedName) <$> o .: "instance")

instance Hashable UsedName


-- Reference instances

deriving instance Show Reference
deriving instance Eq Reference
deriving instance Generic Reference

instance ToJSON Reference where
    toJSON (OtherSlice sliceID) = object ["otherSlice" .= sliceID]
    toJSON (Primitive originalModule) = object ["originalModule" .= originalModule]

instance FromJSON Reference where
    parseJSON = withObject "reference" (\o ->
        OtherSlice <$> o .: "otherSlice" <|>
        Primitive <$> o .: "originalModule")

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

-- Slice parse errors

data SliceParseError = SliceParseError FilePath String

deriving instance Typeable SliceParseError
deriving instance Show SliceParseError

instance Exception SliceParseError

writeSlice :: FilePath -> Slice -> IO ()
writeSlice slicePath slice = do
    createDirectoryIfMissing True (dropFileName slicePath)
    writeFile slicePath (encode slice)

writeSliceDefault :: Slice -> IO ()
writeSliceDefault slice@(Slice sliceID _ _ _) = writeSlice (sliceDefaultPath sliceID) slice

readSlice :: FilePath -> IO Slice
readSlice slicePath = do
    sliceFile <- readFile slicePath
    either (throwIO . SliceParseError slicePath) return (eitherDecode sliceFile)

readSliceDefault :: SliceID -> IO Slice
readSliceDefault sliceID = readSlice (sliceDefaultPath sliceID)

sliceDefaultPath :: SliceID -> FilePath
sliceDefaultPath sliceID = sliceDirectory </> show sliceID

sliceDirectory :: FilePath
sliceDirectory = "fragnix" </> "slices"