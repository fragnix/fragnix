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
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing,doesFileExist)

data Slice = Slice SliceID Language Fragment [Usage]

data Language = Language [GHCExtension]

data Fragment = Fragment [SourceCode]

data Usage = Usage (Maybe Qualification) UsedName Reference

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
    toJSON (Slice sliceID language fragment usages) = object [
        "sliceID" .= sliceID,
        "language" .= language,
        "fragment" .= fragment,
        "usages" .= usages]

instance FromJSON Slice where
    parseJSON = withObject "slice" (\o ->
        Slice <$> o .: "sliceID" <*> o .: "language" <*> o .: "fragment" <*> o .: "usages")


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


-- Usage instances

deriving instance Show Usage
deriving instance Eq Usage
deriving instance Generic Usage

instance ToJSON Usage where
    toJSON (Usage qualification usedName reference) = object [
        "qualification" .= qualification,
        "usedName" .= usedName,
        "reference" .= reference]

instance FromJSON Usage where
    parseJSON = withObject "usage" (\o ->
        Usage <$> o .: "qualification" <*> o .: "usedName" <*> o .: "reference")

instance Hashable Usage


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

data SliceParseError = SliceParseError SliceID String

deriving instance Typeable SliceParseError
deriving instance Show SliceParseError

instance Exception SliceParseError

writeSlice :: Slice -> IO ()
writeSlice slice@(Slice sliceID _ _ _) = do
    createDirectoryIfMissing True sliceDirectory
    writeFile (slicePath sliceID) (encode slice)

readSlice :: SliceID -> IO Slice
readSlice sliceID = do
    sliceFile <- readFile (slicePath sliceID)
    either (throwIO . SliceParseError sliceID) return (eitherDecode sliceFile)

doesSliceExist :: SliceID -> IO Bool
doesSliceExist sliceID = doesFileExist (slicePath sliceID)

slicePath :: SliceID -> FilePath
slicePath sliceID = sliceDirectory </> show sliceID

sliceDirectory :: FilePath
sliceDirectory = "fragnix" </> "slices"