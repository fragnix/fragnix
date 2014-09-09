{-# LANGUAGE OverloadedStrings,StandaloneDeriving,DeriveGeneric #-}
module Fragnix.Slice where

import Data.Aeson (
    ToJSON(toJSON),object,(.=),
    FromJSON(parseJSON),withObject,(.:))

import GHC.Generics (Generic)
import Data.Hashable (Hashable)

import Data.Text (Text)

import Control.Applicative ((<$>),(<*>),(<|>))


data Slice = Slice SliceID Fragment [Usage]

data Fragment = Fragment [SourceCode]

data Usage = Usage (Maybe Qualification) UsedName Reference

data Reference = OtherSlice SliceID | Primitive OriginalModule

data UsedName =
    VarId Text |
    ConId Text |
    VarSym Text |
    ConSym Text

type SliceID = Integer
type SourceCode = Text
type Qualification = Text
type OriginalModule = Text

deriving instance Show Slice

instance ToJSON Slice where
    toJSON (Slice sliceID fragment usages) = object [
        "sliceID" .= sliceID,
        "fragment" .= fragment,
        "usages" .= usages]

instance FromJSON Slice where
    parseJSON = withObject "slice" (\o ->
        Slice <$> o .: "sliceID" <*> o .: "fragment" <*> o .: "usages")


deriving instance Show Fragment
deriving instance Generic Fragment

instance ToJSON Fragment where
    toJSON (Fragment declarations) = toJSON declarations

instance FromJSON Fragment where
    parseJSON = fmap Fragment . parseJSON

instance Hashable Fragment


deriving instance Show Usage
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


deriving instance Show UsedName
deriving instance Eq UsedName
deriving instance Ord UsedName
deriving instance Generic UsedName

instance ToJSON UsedName  where
    toJSON (VarId name) = object ["varId" .= name]
    toJSON (ConId name) = object ["conId" .= name]
    toJSON (VarSym name) = object ["varSym" .= name]
    toJSON (ConSym name) = object ["conSym" .= name]

instance FromJSON UsedName where
    parseJSON = withObject "used name" (\o ->
        VarId <$> o .: "varId" <|>
        ConId <$> o .: "conId" <|>
        VarSym <$> o .: "varSym" <|>
        ConSym <$> o .: "conSym")

instance Hashable UsedName


deriving instance Show Reference
deriving instance Generic Reference

instance ToJSON Reference where
    toJSON (OtherSlice sliceID) = object ["otherSlice" .= sliceID]
    toJSON (Primitive originalModule) = object ["originalModule" .= originalModule]

instance FromJSON Reference where
    parseJSON = withObject "reference" (\o ->
        OtherSlice <$> o .: "otherSlice" <|>
        Primitive <$> o .: "originalModule")

instance Hashable Reference

