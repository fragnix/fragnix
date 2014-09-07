{-# LANGUAGE OverloadedStrings,StandaloneDeriving #-}
module Fragnix.Slice where

import Data.Aeson (
    ToJSON(toJSON),object,(.=),
    FromJSON(parseJSON),withObject,(.:))

import Data.Text (Text)

import Control.Applicative ((<$>),(<*>),(<|>))


data Slice = Slice SliceID Fragment [Usage]

deriving instance Show Slice

instance ToJSON Slice where
    toJSON (Slice sliceID fragment usages) = object [
        "sliceID" .= sliceID,
        "fragment" .= fragment,
        "usages" .= usages]

instance FromJSON Slice where
    parseJSON = withObject "slice" (\o ->
        Slice <$> o .: "sliceID" <*> o .: "fragment" <*> o .: "usages")

data Fragment = Binding Signature SourceCode

deriving instance Show Fragment

instance ToJSON Fragment where
    toJSON (Binding signature sourceCode) = object [
        "binding" .= object [
            "signature" .= signature,
            "sourceCode" .= sourceCode]]

instance FromJSON Fragment where
    parseJSON = withObject "fragment" (\o -> do
        binding <- o .: "binding"
        Binding <$> binding .: "signature" <*> binding .: "sourceCode")

data Usage = Usage (Maybe Qualification) UsedName Reference

deriving instance Show Usage

instance ToJSON Usage where
    toJSON (Usage qualification usedName reference) = object [
        "qualification" .= qualification,
        "usedName" .= usedName,
        "reference" .= reference]

instance FromJSON Usage where
    parseJSON = withObject "usage" (\o ->
        Usage <$> o .: "qualification" <*> o .: "usedName" <*> o .: "reference")

data UsedName =
    VarId Text |
    ConId Text |
    VarSym Text |
    ConSym Text

deriving instance Show UsedName
deriving instance Eq UsedName
deriving instance Ord UsedName

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

data Reference = OtherSlice SliceID | Primitive OriginalModule

deriving instance Show Reference

instance ToJSON Reference where
    toJSON (OtherSlice sliceID) = object ["otherSlice" .= sliceID]
    toJSON (Primitive originalModule) = object ["originalModule" .= originalModule]

instance FromJSON Reference where
    parseJSON = withObject "reference" (\o ->
        OtherSlice <$> o .: "otherSlice" <|>
        Primitive <$> o .: "originalModule")

type SliceID = Integer
type SourceCode = Text
type Signature = SourceCode
type Qualification = Text
type OriginalModule = Text


