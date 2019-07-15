{-# LANGUAGE OverloadedStrings,StandaloneDeriving,DeriveGeneric,GeneralizedNewtypeDeriving #-}
module Fragnix.LocalSlice where

import Fragnix.Slice (
    SliceID, OriginalModule, Language, Fragment, Qualification, UsedName,
    InstancePart, InstanceID)

import Data.Aeson (
    ToJSON(toJSON),object,(.=),
    FromJSON(parseJSON),withObject,(.:),
    ToJSONKey,FromJSONKey)

import Control.Applicative ((<$>),(<*>),(<|>))
import Data.Text (Text)
import GHC.Generics (Generic)


-- | A local ID before slices have been hashed.
newtype LocalSliceID = LocalSliceID Text

-- | A Slice with a local ID that may use slices with local IDs as well as global
-- slices with slice IDs.
data LocalSlice = LocalSlice LocalSliceID Language Fragment [LocalUse] [LocalInstance]

-- | A local use may refer to local slices and global slices.
data LocalUse = LocalUse (Maybe Qualification) UsedName LocalReference

data LocalReference =
    OtherSlice SliceID |
    Builtin OriginalModule |
    OtherLocalSlice LocalSliceID

data LocalInstance =
    LocalInstance InstancePart LocalInstanceID |
    GlobalInstance InstancePart InstanceID

type LocalInstanceID = LocalSliceID


-- LocalSliceID instances

deriving instance Show LocalSliceID
deriving instance Eq LocalSliceID
deriving instance Ord LocalSliceID
deriving instance Generic LocalSliceID

instance ToJSON LocalSliceID where
    toJSON (LocalSliceID localSliceID) = toJSON localSliceID

instance FromJSON LocalSliceID where
    parseJSON x = LocalSliceID <$> parseJSON x

deriving instance ToJSONKey LocalSliceID
deriving instance FromJSONKey LocalSliceID

-- LocalSlice instances

deriving instance Show LocalSlice
deriving instance Eq LocalSlice
deriving instance Ord LocalSlice
deriving instance Generic LocalSlice

instance ToJSON LocalSlice where
    toJSON (LocalSlice localSliceID language fragment localUses localInstances) = object [
        "localSliceID" .= localSliceID,
        "language" .= language,
        "fragment" .= fragment,
        "localUses" .= localUses,
        "localInstances" .= localInstances]

instance FromJSON LocalSlice where
    parseJSON = withObject "localSlice" (\o ->
        LocalSlice <$>
            o .: "localSliceID" <*>
            o .: "language" <*>
            o .: "fragment" <*>
            o .: "localUses" <*>
            o .: "localInstances")

-- LocalUseInstances

deriving instance Show LocalUse
deriving instance Eq LocalUse
deriving instance Ord LocalUse
deriving instance Generic LocalUse

instance ToJSON LocalUse where
    toJSON (LocalUse qualification usedName localReference) = object [
        "qualification" .= qualification,
        "usedName" .= usedName,
        "localReference" .= localReference]

instance FromJSON LocalUse where
    parseJSON = withObject "localUse" (\o ->
        LocalUse <$> o .: "qualification" <*> o .: "usedName" <*> o .: "localReference")

-- LocalReference instances

deriving instance Show LocalReference
deriving instance Eq LocalReference
deriving instance Ord LocalReference
deriving instance Generic LocalReference

instance ToJSON LocalReference where
    toJSON (OtherSlice sliceID) = object ["otherSlice" .= sliceID]
    toJSON (Builtin originalModule) = object ["builtinModule" .= originalModule]
    toJSON (OtherLocalSlice sliceID) = object ["otherLocalSlice" .= sliceID]

instance FromJSON LocalReference where
    parseJSON = withObject "localReference" (\o ->
        OtherSlice <$> o .: "otherSlice" <|>
        Builtin <$> o .: "builtinModule" <|>
        OtherLocalSlice <$> o .: "otherLocalSlice")

-- LocalInstance instances

deriving instance Show LocalInstance
deriving instance Eq LocalInstance
deriving instance Ord LocalInstance
deriving instance Generic LocalInstance

instance ToJSON LocalInstance where
    toJSON (LocalInstance instancePart localInstanceID) =
        object ["instancePart" .= instancePart,"localInstanceID" .= localInstanceID]
    toJSON (GlobalInstance instancePart globalInstanceID) =
        object ["instancePart" .= instancePart,"globalInstanceID" .= globalInstanceID]

instance FromJSON LocalInstance where
    parseJSON = withObject "localInstance" (\o ->
        LocalInstance <$> o .: "instancePart" <*> o .: "localInstanceID" <|>
        GlobalInstance <$> o .: "instancePart" <*> o .: "globalInstanceID")


