{-# LANGUAGE OverloadedStrings,StandaloneDeriving,DeriveGeneric,GeneralizedNewtypeDeriving #-}
module Fragnix.LocalSlice
  ( LocalSlice(..)
  , LocalSliceID(..)
  , LocalUse(..)
  , LocalReference(..)
  , LocalInstance(..)
  , LocalInstanceID
  ) where

import Fragnix.Slice (
    SliceID, OriginalModule, Language, Fragment, Qualification, UsedName,
    InstancePart, InstanceID)

import Data.Aeson (FromJSON, ToJSON, ToJSONKey, FromJSONKey)

import Data.Text (Text)
import GHC.Generics (Generic)


-- | A local ID before slices have been hashed.
-- It has to be a valid Haskell module name.
newtype LocalSliceID = LocalSliceID Text

deriving instance Show LocalSliceID
deriving instance Eq LocalSliceID
deriving instance Ord LocalSliceID
deriving instance Generic LocalSliceID

instance ToJSON LocalSliceID
instance FromJSON LocalSliceID

deriving instance ToJSONKey LocalSliceID
deriving instance FromJSONKey LocalSliceID

-- | A Slice with a local ID that may use slices with local IDs as well as global
-- slices with slice IDs.
data LocalSlice = LocalSlice
  { localSliceID :: LocalSliceID
  , language :: Language
  , fragment :: Fragment
  , localUses :: [LocalUse]
  , localInstances :: [LocalInstance]
  }

deriving instance Show LocalSlice
deriving instance Eq LocalSlice
deriving instance Ord LocalSlice
deriving instance Generic LocalSlice

instance ToJSON LocalSlice
instance FromJSON LocalSlice

-- | A local use may refer to local slices and global slices.
data LocalUse = LocalUse
  { qualification :: Maybe Qualification
  , usedName :: UsedName
  , localReference :: LocalReference
  }

deriving instance Show LocalUse
deriving instance Eq LocalUse
deriving instance Ord LocalUse
deriving instance Generic LocalUse

instance ToJSON LocalUse
instance FromJSON LocalUse

data LocalReference =
    OtherSlice SliceID |
    Builtin OriginalModule |
    OtherLocalSlice LocalSliceID

deriving instance Show LocalReference
deriving instance Eq LocalReference
deriving instance Ord LocalReference
deriving instance Generic LocalReference

instance ToJSON LocalReference
instance FromJSON LocalReference


-- TODO: Reorganize in order to avoid partial record labels.
data LocalInstance =
    LocalInstance { instancePart :: InstancePart, localInstanceID :: LocalInstanceID } |
    GlobalInstance { instancePart :: InstancePart, globalInstanceID :: InstanceID }

type LocalInstanceID = LocalSliceID


deriving instance Show LocalInstance
deriving instance Eq LocalInstance
deriving instance Ord LocalInstance
deriving instance Generic LocalInstance

instance ToJSON LocalInstance
instance FromJSON LocalInstance

