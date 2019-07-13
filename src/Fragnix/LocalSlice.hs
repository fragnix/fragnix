{-# LANGUAGE StandaloneDeriving,DeriveGeneric #-}
module Fragnix.LocalSlice where

import Fragnix.Slice (
    SliceID, OriginalModule, Language, Fragment, Qualification, UsedName,
    InstancePart, InstanceID)

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


deriving instance Show LocalSlice
deriving instance Eq LocalSlice
deriving instance Ord LocalSlice
deriving instance Generic LocalSlice

deriving instance Show LocalSliceID
deriving instance Eq LocalSliceID
deriving instance Ord LocalSliceID
deriving instance Generic LocalSliceID

deriving instance Show LocalUse
deriving instance Eq LocalUse
deriving instance Ord LocalUse
deriving instance Generic LocalUse

deriving instance Show LocalReference
deriving instance Eq LocalReference
deriving instance Ord LocalReference
deriving instance Generic LocalReference

deriving instance Show LocalInstance
deriving instance Eq LocalInstance
deriving instance Ord LocalInstance
deriving instance Generic LocalInstance


