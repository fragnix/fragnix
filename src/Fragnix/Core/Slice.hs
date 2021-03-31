{-# LANGUAGE StandaloneDeriving, DeriveGeneric, DeriveDataTypeable #-}
module Fragnix.Core.Slice where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)

-- Slices

type SliceID = Text

data Slice = Slice
  { sliceID :: SliceID
    -- ^ The hash by which this slice is identified.
  , language :: Language
    -- ^ The language extensions which have to be enabled in order to
    -- be able to compile this slice.
  , fragment :: Fragment
    -- ^ The actual source code contained in this slice.
  , uses :: [Use]
    -- ^ The dependencies of this slice.
  , instances :: [Instance]
    -- ^ The typeclass instances that this slide provides.
  }

deriving instance Show Slice
deriving instance Eq Slice
deriving instance Ord Slice
deriving instance Generic Slice
deriving instance Data Slice
deriving instance Typeable Slice

instance ToJSON Slice
instance FromJSON Slice

-- Language

type GHCExtension = Text

-- | A list of GHC language extensions.
data Language = Language { extensions :: [GHCExtension] }

deriving instance Show Language
deriving instance Eq Language
deriving instance Ord Language
deriving instance Generic Language
deriving instance Data Language
deriving instance Typeable Language

instance ToJSON Language
instance FromJSON Language
instance Hashable Language

-- Fragment

type SourceCode = Text

-- | Haskell source code fragments.
data Fragment = Fragment [SourceCode]

deriving instance Show Fragment
deriving instance Eq Fragment
deriving instance Ord Fragment
deriving instance Generic Fragment
deriving instance Data Fragment
deriving instance Typeable Fragment

instance ToJSON Fragment
instance FromJSON Fragment
instance Hashable Fragment

-- Use

type Qualification = Text

data Use = Use
  { qualification :: (Maybe Qualification)
  , usedName :: UsedName
  , reference :: Reference
  }

deriving instance Show Use
deriving instance Eq Use
deriving instance Ord Use
deriving instance Generic Use
deriving instance Data Use
deriving instance Typeable Use

instance ToJSON Use
instance FromJSON Use
instance Hashable Use

-- Instance

type InstanceID = SliceID

data Instance = Instance
  { instancePart :: InstancePart
  , instanceID :: InstanceID
  }

deriving instance Show Instance
deriving instance Eq Instance
deriving instance Ord Instance
deriving instance Generic Instance
deriving instance Data Instance
deriving instance Typeable Instance

instance ToJSON Instance
instance FromJSON Instance
instance Hashable Instance

-- Instance Part

data InstancePart =
    OfThisClass |
    OfThisClassForUnknownType |
    ForThisType |
    ForThisTypeOfUnknownClass

deriving instance Show InstancePart
deriving instance Eq InstancePart
deriving instance Ord InstancePart
deriving instance Generic InstancePart
deriving instance Data InstancePart
deriving instance Typeable InstancePart

instance ToJSON InstancePart
instance FromJSON InstancePart
instance Hashable InstancePart

-- UsedName

type TypeName = Name

data UsedName =
    ValueName { valueName :: Name } |
    TypeName { typeName :: Name } |
    ConstructorName { constructorTypeName :: TypeName, constructorName :: Name }

deriving instance Show UsedName
deriving instance Eq UsedName
deriving instance Ord UsedName
deriving instance Generic UsedName
deriving instance Data UsedName
deriving instance Typeable UsedName

instance ToJSON UsedName
instance FromJSON UsedName
instance Hashable UsedName

-- Reference

type OriginalModule = Text

data Reference = OtherSlice SliceID | Builtin OriginalModule

deriving instance Show Reference
deriving instance Eq Reference
deriving instance Ord Reference
deriving instance Generic Reference
deriving instance Data Reference
deriving instance Typeable Reference

instance ToJSON Reference
instance FromJSON Reference
instance Hashable Reference

-- Name

data Name = Identifier Text | Operator Text

deriving instance Show Name
deriving instance Eq Name
deriving instance Ord Name
deriving instance Generic Name
deriving instance Data Name
deriving instance Typeable Name

instance ToJSON Name
instance FromJSON Name
instance Hashable Name

