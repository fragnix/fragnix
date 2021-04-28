{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Fragnix.Core.Loaf where

import Fragnix.Core.Slice (Reference (..))

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data, Typeable)
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)

data Name
    = Ident Text   -- ^ /varid/ or /conid/.
    | Symbol Text   -- ^ /varsym/ or /consym/
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

instance FromJSON Name
instance ToJSON Name
instance Hashable Name


-- | Information about an entity. Carries at least the module it was originally
-- declared in and its name.
data Symbol
    = Value
      { symbolModule :: Reference
      , symbolName   :: Name
      }
      -- ^ value or function
    | Method
      { symbolModule :: Reference
      , symbolName   :: Name
      , className    :: Name
      }
      -- ^ class method
    | Selector
      { symbolModule :: Reference
      , symbolName   :: Name
      , typeName     :: Name
      , constructors :: [Name]
      }
      -- ^ record field selector
    | Constructor
      { symbolModule :: Reference
      , symbolName   :: Name
      , typeName     :: Name
      }
      -- ^ data constructor
    | Type
      { symbolModule :: Reference
      , symbolName   :: Name
      }
      -- ^ type synonym
    | Data
      { symbolModule :: Reference
      , symbolName   :: Name
      }
      -- ^ data type
    | NewType
      { symbolModule :: Reference
      , symbolName   :: Name
      }
      -- ^ newtype
    | TypeFam
      { symbolModule :: Reference
      , symbolName   :: Name
      , associate    :: Maybe Name
      }
      -- ^ type family
    | DataFam
      { symbolModule :: Reference
      , symbolName   :: Name
      , associate    :: Maybe Name
      }
      -- ^ data family
    | Class
      { symbolModule :: Reference
      , symbolName   :: Name
      }
      -- ^ type class
    | PatternConstructor
      { symbolModule    :: Reference
      , symbolName      :: Name
      , patternTypeName :: Maybe Name
      }
      -- ^ pattern synonym constructor
    | PatternSelector
      { symbolModule           :: Reference
      , symbolName             :: Name
      , patternTypeName        :: Maybe Name
      , patternConstructorName :: Name
      }
      -- ^ pattern synonym selector
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance FromJSON Symbol
instance ToJSON Symbol
instance Hashable Symbol

type LoafID = Text

data Loaf = Loaf
  { loafID  :: LoafID
  , name    :: Text
  , symbols :: [Symbol]
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance FromJSON Loaf
instance ToJSON Loaf
instance Hashable Loaf
