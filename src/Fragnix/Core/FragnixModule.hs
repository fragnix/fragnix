{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Fragnix.Core.FragnixModule where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

-- The definition of FragnixModule is a copy of the definition of Environment from the haskell-names package.

-- | This type is used to represent variables, and also constructors.
data Name
    = Ident Text   -- ^ /varid/ or /conid/.
    | Symbol Text   -- ^ /varsym/ or /consym/
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

instance FromJSON Name
instance ToJSON Name

-- | The name of a Haskell module.
data ModuleName = ModuleName Text
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

instance FromJSON ModuleName
instance ToJSON ModuleName

-- | Information about an entity. Carries at least the module it was originally
-- declared in and its name.
data Symbol
    = Value
      { symbolModule :: ModuleName
      , symbolName :: Name
      }
      -- ^ value or function
    | Method
      { symbolModule :: ModuleName
      , symbolName :: Name
      , className :: Name
      }
      -- ^ class method
    | Selector
      { symbolModule :: ModuleName
      , symbolName :: Name
      , typeName :: Name
      , constructors :: [Name]
      }
      -- ^ record field selector
    | Constructor
      { symbolModule :: ModuleName
      , symbolName :: Name
      , typeName :: Name
      }
      -- ^ data constructor
    | Type
      { symbolModule :: ModuleName
      , symbolName :: Name
      }
      -- ^ type synonym
    | Data
      { symbolModule :: ModuleName
      , symbolName :: Name
      }
      -- ^ data type
    | NewType
      { symbolModule :: ModuleName
      , symbolName :: Name
      }
      -- ^ newtype
    | TypeFam
      { symbolModule :: ModuleName
      , symbolName :: Name
      , associate :: Maybe Name
      }
      -- ^ type family
    | DataFam
      { symbolModule :: ModuleName
      , symbolName :: Name
      , associate :: Maybe Name
      }
      -- ^ data family
    | Class
      { symbolModule :: ModuleName
      , symbolName :: Name
      }
      -- ^ type class
    | PatternConstructor
      { symbolModule :: ModuleName
      , symbolName :: Name
      , patternTypeName :: Maybe Name
      }
      -- ^ pattern synonym constructor
    | PatternSelector
      { symbolModule :: ModuleName
      , symbolName :: Name
      , patternTypeName :: Maybe Name
      , patternConstructorName :: Name
      }
      -- ^ pattern synonym selector
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance FromJSON Symbol
instance ToJSON Symbol

type ModuleID = Text

data FragnixModule = FragnixModule
  { moduleName :: ModuleName
  , moduleId :: ModuleID
  , moduleExports :: [Symbol]
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance FromJSON FragnixModule
instance ToJSON FragnixModule

