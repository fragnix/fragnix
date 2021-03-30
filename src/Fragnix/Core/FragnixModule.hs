{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
module Fragnix.Core.FragnixModule where

import Data.Typeable
import Data.Data
import Data.Map (Map)

import GHC.Generics (Generic)

-- The definition of FragnixModule is a copy of the definition of Environment from the haskell-names package.

-- | This type is used to represent variables, and also constructors.
data Name l
    = Ident  l String   -- ^ /varid/ or /conid/.
    | Symbol l String   -- ^ /varsym/ or /consym/
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)


-- | The name of a Haskell module.
data ModuleName l = ModuleName l String
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Information about an entity. Carries at least the module it was originally
-- declared in and its name.
data Symbol
    = Value
      { symbolModule :: ModuleName ()
      , symbolName :: Name ()
      }
      -- ^ value or function
    | Method
      { symbolModule :: ModuleName ()
      , symbolName :: Name ()
      , className :: Name ()
      }
      -- ^ class method
    | Selector
      { symbolModule :: ModuleName ()
      , symbolName :: Name ()
      , typeName :: Name ()
      , constructors :: [Name ()]
      }
      -- ^ record field selector
    | Constructor
      { symbolModule :: ModuleName ()
      , symbolName :: Name ()
      , typeName :: Name ()
      }
      -- ^ data constructor
    | Type
      { symbolModule :: ModuleName ()
      , symbolName :: Name ()
      }
      -- ^ type synonym
    | Data
      { symbolModule :: ModuleName ()
      , symbolName :: Name ()
      }
      -- ^ data type
    | NewType
      { symbolModule :: ModuleName ()
      , symbolName :: Name ()
      }
      -- ^ newtype
    | TypeFam
      { symbolModule :: ModuleName ()
      , symbolName :: Name ()
      , associate :: Maybe (Name ())
      }
      -- ^ type family
    | DataFam
      { symbolModule :: ModuleName ()
      , symbolName :: Name ()
      , associate :: Maybe (Name ())
      }
      -- ^ data family
    | Class
      { symbolModule :: ModuleName ()
      , symbolName :: Name ()
      }
      -- ^ type class
    | PatternConstructor
      { symbolModule :: ModuleName ()
      , symbolName :: Name ()
      , patternTypeName :: Maybe (Name ())
      }
      -- ^ pattern synonym constructor
    | PatternSelector
      { symbolModule :: ModuleName ()
      , symbolName :: Name ()
      , patternTypeName :: Maybe (Name ())
      , patternConstructorName :: Name ()
      }
      -- ^ pattern synonym selector
    deriving (Eq, Ord, Show, Data, Typeable)

-- | A map from module name to list of symbols it exports.
type Environment = Map (ModuleName ()) [Symbol]
