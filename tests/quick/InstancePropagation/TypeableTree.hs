{-# LANGUAGE  DeriveDataTypeable, StandaloneDeriving #-}
module TypeableTree where

import Data.Typeable (Typeable)
import Tree (Tree)

deriving instance Typeable Tree