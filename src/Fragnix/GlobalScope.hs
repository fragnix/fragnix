module Fragnix.GlobalScope where

import Fragnix.Slice (SliceID)
import Language.Haskell.Names (Symbol)

import Data.Map (Map)

type GlobalScope = Map Symbol SliceID

