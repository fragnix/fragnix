module Fragnix.GlobalScope where

import Fragnix.Slice (SliceID)
import Language.Haskell.Names (Symbol,OrigName)

import Data.Map (Map)

type GlobalScope = Map (Symbol OrigName) SliceID

