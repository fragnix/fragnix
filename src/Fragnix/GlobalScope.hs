module Fragnix.GlobalScope where

import Fragnix.Slice (SliceID)
import Fragnix.Symbol (Symbol)

import Data.Map (Map)

type GlobalScope = Map Symbol SliceID

