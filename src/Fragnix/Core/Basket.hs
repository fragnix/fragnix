module Fragnix.Core.Basket where

import Fragnix.Core.Loaf (LoafID)

import Data.Map (Map)
import Data.Text (Text)

type Basket = Map Text [LoafID]

