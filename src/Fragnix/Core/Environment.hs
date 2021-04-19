module Fragnix.Core.Environment where

import Data.Map (Map)
import Data.Text (Text)
import Fragnix.Core.Slice (SliceID)

type EnvironmentID = Text

type Environment = Map Text SliceID
