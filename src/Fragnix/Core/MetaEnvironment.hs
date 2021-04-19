module Fragnix.Core.MetaEnvironment where

import Data.Map (Map)
import Fragnix.Core.Environment (Environment, EnvironmentID)

type MetaEnvironment = Map EnvironmentID Environment
