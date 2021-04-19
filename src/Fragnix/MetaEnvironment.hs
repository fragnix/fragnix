module Fragnix.MetaEnvironment (
  resolve
) where

import Fragnix.Core.Environment (Environment, EnvironmentID)
import Fragnix.Core.MetaEnvironment (MetaEnvironment)

import qualified Data.Map as Map (fromList, union)

resolve :: [(EnvironmentID, Environment)] -> MetaEnvironment -> MetaEnvironment
resolve envs meta = Map.union meta $ Map.fromList envs
