{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
module Fragnix.Core.Config where

import Fragnix.Core.Environment (Environment)
import Fragnix.Core.ForeignSlice (ForeignSlice)
import Fragnix.Core.Slice (Slice)

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Config = Config
  { name          :: Text
  , authors       :: Text
  , description   :: Text
  , environments  :: [Environment]
  , slices        :: [Slice]
  , foreignSlices :: [ForeignSlice]
  , mainIs        :: [FilePath]
  , license       :: Text
  , categories    :: [Text]
  }

deriving instance Show Config
deriving instance Eq Config
deriving instance Ord Config
deriving instance Generic Config
instance ToJSON Config
instance FromJSON Config
