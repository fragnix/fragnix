{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
module Fragnix.Core.Config where

import Fragnix.Core.Basket (Basket)
import Fragnix.Core.Slice (SliceID)

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Config = Config
  { name          :: Text
  , authors       :: Text
  , description   :: Text
  , loaves        :: Basket
  , slices        :: [SliceID]
  , foreignSlices :: [SliceID]
  , mainIs        :: FilePath
  , cbitsFolder   :: FilePath
  , license       :: Text
  , categories    :: [Text]
  }

deriving instance Show Config
deriving instance Eq Config
deriving instance Ord Config
deriving instance Generic Config
instance ToJSON Config
instance FromJSON Config
