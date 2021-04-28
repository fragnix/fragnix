{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
module Fragnix.Core.ForeignSlice where

import Fragnix.Core.Slice (SliceID)

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data ForeignSlice = ForeignSlice
  { sliceID   :: SliceID
  , code      :: Text
  , fileName  :: Text
  }

deriving instance Show ForeignSlice
deriving instance Eq ForeignSlice
deriving instance Ord ForeignSlice
deriving instance Generic ForeignSlice
instance ToJSON ForeignSlice
instance FromJSON ForeignSlice
