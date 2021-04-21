{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Fragnix.Core.Loaf where

import Fragnix.Core.Slice (SliceID)

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data, Typeable)
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)

type LoafID = Text

data Loaf = Loaf
  { loafID :: LoafID
  , name   :: Text
  , slices :: [SliceID]
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance FromJSON Loaf
instance ToJSON Loaf
instance Hashable Loaf
