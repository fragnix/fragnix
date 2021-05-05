{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
module Fragnix.Core.ForeignSlice where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

type ForeignSliceID = Text

data ForeignSlice = ForeignSlice
  { sliceID  :: ForeignSliceID
  , code     :: Text
  , fileName :: Text
  }

deriving instance Show ForeignSlice
deriving instance Eq ForeignSlice
deriving instance Ord ForeignSlice
deriving instance Generic ForeignSlice
instance ToJSON ForeignSlice
instance FromJSON ForeignSlice
