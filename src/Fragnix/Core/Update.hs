{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
module Fragnix.Core.Update where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Fragnix.Core.Slice

type Update = [(SliceID, SliceID)]
type UpdateID = Text

data PersistedUpdate = PersistedUpdate
  { updateID :: UpdateID
  , updateDescription :: Text
  , updateContent :: Update
  }

deriving instance Show PersistedUpdate
deriving instance Eq PersistedUpdate
deriving instance Generic PersistedUpdate
instance ToJSON PersistedUpdate
instance FromJSON PersistedUpdate
