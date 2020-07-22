{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Fragnix.Update where

import Prelude hiding (writeFile,readFile)

import Data.Aeson (ToJSON, FromJSON, eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (Text)
import Data.Hashable (hash)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Data.ByteString.Lazy (writeFile,readFile)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import Control.Exception (Exception,throwIO)
import Data.Typeable(Typeable)

import Fragnix.Paths (updatePath)
import Fragnix.Slice (SliceID)

type UpdateID = Text
data Update = Update
  { updateID :: UpdateID
  , updateDescription :: Text
  , updateContent :: [(SliceID, SliceID)]
  }


-- Instances for Update

deriving instance Show Update
deriving instance Eq Update
deriving instance Generic Update
instance ToJSON Update
instance FromJSON Update

data UpdateParseError = UpdateParseError FilePath String

deriving instance Typeable UpdateParseError
deriving instance Show UpdateParseError

instance Exception UpdateParseError

-- | Write the update into .fragnix folder.
writeUpdate :: Update -> IO ()
writeUpdate upd = do
  let filepath = updatePath </> (Text.unpack $ updateID upd)
  createDirectoryIfMissing True updatePath
  writeFile filepath (encodePretty upd)


-- | Read the update with given ID from .fragnix folder.
readUpdate :: UpdateID -> IO Update
readUpdate upid = do
  let filepath = updatePath </> Text.unpack upid
  updateFile <- readFile filepath
  either (throwIO . UpdateParseError filepath) return (eitherDecode updateFile)


createUpdate :: Text -> [(SliceID, SliceID)] -> Update
createUpdate desc upd = Update upid desc upd
  where
    upid = (Text.pack . show . abs . hash) (desc, upd)

-- | Return all updates available in the fragnix folder. (Dummy implementation)
getUpdates :: IO [Update]
getUpdates = return [upd1, upd2, upd3]
  where
    upd1 = createUpdate "containers-4.2.13-to-containers-4.12.14" [("123123123","141241241")]
    upd2 = createUpdate "containers-4.2.14-to-containers-4.12.15" [("123122354","141241241")]
    upd3 = createUpdate "base-4.2.13-to-base-4.12.14" [("123123123","141241241")]
