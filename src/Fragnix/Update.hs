{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Fragnix.Update where

import Prelude hiding (writeFile,readFile)

import Fragnix.Slice (Slice, SliceID)
import Fragnix.LocalSlice (LocalSlice, LocalSliceID)
import Fragnix.Paths (updatePath, environmentPath)
import Fragnix.Utils (listFilesRecursive)
import Fragnix.Environment (loadEnvironment, persistEnvironment)

import Data.Aeson (ToJSON, FromJSON, eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map ()
import Data.Text (Text)
import Data.Hashable (hash)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Data.ByteString.Lazy (writeFile,readFile)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import Control.Monad (forM)
import Control.Exception (Exception,throwIO)
import Data.Typeable (Typeable)
import Language.Haskell.Names (Environment(..), Symbol(..))
import Language.Haskell.Exts.Syntax (ModuleName(..))


type Update = [(SliceID, SliceID)]

-- | Given a global list of slices, an update and a slice ID,
-- computes the result of deeply applying the update to the slice
-- with the given slice ID. The result is the local slice ID that
-- corresponds to the given sliceID. Also produces additional local slices in
-- the process.
-- apply :: Map SliceID Slice -> Update -> SliceID -> ([LocalSlice], LocalSliceID)
-- apply slices update sliceID = undefined

-- | Given a global list of slices, and a pair of slice IDs, computes
-- the difference between the two corresponding slices. The result should
-- be minimal.
-- diff :: Map SliceID Slice -> SliceID -> SliceID -> Update
-- diff = undefined

-- Law1

-- forall slices sliceID1 sliceID2 .
-- let update = diff slices sliceID1 sliceID2
-- let (localSlices, localSliceID) = apply slices update sliceID1
-- let (localSliceMap, slices2) = hashLocalSlices localSlices
-- (sliceID2 == localSliceMap !! localSliceID) && (all (`elem` slices) slices2)

-- Law2 only holds up to typical pseudo-inverse normalization stuff

-- forall slices update sliceID .
-- let (localSlices, localSliceID) = apply slices update sliceID
-- let (localSliceMap, slices2) = hashLocalSlices localSlices
-- let sliceID2 = localSliceMap !! localSliceID
-- diff (slices ++ slices2) sliceID sliceID2 == update

type UpdateID = Text

data PersistedUpdate = PersistedUpdate
  { updateID :: UpdateID
  , updateDescription :: Text
  , updateContent :: Update
  }

applyUpdate :: UpdateID -> IO ()
applyUpdate upid = do
  update <- readUpdate upid
  env <- loadEnvironment environmentPath
  let env' = applyUpdatePure (updateContent update) env
  persistEnvironment environmentPath env'

-- | Apply all the replacements contained in the update to the environment.
applyUpdatePure :: Update -> Environment -> Environment
applyUpdatePure upd env = (fmap . fmap) updateSymbol env
  where
    updateSymbol :: Symbol -> Symbol
    updateSymbol s = let sliceID = case (symbolModule s) of (ModuleName _ n) -> Text.pack (tail n) in
          case lookup sliceID upd of
            Nothing -> s
            Just sliceID' -> s { symbolModule = ModuleName () ('F' : Text.unpack sliceID') }


-- Instances for PersistedUpdate

deriving instance Show PersistedUpdate
deriving instance Eq PersistedUpdate
deriving instance Generic PersistedUpdate
instance ToJSON PersistedUpdate
instance FromJSON PersistedUpdate

data UpdateParseError = UpdateParseError FilePath String

deriving instance Typeable UpdateParseError
deriving instance Show UpdateParseError

instance Exception UpdateParseError

-- | Write the update into .fragnix folder.
writeUpdate :: PersistedUpdate -> IO ()
writeUpdate upd = do
  let filepath = updatePath </> (Text.unpack $ updateID upd)
  createDirectoryIfMissing True updatePath
  writeFile filepath (encodePretty upd)


-- | Read the update with given ID from .fragnix folder.
readUpdate :: UpdateID -> IO PersistedUpdate
readUpdate upid = do
  let filepath = updatePath </> Text.unpack upid
  updateFile <- readFile filepath
  either (throwIO . UpdateParseError filepath) return (eitherDecode updateFile)


createUpdate :: Text -> [(SliceID, SliceID)] -> PersistedUpdate
createUpdate desc upd = PersistedUpdate upid desc upd
  where
    upid = (Text.pack . show . abs . hash) (desc, upd)

-- | Return all updates available in the fragnix folder.
getUpdates :: IO [PersistedUpdate]
getUpdates = do
  files <- listFilesRecursive updatePath
  forM files $ \file -> readUpdate (Text.pack file)
