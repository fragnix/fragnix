{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Fragnix.Update where

import Prelude hiding (writeFile,readFile)

import Fragnix.Slice (
  Slice(Slice), SliceID,
  Use(Use), Reference(OtherSlice, Builtin), Instance(Instance))
import Fragnix.LocalSlice (
  LocalSlice(LocalSlice), LocalSliceID(LocalSliceID),
  LocalUse(LocalUse), LocalInstance(LocalInstance, GlobalInstance))
import qualified Fragnix.LocalSlice as Local (
  LocalReference(OtherSlice,Builtin,OtherLocalSlice))
import Fragnix.Environment (loadEnvironment, persistEnvironment)
import Fragnix.Paths (updatePath, environmentPath)
import Fragnix.Utils (listFilesRecursive)

import Data.Aeson (ToJSON, FromJSON, eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (lookup, insert)
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as Text (unpack, pack, cons)
import Data.Hashable (hash)
import GHC.Generics (Generic)
import Data.ByteString.Lazy (writeFile,readFile)
import System.FilePath ((</>), takeFileName)
import System.Directory (createDirectoryIfMissing)
import Control.Monad.Trans.State.Strict (State, get, put)
import Control.Monad (forM)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Control.Exception (Exception,throwIO)
import Data.Typeable (Typeable)
import Language.Haskell.Names (Environment, Symbol(..))
import Language.Haskell.Exts.Syntax (ModuleName(..))


type Update = [(SliceID, SliceID)]

-- TODO optimize the case where nothing changes

-- | Given a global list of slices, an update and a slice ID,
-- computes the result of deeply applying the update to the slice
-- with the given slice ID. The result is the local slice ID that
-- corresponds to the given sliceID. Also produces additional local slices in
-- the process.
apply :: Map SliceID Slice -> Update -> SliceID -> State (Map SliceID LocalSlice) (Either SliceID LocalSliceID)
apply slicesMap update sliceID =
  case applySliceID update sliceID of
    Just newSliceID -> do
      return (Left newSliceID)
    Nothing -> do
      seenIDMap <- get
      case Map.lookup sliceID seenIDMap of
        Just (LocalSlice localSliceID _ _ _ _) -> do
          return (Right localSliceID)
        Nothing -> do
          let slice = fromMaybe (error "sliceID not found") (Map.lookup sliceID slicesMap)
          let Slice _ language fragment uses instances = slice
          let localSliceID = LocalSliceID (Text.cons 'T' sliceID)
          localUses <- mapM (applyUse slicesMap update) uses
          localInstances <- mapM (applyInstance slicesMap update) instances
          let localSlice = LocalSlice localSliceID language fragment localUses localInstances
          put (Map.insert sliceID localSlice seenIDMap)
          return (Right localSliceID)

applyUse :: Map SliceID Slice -> Update -> Use -> State (Map SliceID LocalSlice) LocalUse
applyUse slicesMap update (Use qualification usedName reference) = do
  localReference <- applyReference slicesMap update reference
  return (LocalUse qualification usedName localReference)

applyReference :: Map SliceID Slice -> Update -> Reference -> State (Map SliceID LocalSlice) Local.LocalReference
applyReference slicesMap update (OtherSlice sliceID) = do
  someID <- apply slicesMap update sliceID
  case someID of
    Left newSliceID -> do
      return (Local.OtherSlice newSliceID)
    Right localSliceID -> do
      return (Local.OtherLocalSlice localSliceID)
applyReference _ _ (Builtin originalModule) = do
  return (Local.Builtin originalModule)

applyInstance :: Map SliceID Slice -> Update -> Instance -> State (Map SliceID LocalSlice) LocalInstance
applyInstance slicesMap update (Instance instancePart instanceID) = do
  someID <- apply slicesMap update instanceID
  case someID of
    Left newInstanceID -> do
      return (GlobalInstance instancePart newInstanceID)
    Right localInstanceID -> do
      return (LocalInstance instancePart localInstanceID)

applySliceID :: Update -> SliceID -> Maybe SliceID
applySliceID update sliceID = lookup sliceID update

-- | Given a global list of slices, and a pair of slice IDs, computes
-- the difference between the two corresponding slices. The result should
-- be minimal.
diff :: Map SliceID Slice -> SliceID -> SliceID -> Update
diff = undefined

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

applyUpdate :: PersistedUpdate -> IO ()
applyUpdate update = do
  putStrLn ("Applying update: " <> (Text.unpack $ updateDescription update))
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
  createDirectoryIfMissing True updatePath
  let filepath = updatePath </> Text.unpack upid
  updateFile <- readFile filepath
  either (throwIO . UpdateParseError filepath) return (eitherDecode updateFile)

-- | Try to interpret the argument as a the prefix of a hash, or a description otherwise.
findUpdateFuzzy :: Text -> IO (Maybe PersistedUpdate)
findUpdateFuzzy desc | Text.all isDigit desc = do
                         updates <- getUpdates
                         return $ find (\update -> Text.isPrefixOf desc (updateID update)) updates
                     | otherwise = do
                         updates <- getUpdates
                         return $ find (\update -> Text.isPrefixOf desc (updateDescription update)) updates

createUpdate :: Text -> [(SliceID, SliceID)] -> PersistedUpdate
createUpdate desc upd = PersistedUpdate upid desc upd
  where
    upid = (Text.pack . show . abs . hash) (desc, upd)

-- | Return all updates available in the fragnix folder.
getUpdates :: IO [PersistedUpdate]
getUpdates = do
  createDirectoryIfMissing True updatePath
  files <- fmap takeFileName <$> listFilesRecursive updatePath
  forM files $ \file -> readUpdate (Text.pack file)
