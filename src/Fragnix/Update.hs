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
import Fragnix.Paths (updatePath)
import Fragnix.Utils (listFilesRecursive)

import Data.Aeson (ToJSON, FromJSON, eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Map (Map)
import qualified Data.Map as Map (lookup, insert)
import Data.ByteString.Lazy (writeFile,readFile)
import Data.Text (Text)
import qualified Data.Text as Text (unpack, pack, cons, isPrefixOf, all)
import Data.Hashable (hash)
import System.FilePath ((</>), takeFileName)
import System.Directory (createDirectoryIfMissing)

import Data.Char (isDigit)
import Control.Monad.Trans.State.Strict (State, get, put)
import Control.Monad (forM)
import Data.Maybe (fromMaybe)
import Data.List (find)
import Control.Exception (Exception,throwIO)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)


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
diff slicesMap sliceID1 sliceID2 = do
  case sliceID1 == sliceID2 of
    True -> []
    False -> do
      let slice1 = fromMaybe (error "sliceID not found") (Map.lookup sliceID1 slicesMap)
      let Slice _ language1 fragment1 uses1 instances1 = slice1
      let slice2 = fromMaybe (error "sliceID not found") (Map.lookup sliceID2 slicesMap)
      let Slice _ language2 fragment2 uses2 instances2 = slice2
      case language1 == language2 && fragment1 == fragment2 of
        False -> [(sliceID1, sliceID2)]
        True ->
          case diffUses slicesMap uses1 uses2 of
            Nothing -> [(sliceID1, sliceID2)]
            Just usesUpdate -> case diffInstances slicesMap instances1 instances2 of
              Nothing -> [(sliceID1, sliceID2)]
              Just instancesUpdate -> usesUpdate ++ instancesUpdate

diffUses :: Map SliceID Slice -> [Use] -> [Use] -> Maybe Update
diffUses _ [] [] = return []
diffUses _ (_ : _) [] = Nothing
diffUses _ [] (_ : _) = Nothing
diffUses slicesMap (use1 : uses1) (use2 : uses2) = do
  useUpdate <- diffUse slicesMap use1 use2
  usesUpdate <- diffUses slicesMap uses1 uses2
  return (useUpdate ++ usesUpdate)

diffUse :: Map SliceID Slice -> Use -> Use -> Maybe Update
diffUse slicesMap use1 use2 = do
  let Use qualification1 usedName1 reference1 = use1
  let Use qualification2 usedName2 reference2 = use2
  case qualification1 == qualification2 && usedName1 == usedName2 of
    False -> Nothing
    True -> diffReference slicesMap reference1 reference2

diffReference :: Map SliceID Slice -> Reference -> Reference -> Maybe Update
diffReference _ (Builtin moduleName1) (Builtin moduleName2) =
  case moduleName1 == moduleName2 of
    False -> Nothing
    True -> return []
diffReference _ (Builtin _) (OtherSlice _) = Nothing
diffReference _ (OtherSlice _) (Builtin _) = Nothing
diffReference slicesMap (OtherSlice sliceID1) (OtherSlice sliceID2) =
  case sliceID1 == sliceID2 of
    False -> return (diff slicesMap sliceID1 sliceID2)
    True -> return []

diffInstances :: Map SliceID Slice -> [Instance] -> [Instance] -> Maybe Update
diffInstances _ [] [] = return []
diffInstances _ (_ : _) [] = Nothing
diffInstances _ [] (_ : _) = Nothing
diffInstances slicesMap (instance1 : instances1) (instance2 : instances2) = do
  instanceUpdate <- diffInstance slicesMap instance1 instance2
  instancesUpdate <- diffInstances slicesMap instances1 instances2
  return (instanceUpdate ++ instancesUpdate)

diffInstance :: Map SliceID Slice -> Instance -> Instance -> Maybe Update
diffInstance slicesMap (Instance instancePart1 instanceID1) (Instance instancePart2 instanceID2) =
  case instancePart1 == instancePart2 of
    False -> Nothing
    True -> return (diff slicesMap instanceID1 instanceID2)


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
