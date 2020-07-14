module Fragnix.HashLocalSlices
  ( hashLocalSlices
  ) where

import Fragnix.Slice (
    Slice(Slice),SliceID,
    Language, Fragment, Use(Use),Reference(OtherSlice,Builtin),
    Instance(Instance))
import Fragnix.LocalSlice (
    LocalSlice(LocalSlice),LocalSliceID,
    LocalUse(LocalUse),LocalReference(OtherLocalSlice),
    LocalInstance(LocalInstance,GlobalInstance))
import qualified Fragnix.LocalSlice as LocalSlice (
     LocalReference(Builtin, OtherSlice))

import Data.Foldable (for_)
import Data.Traversable (for)
import Control.Monad.Trans.State (State,execState,get,put)
import Data.Text (pack)
import Data.Map (Map)
import qualified Data.Map as Map (
    lookup,fromList,(!),keys,
    empty,insert)
import qualified Data.Hashable as Hashable (hash)


-- | A slice ID after the first round of hashing.
type Hash1ID = SliceID


-- | Hash the given local slices to give each of them a slice ID. The results
-- are a map from local ID to slice ID and the list of slices.
hashLocalSlices :: [LocalSlice] -> (Map LocalSliceID SliceID, [Slice])
hashLocalSlices localSlices = let

    localSliceMap = sliceMap localSlices

    localSliceIDMap = hashSlices2 localSliceMap

    slices = map (replaceLocalSliceID (localSliceIDMap Map.!)) localSlices

    in (localSliceIDMap, slices)


-- | Build up a map from local ID to corresponding local slice for better lookup.
sliceMap :: [LocalSlice] -> Map LocalSliceID LocalSlice
sliceMap localSlices = Map.fromList (do
    localSlice@(LocalSlice localSliceID _ _ _ _) <- localSlices
    return (localSliceID,localSlice))


-- | Associate every local ID with its slice ID. This slice ID is
-- a hash of the slice's content. If we include instances in this content
-- we get cycles. To avoid these cycles we hash slices in two steps:
-- First without taking instances into account and then with taking instances
-- into account.
-- To prevent cycles in the second step we lookup the instance hash from
-- the first hashing step instead of recursing.
hashSlices2 :: Map LocalSliceID LocalSlice -> Map LocalSliceID SliceID
hashSlices2 localSliceMap = execState (
    for_ localIDs (hashSlice2 localSliceMap hash1IDMap)) Map.empty where
        localIDs = Map.keys localSliceMap
        hash1IDMap = hashSlices1 localSliceMap

-- | Compute the second hash for the temporary slice with the given local ID.
hashSlice2 ::
    Map LocalSliceID LocalSlice ->
    Map LocalSliceID Hash1ID ->
    LocalSliceID ->
    State (Map LocalSliceID SliceID) SliceID
hashSlice2 localSliceMap hash1IDMap localSliceID = do
    localSliceIDMap <- get
    case Map.lookup localSliceID localSliceIDMap of
        Just sliceID -> return sliceID
        Nothing -> do
            let Just (LocalSlice _ language fragment localUses localInstances) =
                    Map.lookup localSliceID localSliceMap
            uses <- for localUses (hashUse2 localSliceMap hash1IDMap)
            let instances = map (replaceLocalInstanceID (hash1IDMap Map.!)) localInstances
            let sliceID = hash2 (language,fragment,uses,instances)
            put (Map.insert localSliceID sliceID localSliceIDMap)
            return sliceID

-- | The second round of hashing takes into account every part of a slice.
hash2 :: (Language, Fragment, [Use], [Instance])-> SliceID
hash2 a = pack (show (abs (fromIntegral (Hashable.hash a) :: Integer)))

-- | If the given Use references a local ID replace it by the final slice ID.
hashUse2 ::
    Map LocalSliceID LocalSlice ->
    Map LocalSliceID Hash1ID ->
    LocalUse ->
    State (Map LocalSliceID SliceID) Use
hashUse2 localSliceMap hash1IDMap localUse = case localUse of
    LocalUse qualification usedName (OtherLocalSlice localSliceID) -> do
        sliceID <- hashSlice2 localSliceMap hash1IDMap localSliceID
        return (Use qualification usedName (OtherSlice sliceID))
    LocalUse qualification usedName (LocalSlice.OtherSlice sliceID) -> do
        return (Use qualification usedName (OtherSlice sliceID))
    LocalUse qualification usedName (LocalSlice.Builtin originalModule) -> do
        return (Use qualification usedName (Builtin originalModule))


-- | Given a map from local ID to local Slice computes a Map
-- from local ID to the first hash. This first hash does not include
-- type class instances to avoid cycles.
hashSlices1 :: Map LocalSliceID LocalSlice -> Map LocalSliceID Hash1ID
hashSlices1 localSliceMap = flip execState Map.empty (
    for (Map.keys localSliceMap)(hash1Slice localSliceMap))

-- | Compute the first hash for the given local ID using a cache.
hash1Slice :: Map LocalSliceID LocalSlice -> LocalSliceID -> State (Map LocalSliceID Hash1ID) Hash1ID
hash1Slice localSliceMap localSliceID = do
    hash1SliceMap <- get
    case Map.lookup localSliceID hash1SliceMap of
        Just hash1ID -> return hash1ID
        Nothing -> do
            let Just (LocalSlice _ language fragment localUses _) =
                    Map.lookup localSliceID localSliceMap
            hash1Uses <- for localUses (hash1Use localSliceMap)
            let hash1ID = hash1 (language,fragment,hash1Uses)
            put (Map.insert localSliceID hash1ID hash1SliceMap)
            return hash1ID

-- | The first round of hashing does not take into account instances to avoid cycles.
hash1 :: (Language, Fragment, [Use])-> Hash1ID
hash1 a = pack (show (abs (fromIntegral (Hashable.hash a) :: Integer)))

-- | If the given use references a local slice via a local ID
-- then replace that reference by the first hash of that local slice.
-- If the local use refers to a global ID, no change is needed.
-- If the local use refers to a builtin, no change is needed.
hash1Use :: Map LocalSliceID LocalSlice -> LocalUse -> State (Map LocalSliceID Hash1ID) Use
hash1Use localSliceMap localUse = case localUse of
    LocalUse qualification usedName (OtherLocalSlice localSliceID) -> do
        hash1ID <- hash1Slice localSliceMap localSliceID
        return (Use qualification usedName (OtherSlice hash1ID))
    LocalUse qualification usedName (LocalSlice.OtherSlice sliceID) ->
        return (Use qualification usedName (OtherSlice sliceID))
    LocalUse qualification usedName (LocalSlice.Builtin originalModule) ->
        return (Use qualification usedName (Builtin originalModule))


-- | Replace every occurence of a local ID in the given slice with its slice ID.
replaceLocalSliceID :: (LocalSliceID -> SliceID) -> LocalSlice -> Slice
replaceLocalSliceID f (LocalSlice localSliceID language fragment localUses localInstances) =
    Slice (f localSliceID) language fragment (map (replaceLocalUseID f) localUses) (map (replaceLocalInstanceID f) localInstances)

-- | Replace every occurence of a local ID in the given use with its slice ID.
replaceLocalUseID :: (LocalSliceID -> SliceID) -> LocalUse -> Use
replaceLocalUseID f (LocalUse qualification usedName (OtherLocalSlice localSliceID))
    = Use qualification usedName (OtherSlice (f localSliceID))
replaceLocalUseID _ (LocalUse qualification usedName (LocalSlice.OtherSlice sliceID))
    = Use qualification usedName (OtherSlice sliceID)
replaceLocalUseID _ (LocalUse qualification usedName (LocalSlice.Builtin originalModule))
    = Use qualification usedName (Builtin originalModule)

-- | Replace every occurence of a local ID in the given Instance with its slice ID.
replaceLocalInstanceID :: (LocalSliceID -> SliceID) -> LocalInstance -> Instance
replaceLocalInstanceID f (LocalInstance instancePart localInstanceID) =
    Instance instancePart (f localInstanceID)
replaceLocalInstanceID _ (GlobalInstance instancePart instanceID) =
    Instance instancePart instanceID


-- | Replace every occurence of a slice ID in the given slice with its local ID.
replaceSliceID :: (SliceID -> LocalSliceID) -> Slice -> LocalSlice
replaceSliceID f (Slice sliceID language fragment uses instances) =
    LocalSlice (f sliceID) language fragment (map (replaceUseID f) uses) (map (replaceInstanceID f) instances)

-- | Replace every occurence of a slice ID in the given use with its local ID.
replaceUseID :: (SliceID -> LocalSliceID) -> Use -> LocalUse
replaceUseID f (Use qualification usedName (OtherSlice sliceID))
    = LocalUse qualification usedName (OtherLocalSlice (f sliceID))
replaceUseID _ (Use qualification usedName (Builtin originalModule))
    = LocalUse qualification usedName (LocalSlice.Builtin originalModule)

-- | Replace every occurence of a local ID in the given Instance with its slice ID.
replaceInstanceID :: (SliceID -> LocalSliceID) -> Instance -> LocalInstance
replaceInstanceID f (Instance instancePart instanceID) =
    LocalInstance instancePart (f instanceID)



