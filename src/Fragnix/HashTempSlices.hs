module Fragnix.HashTempSlices where

import Fragnix.Slice (
    Slice(Slice),SliceID,
    Use(Use),Reference(OtherSlice),
    Instance(Instance))

import Language.Haskell.Names (Symbol)

import Data.Foldable (for_)
import Data.Traversable (for)
import Control.Monad.Trans.State (State,execState,get,put)
import Data.Text (pack)
import Data.Map (Map)
import qualified Data.Map as Map (
    lookup,fromList,(!),map,keys,
    empty,insert)
import Data.Hashable (hash)
import qualified Data.Text as Text




-- | A temporary ID before slices can be hashed.
-- TempIDs are always smaller than zero.
type TempID = SliceID

-- | A Slice with a temporary ID that may use slices with
-- temporary IDs.
type TempSlice = Slice

-- | A slice ID after the first round of hashing.
type Hash1ID = SliceID


hashTempSlices :: [TempSlice] -> Map Symbol TempID -> ([Slice], Map Symbol SliceID)
hashTempSlices tempSlices symbolTempIDs = let

    tempSliceMap = sliceMap tempSlices

    sliceIDMap = hashSlices2 tempSliceMap

    slices = map (replaceSliceID (\tempID -> sliceIDMap Map.! tempID)) tempSlices

    symbolSliceIDs = Map.map (\tempID -> sliceIDMap Map.! tempID) symbolTempIDs

    in (slices, symbolSliceIDs)


-- | Build up a map from temporary ID to corresponding slice for better lookup.
sliceMap :: [TempSlice] -> Map TempID TempSlice
sliceMap tempSliceList = Map.fromList (do
    tempSlice@(Slice tempSliceID _ _ _ _) <- tempSliceList
    return (tempSliceID,tempSlice))


-- | Associate every temporary ID with its slice ID. This slice ID is
-- a hash of the slice's content. If we include instances in this content
-- we get cycles. To avoid these cycles we hash slices in two steps:
-- First without taking instances into account and then with taking instances
-- into account.
-- To prevent cycles in the second step we lookup the instance hash from
-- the first hashing step instead of recursing.
hashSlices2 :: Map TempID TempSlice -> Map TempID SliceID
hashSlices2 tempSliceMap = execState (
    for_ tempIDs (hashSlice2 tempSliceMap hash1IDMap)) Map.empty where
        tempIDs = Map.keys tempSliceMap
        hash1IDMap = hashSlices1 tempSliceMap


-- | Compute the second hash for the temporary slice with the given temporary ID.
hashSlice2 ::
    Map TempID TempSlice ->
    Map TempID Hash1ID ->
    TempID ->
    State (Map TempID SliceID) SliceID
hashSlice2 tempSliceMap hash1IDMap tempID = do
    sliceIDMap <- get
    case Map.lookup tempID sliceIDMap of
        Just sliceID -> return sliceID
        Nothing -> do
            let Just (Slice _ language fragment tempUses tempInstances) =
                    Map.lookup tempID tempSliceMap
            uses <- for tempUses (hashUse2 tempSliceMap hash1IDMap)
            let instances = map (
                    replaceInstanceID (\instanceTempID -> hash1IDMap Map.! instanceTempID))
                    tempInstances
                sliceID = pack (show (abs (fromIntegral (hash (language,fragment,uses,instances)) :: Integer)))
            put (Map.insert tempID sliceID sliceIDMap)
            return sliceID


-- | If the given Use references a temporary ID replace that temporary ID by
-- the final slice ID.
hashUse2 ::
    Map TempID TempSlice ->
    Map TempID Hash1ID ->
    Use ->
    State (Map TempID SliceID) Use
hashUse2 tempSliceMap hash1IDMap use = case use of
    Use qualification usedName (OtherSlice tempID) -> do
        if Text.head(tempID) == '-'
            then do
                sliceID <- hashSlice2 tempSliceMap hash1IDMap tempID
                return (Use qualification usedName (OtherSlice sliceID))
            else do
                return use
    _ -> return use


-- | Given a list from temporary ID to temporary Slice computes a Map
-- from temporary ID to the first hash. This first hash does not include
-- type class instances.
hashSlices1 :: Map TempID TempSlice -> Map TempID Hash1ID
hashSlices1 tempSliceMap = execState (
    for_ tempIDs (hashSlice1 tempSliceMap)) Map.empty where
        tempIDs = Map.keys tempSliceMap

-- | Compute the first hash for the given temporary ID using a cache.
hashSlice1 :: Map TempID TempSlice -> TempID -> State (Map TempID Hash1ID) Hash1ID
hashSlice1 tempSliceMap tempID = do
    hash1IDMap <- get
    case Map.lookup tempID hash1IDMap of
        Just hash1ID -> return hash1ID
        Nothing -> do
            let Just (Slice _ language fragment tempUses _) =
                    Map.lookup tempID tempSliceMap
            uses <- for tempUses (hashUse1 tempSliceMap)
            let hash1ID = pack (show (abs (fromIntegral (hash (language,fragment,uses)) :: Integer)))
            put (Map.insert tempID hash1ID hash1IDMap)
            return hash1ID

-- | If the given use references a temporary slice via a temporary ID
-- then replace that reference by the first hash of that temporary slice.
hashUse1 :: Map TempID TempSlice -> Use -> State (Map TempID Hash1ID) Use
hashUse1 tempSliceMap use = case use of
    Use qualification usedName (OtherSlice tempID) -> do
        if Text.head(tempID) == '-'
            then do
                hash1ID <- hashSlice1 tempSliceMap tempID
                return (Use qualification usedName (OtherSlice hash1ID))
            else do
                return use
    _ -> return use

-- | Replace every occurence of a temporary ID in the given slice with the final ID.
replaceSliceID :: (TempID -> SliceID) -> TempSlice -> Slice
replaceSliceID f (Slice tempID language fragment uses instances) =
    Slice (f tempID) language fragment (map (replaceUseID f) uses) (map (replaceInstanceID f) instances)

-- | Replace every occurence of a temporary ID in the given use with the final ID.
-- A temporary as opposed to a slice ID is smaller than zero.
replaceUseID :: (TempID -> SliceID) -> Use -> Use
replaceUseID f (Use qualification usedName (OtherSlice tempID))
    | Text.head(tempID) == '-' = (Use qualification usedName (OtherSlice (f tempID)))
replaceUseID _ use = use

-- | Replace every occurence of a temporary ID in the given Instance with the final
-- ID. A temporary ID is smaller than zero.
replaceInstanceID :: (TempID -> SliceID) -> Instance -> Instance
replaceInstanceID f (Instance instancePart instanceID)
    | Text.head(instanceID) == '-' = Instance instancePart (f instanceID)
replaceInstanceID _ instanc = instanc


