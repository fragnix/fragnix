module Fragnix.SliceInstanceResolution
  ( resolveSlices
  ) where

import Fragnix.Slice
  ( Slice(Slice)
  , SliceI(SliceI)
  , SliceID
  , Use(Use)
  , Reference(OtherSlice)
  , InstanceID
  , Instance(Instance)
  , InstancePart(OfThisClass,OfThisClassForUnknownType,ForThisType,ForThisTypeOfUnknownClass)
  )

import Data.Map (Map)
import qualified Data.Map.Strict as Map (
    fromList,toList,(!),keys,map,mapWithKey)
import Data.Set (Set)
import qualified Data.Set as Set (
    fromList,toList,map,filter,delete,
    empty,singleton,union,intersection,unions,difference)

resolveSlices :: [Slice] -> [SliceI]
resolveSlices slices = map annotSlice slices
  where
    annotSlice slice@(Slice sliceID _ _ _ _) = SliceI slice (Set.toList (sliceInstancesMap Map.! sliceID))
    sliceInstancesMap = sliceInstances slices

-- | Given a list of slices, find for each slice the list of relevant
-- instances. Then reduce these lists to only those instances that have
-- to be imported explicitly and put them into a Map for easy lookup.
sliceInstances :: [Slice] -> Map SliceID (Set InstanceID)
sliceInstances slices = sliceInstancesMap where

    -- Map from slice ID to set of all instances that have to be imported explicitly
    sliceInstancesMap = Map.fromList (do
        (sliceID,instanceIDSet) <- Map.toList explicitInstancesMap
        let reducedInstanceIDSet = Set.difference instanceIDSet directlyUsedInstanceIDSet
            directlyUsedInstanceIDSet = Set.unions (do
                directlyUsedSliceID <- Set.toList (directlyUsedSliceMap Map.! sliceID)
                return (explicitInstancesMap Map.! directlyUsedSliceID))
        return (sliceID,reducedInstanceIDSet))

    -- Map from slice ID to set of all required instances. Contains instances that would be
    -- in scope implicitly. The set never includes the slice itself.
    explicitInstancesMap = removeSliceItself (Map.map snd usedSliceMapFixpoint)
    removeSliceItself = Map.mapWithKey (\sliceID -> Set.delete sliceID)

    -- Fixedpoint of transitively adding more and more used slices and instances.
    usedSliceMapFixpoint = fixedpoint (addInstances instancesMap . addUsedSlices directlyUsedSliceMap) initialUsedSlicesAndInstancesMap

    -- Initially a slice uses just itself and no instances
    initialUsedSlicesAndInstancesMap = Map.fromList (do
        Slice sliceID _ _ _ _ <- slices
        return (sliceID, (Set.singleton sliceID, Set.empty)))
    -- Directly used slices.
    directlyUsedSliceMap = Map.fromList (do
        slice@(Slice sliceID _ _ _ _) <- slices
        return (sliceID,Set.fromList (usedSliceIDs slice)))
    -- For each slice its list of instances.
    instancesMap = Map.fromList (do
        Slice sliceID _ _ _ instances <- slices
        return (sliceID,instances))


-- | Iterate the given function untile the argument doesn't change anymore.
fixedpoint :: (Eq a) => (a -> a) -> a -> a
fixedpoint f x
    | x == x' = x
    | otherwise = f x' where
        x' = f x

-- | Given a Map from slice ID to set of directly used slices and a map from
-- SliceID to a pair of a set of used slices and implicitly
-- used instances, does one step of transitivity
-- i.e. adds all slices the used slices or instances directly use.
addUsedSlices :: Map SliceID (Set SliceID) -> Map SliceID (Set SliceID, Set InstanceID) -> Map SliceID (Set SliceID, Set InstanceID)
addUsedSlices directlyUsedSliceMap usedSlicesAndInstancesMap = Map.fromList (do
    sliceID <- Map.keys usedSlicesAndInstancesMap
    let (usedSliceIDSet, usedInstanceIDSet) = usedSlicesAndInstancesMap Map.! sliceID
        usedSliceIDSet' = Set.union usedSliceIDSet (Set.unions (do
            usedSliceID <- Set.toList (Set.union usedSliceIDSet usedInstanceIDSet)
            return (directlyUsedSliceMap Map.! usedSliceID)))
    return (sliceID,(usedSliceIDSet', usedInstanceIDSet)))


-- | Given a Map from slice ID to list of instances of that slice and a Map
-- from slice ID to sets of used slices and instances, adds for each key in
-- the map the relevant instances.
addInstances :: Map SliceID [Instance] -> Map SliceID (Set SliceID, Set InstanceID) -> Map SliceID (Set SliceID, Set InstanceID)
addInstances instancesMap usedSlicesAndInstancesMap = Map.fromList (do
    sliceID <- Map.keys usedSlicesAndInstancesMap
    let (usedSliceIDSet, usedInstanceIDSet) = usedSlicesAndInstancesMap Map.! sliceID
        instances = Set.fromList (do
            usedSliceID <- Set.toList usedSliceIDSet
            instancesMap Map.! usedSliceID)
        usedInstanceIDSet' = Set.union usedInstanceIDSet (relevantInstanceIDs instances)
    return (sliceID,(usedSliceIDSet, usedInstanceIDSet')))

-- | As an optimization we prune the set of instances that might be relevant for
-- a slice. Given a set of instances possibly used by a slice, finds those that are
-- actually relevant. The given set of instances contains information on why
-- we think the instance might be relevant.
-- An instance is relevant for a slice if one of the
-- following four conditions holds:
--     * The slice transitively uses the instance's class and type
--     * The slice transitively uses the instance's class and the type is unknown
--     * The slice transitively uses the instance's type and the class is unknown
--     * The slices that a relevant instance uses make one of the previous
--       conditions true.
-- The last condition requires us to do a fixed point iteration.
relevantInstanceIDs :: Set Instance -> Set InstanceID
relevantInstanceIDs instances = Set.unions [
    Set.intersection (instancesPlayingPart OfThisClass instances) (instancesPlayingPart ForThisType instances),
    instancesPlayingPart OfThisClassForUnknownType instances,
    instancesPlayingPart ForThisTypeOfUnknownClass instances]


-- | Given a set of instances find the set of instance IDs of all instances playing
-- the given part.
instancesPlayingPart :: InstancePart -> Set Instance -> Set InstanceID
instancesPlayingPart desiredInstancePart =
    Set.map getInstanceID . Set.filter playsDesiredInstancePart where
        playsDesiredInstancePart (Instance instancePart _) = instancePart == desiredInstancePart
        getInstanceID (Instance _ instanceID) = instanceID

usedSliceIDs :: Slice -> [SliceID]
usedSliceIDs (Slice _ _ _ uses _) = do
    Use _ _ (OtherSlice sliceID) <- uses
    return sliceID
