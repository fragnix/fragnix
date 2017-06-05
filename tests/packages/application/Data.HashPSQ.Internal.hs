{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Data/HashPSQ/Internal.hs" #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Data.HashPSQ.Internal
    ( -- * Type
      Bucket (..)
    , mkBucket
    , HashPSQ (..)

      -- * Query
    , null
    , size
    , member
    , lookup
    , findMin

      -- * Construction
    , empty
    , singleton

      -- * Insertion
    , insert

      -- * Delete/update
    , delete
    , deleteMin
    , alter
    , alterMin

      -- * Lists
    , fromList
    , toList
    , keys

      -- * Views
    , insertView
    , deleteView
    , minView

      -- * Traversal
    , map
    , fold'

      -- * Unsafe operations
    , unsafeLookupIncreasePriority
    , unsafeInsertIncreasePriority
    , unsafeInsertIncreasePriorityView

      -- * Validity check
    , valid
    ) where

import           Control.DeepSeq      (NFData (..))
import           Data.Foldable        (Foldable (foldr))
import           Data.Hashable
import qualified Data.List            as List
import           Data.Maybe           (isJust)
import           Prelude              hiding (foldr, lookup, map, null)
import           Data.Traversable

import qualified Data.IntPSQ.Internal as IntPSQ
import qualified Data.OrdPSQ          as OrdPSQ

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

data Bucket k p v = B !k !v !(OrdPSQ.OrdPSQ k p v)
    deriving (Foldable, Functor, Show, Traversable)

-- | Smart constructor which takes care of placing the minimum element directly
-- in the 'Bucket'.
{-# INLINABLE mkBucket #-}
mkBucket
    :: (Ord k, Ord p)
    => k -> p -> v -> OrdPSQ.OrdPSQ k p v -> (p, Bucket k p v)
mkBucket k p x opsq =
    -- TODO (jaspervdj): We could do an 'unsafeInsertNew' here for all call
    -- sites.
    case toBucket (OrdPSQ.insert k p x opsq) of
        Just bucket -> bucket
        Nothing     -> error $ "mkBucket: internal error"

toBucket :: (Ord k, Ord p) => OrdPSQ.OrdPSQ k p v -> Maybe (p, Bucket k p v)
toBucket opsq = case OrdPSQ.minView opsq of
    Just (k, p, x, opsq') -> Just (p, B k x opsq')
    Nothing               -> Nothing

instance (NFData k, NFData p, NFData v) => NFData (Bucket k p v) where
    rnf (B k v x) = rnf k `seq` rnf v `seq` rnf x

-- | A priority search queue with keys of type @k@ and priorities of type @p@
-- and values of type @v@. It is strict in keys, priorities and values.
newtype HashPSQ k p v = HashPSQ (IntPSQ.IntPSQ p (Bucket k p v))
    deriving (Foldable, Functor, NFData, Show, Traversable)

instance (Eq k, Eq p, Eq v, Hashable k, Ord k, Ord p) =>
            Eq (HashPSQ k p v) where
    x == y = case (minView x, minView y) of
        (Nothing              , Nothing                ) -> True
        (Just (xk, xp, xv, x'), (Just (yk, yp, yv, y'))) ->
            xk == yk && xp == yp && xv == yv && x' == y'
        (Just _               , Nothing                ) -> False
        (Nothing              , Just _                 ) -> False


------------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------------

-- | /O(1)/ True if the queue is empty.
{-# INLINABLE null #-}
null :: HashPSQ k p v -> Bool
null (HashPSQ ipsq) = IntPSQ.null ipsq

-- | /O(n)/ The number of elements stored in the PSQ.
{-# INLINABLE size #-}
size :: HashPSQ k p v -> Int
size (HashPSQ ipsq) = IntPSQ.fold'
    (\_ _ (B _ _ opsq) acc -> 1 + OrdPSQ.size opsq + acc)
    0
    ipsq

-- | /O(min(n,W))/ Check if a key is present in the the queue.
{-# INLINABLE member #-}
member :: (Hashable k, Ord k, Ord p) => k -> HashPSQ k p v -> Bool
member k = isJust . lookup k

-- | /O(min(n,W))/ The priority and value of a given key, or 'Nothing' if the
-- key is not bound.
{-# INLINABLE lookup #-}
lookup :: (Ord k, Hashable k, Ord p) => k -> HashPSQ k p v -> Maybe (p, v)
lookup k (HashPSQ ipsq) = do
    (p0, B k0 v0 os) <- IntPSQ.lookup (hash k) ipsq
    if k0 == k
        then return (p0, v0)
        else OrdPSQ.lookup k os

-- | /O(1)/ The element with the lowest priority.
findMin :: (Hashable k, Ord k, Ord p) => HashPSQ k p v -> Maybe (k, p, v)
findMin (HashPSQ ipsq) = case IntPSQ.findMin ipsq of
    Nothing              -> Nothing
    Just (_, p, B k x _) -> Just (k, p, x)


--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | /O(1)/ The empty queue.
empty :: HashPSQ k p v
empty = HashPSQ IntPSQ.empty

-- | /O(1)/ Build a queue with one element.
singleton :: (Hashable k, Ord k, Ord p) => k -> p -> v -> HashPSQ k p v
singleton k p v = insert k p v empty


--------------------------------------------------------------------------------
-- Insertion
--------------------------------------------------------------------------------

-- | /O(min(n,W))/ Insert a new key, priority and value into the queue. If the key
-- is already present in the queue, the associated priority and value are
-- replaced with the supplied priority and value.
{-# INLINABLE insert #-}
insert
    :: (Ord k, Hashable k, Ord p)
    => k -> p -> v -> HashPSQ k p v -> HashPSQ k p v
insert k p v (HashPSQ ipsq) =
    case IntPSQ.alter (\x -> ((), ins x)) (hash k) ipsq of
        ((), ipsq') -> HashPSQ ipsq'
  where
    ins Nothing                         = Just (p,  B k  v  (OrdPSQ.empty))
    ins (Just (p', B k' v' os))
        | k' == k                       =
            -- Tricky: p might have less priority than an item in 'os'.
            Just (mkBucket k p v os)
        | p' < p || (p == p' && k' < k) =
            Just (p', B k' v' (OrdPSQ.insert k  p  v  os))
        | OrdPSQ.member k os            =
            -- This is a bit tricky: k might already be present in 'os' and we
            -- don't want to end up with duplicate keys.
            Just (p,  B k  v  (OrdPSQ.insert k' p' v' (OrdPSQ.delete k os)))
        | otherwise                     =
            Just (p , B k  v  (OrdPSQ.insert k' p' v' os))


--------------------------------------------------------------------------------
-- Delete/update
--------------------------------------------------------------------------------

-- | /O(min(n,W))/ Delete a key and its priority and value from the queue. When
-- the key is not a member of the queue, the original queue is returned.
{-# INLINE delete #-}
delete
    :: (Hashable k, Ord k, Ord p) => k -> HashPSQ k p v -> HashPSQ k p v
delete k t = case deleteView k t of
    Nothing         -> t
    Just (_, _, t') -> t'

-- | /O(min(n,W))/ Delete the binding with the least priority, and return the
-- rest of the queue stripped of that binding. In case the queue is empty, the
-- empty queue is returned again.
{-# INLINE deleteMin #-}
deleteMin
    :: (Hashable k, Ord k, Ord p) => HashPSQ k p v -> HashPSQ k p v
deleteMin t = case minView t of
    Nothing            -> t
    Just (_, _, _, t') -> t'

-- | /O(min(n,W))/ The expression @alter f k queue@ alters the value @x@ at @k@,
-- or absence thereof. 'alter' can be used to insert, delete, or update a value
-- in a queue. It also allows you to calculate an additional value @b@.
{-# INLINABLE alter #-}
alter :: (Hashable k, Ord k, Ord p)
      => (Maybe (p, v) -> (b, Maybe (p, v)))
      -> k -> HashPSQ k p v -> (b, HashPSQ k p v)
alter f k (HashPSQ ipsq) = case IntPSQ.deleteView h ipsq of
    Nothing -> case f Nothing of
        (b, Nothing)     -> (b, HashPSQ ipsq)
        (b, Just (p, x)) ->
            (b, HashPSQ $ IntPSQ.unsafeInsertNew h p (B k x OrdPSQ.empty) ipsq)
    Just (bp, B bk bx opsq, ipsq')
        | k == bk   -> case f (Just (bp, bx)) of
            (b, Nothing) -> case toBucket opsq of
                Nothing             -> (b, HashPSQ ipsq')
                Just (bp', bucket') ->
                    (b, HashPSQ $ IntPSQ.unsafeInsertNew h bp' bucket' ipsq')
            (b, Just (p, x)) -> case mkBucket k p x opsq of
                (bp', bucket') ->
                    (b, HashPSQ $ IntPSQ.unsafeInsertNew h bp' bucket' ipsq')
        | otherwise -> case OrdPSQ.alter f k opsq of
            (b, opsq') -> case mkBucket bk bp bx opsq' of
                (bp', bucket') ->
                    (b, HashPSQ $ IntPSQ.unsafeInsertNew h bp' bucket' ipsq')
  where
    h = hash k

-- | /O(min(n,W))/ A variant of 'alter' which works on the element with the
-- minimum priority. Unlike 'alter', this variant also allows you to change the
-- key of the element.
{-# INLINABLE alterMin #-}
alterMin
    :: (Hashable k, Ord k, Ord p)
     => (Maybe (k, p, v) -> (b, Maybe (k, p, v)))
     -> HashPSQ k p v
     -> (b, HashPSQ k p v)
alterMin f t0 =
    let (t, mbX) = case minView t0 of
                    Nothing             -> (t0, Nothing)
                    Just (k, p, x, t0') -> (t0', Just (k, p, x))
    in case f mbX of
        (b, mbX') ->
            (b, maybe t (\(k, p, x) -> insert k p x t) mbX')


--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

-- | /O(n*min(n,W))/ Build a queue from a list of (key, priority, value) tuples.
-- If the list contains more than one priority and value for the same key, the
-- last priority and value for the key is retained.
{-# INLINABLE fromList #-}
fromList :: (Hashable k, Ord k, Ord p) => [(k, p, v)] -> HashPSQ k p v
fromList = List.foldl' (\psq (k, p, x) -> insert k p x psq) empty

-- | /O(n)/ Convert a queue to a list of (key, priority, value) tuples. The
-- order of the list is not specified.
{-# INLINABLE toList #-}
toList :: (Hashable k, Ord k, Ord p) => HashPSQ k p v -> [(k, p, v)]
toList (HashPSQ ipsq) =
    [ (k', p', x')
    | (_, p, (B k x opsq)) <- IntPSQ.toList ipsq
    , (k', p', x')         <- (k, p, x) : OrdPSQ.toList opsq
    ]

-- | /O(n)/ Obtain the list of present keys in the queue.
{-# INLINABLE keys #-}
keys :: (Hashable k, Ord k, Ord p) => HashPSQ k p v -> [k]
keys t = [k | (k, _, _) <- toList t]


--------------------------------------------------------------------------------
-- Views
--------------------------------------------------------------------------------

-- | /O(min(n,W))/ Insert a new key, priority and value into the queue. If the key
-- is already present in the queue, then the evicted priority and value can be
-- found the first element of the returned tuple.
{-# INLINABLE insertView #-}
insertView
    :: (Hashable k, Ord k, Ord p)
    => k -> p -> v -> HashPSQ k p v -> (Maybe (p, v), HashPSQ k p v)
insertView k p x t =
    -- TODO (jaspervdj): Can be optimized easily
    case deleteView k t of
        Nothing          -> (Nothing,       insert k p x t)
        Just (p', x', _) -> (Just (p', x'), insert k p x t)

-- | /O(min(n,W))/ Delete a key and its priority and value from the queue. If
-- the key was present, the associated priority and value are returned in
-- addition to the updated queue.
{-# INLINABLE deleteView #-}
deleteView
    :: forall k p v. (Hashable k, Ord k, Ord p)
    => k -> HashPSQ k p v -> Maybe (p, v, HashPSQ k p v)
deleteView k (HashPSQ ipsq) = case IntPSQ.alter f (hash k) ipsq of
    (Nothing,     _    ) -> Nothing
    (Just (p, x), ipsq') -> Just (p, x, HashPSQ ipsq')
  where
    f :: Maybe (p, Bucket k p v) -> (Maybe (p, v), Maybe (p, Bucket k p v))
    f Nothing       = (Nothing, Nothing)
    f (Just (p, B bk bx opsq))
        | k == bk   = case OrdPSQ.minView opsq of
            Nothing                  -> (Just (p, bx), Nothing)
            Just (k', p', x', opsq') -> (Just (p, bx), Just (p', B k' x' opsq'))
        | otherwise = case OrdPSQ.deleteView k opsq of
            Nothing              -> (Nothing,       Nothing)
            Just (p', x', opsq') -> (Just (p', x'), Just (p, B bk bx opsq'))

-- | /O(min(n,W))/ Retrieve the binding with the least priority, and the
-- rest of the queue stripped of that binding.
{-# INLINABLE minView #-}
minView
    :: (Hashable k, Ord k, Ord p)
    => HashPSQ k p v -> Maybe (k, p, v, HashPSQ k p v)
minView (HashPSQ ipsq ) =
    case IntPSQ.alterMin f ipsq of
        (Nothing       , _    ) -> Nothing
        (Just (k, p, x), ipsq') -> Just (k, p, x, HashPSQ ipsq')
  where
    f Nothing                 = (Nothing, Nothing)
    f (Just (h, p, B k x os)) = case OrdPSQ.minView os of
        Nothing                ->
            (Just (k, p, x), Nothing)
        Just (k', p', x', os') ->
            (Just (k, p, x), Just (h, p', B k' x' os'))


--------------------------------------------------------------------------------
-- Traversals
--------------------------------------------------------------------------------

-- | /O(n)/ Modify every value in the queue.
{-# INLINABLE map #-}
map :: (k -> p -> v -> w) -> HashPSQ k p v -> HashPSQ k p w
map f (HashPSQ ipsq) = HashPSQ (IntPSQ.map (\_ p v -> mapBucket p v) ipsq)
  where
    mapBucket p (B k v opsq) = B k (f k p v) (OrdPSQ.map f opsq)

-- | /O(n)/ Strict fold over every key, priority and value in the queue. The order
-- in which the fold is performed is not specified.
{-# INLINABLE fold' #-}
fold' :: (k -> p -> v -> a -> a) -> a -> HashPSQ k p v -> a
fold' f acc0 (HashPSQ ipsq) = IntPSQ.fold' goBucket acc0 ipsq
  where
    goBucket _ p (B k v opsq) acc =
        let !acc1 = f k p v acc
            !acc2 = OrdPSQ.fold' f acc1 opsq
        in acc2


--------------------------------------------------------------------------------
-- Unsafe operations
--------------------------------------------------------------------------------

{-# INLINABLE unsafeLookupIncreasePriority #-}
unsafeLookupIncreasePriority
    :: (Hashable k, Ord k, Ord p)
    => k -> p -> HashPSQ k p v -> (Maybe (p, v), HashPSQ k p v)
unsafeLookupIncreasePriority k p (HashPSQ ipsq) =
    (mbPV, HashPSQ ipsq')
  where
    (!mbPV, !ipsq') = IntPSQ.unsafeLookupIncreasePriority
        (\bp b@(B bk bx opsq) ->
            if k == bk
                then let (bp', b') = mkBucket k p bx opsq
                     in (Just (bp, bx), bp', b')
                -- TODO (jaspervdj): Still a lookup-insert here: 3 traversals?
                else case OrdPSQ.lookup k opsq of
                        Nothing      -> (Nothing,     bp, b)
                        Just (p', x) ->
                            let b' = B bk bx (OrdPSQ.insert k p x opsq)
                            in (Just (p', x), bp, b'))
        (hash k)
        ipsq

{-# INLINABLE unsafeInsertIncreasePriority #-}
unsafeInsertIncreasePriority
    :: (Hashable k, Ord k, Ord p)
    => k -> p -> v -> HashPSQ k p v -> HashPSQ k p v
unsafeInsertIncreasePriority k p x (HashPSQ ipsq) = HashPSQ $
    IntPSQ.unsafeInsertWithIncreasePriority
        (\_ _ bp (B bk bx opsq) ->
            if k == bk
                then mkBucket k p x opsq
                else (bp, B bk bx (OrdPSQ.insert k p x opsq)))
        (hash k)
        p
        (B k x OrdPSQ.empty)
        ipsq

{-# INLINABLE unsafeInsertIncreasePriorityView #-}
unsafeInsertIncreasePriorityView
    :: (Hashable k, Ord k, Ord p)
    => k -> p -> v -> HashPSQ k p v -> (Maybe (p, v), HashPSQ k p v)
unsafeInsertIncreasePriorityView k p x (HashPSQ ipsq) =
    (mbEvicted, HashPSQ ipsq')
  where
    (mbBucket, ipsq') = IntPSQ.unsafeInsertWithIncreasePriorityView
        (\_ _ bp (B bk bx opsq) ->
            if k == bk
                then mkBucket k p x opsq
                else (bp, B bk bx (OrdPSQ.insert k p x opsq)))
        (hash k)
        p
        (B k x OrdPSQ.empty)
        ipsq

    mbEvicted = case mbBucket of
        Nothing         -> Nothing
        Just (bp, B bk bv opsq)
            | k == bk   -> Just (bp, bv)
            | otherwise -> OrdPSQ.lookup k opsq


--------------------------------------------------------------------------------
-- Validity check
--------------------------------------------------------------------------------

-- | /O(n^2)/ Internal function to check if the 'HashPSQ' is valid, i.e. if all
-- invariants hold. This should always be the case.
valid :: (Hashable k, Ord k, Ord p) => HashPSQ k p v -> Bool
valid t@(HashPSQ ipsq) =
    not (hasDuplicateKeys t) &&
    and [validBucket k p bucket | (k, p, bucket) <- IntPSQ.toList ipsq]

hasDuplicateKeys :: (Hashable k, Ord k, Ord p) => HashPSQ k p v -> Bool
hasDuplicateKeys = any (> 1) . List.map length . List.group . List.sort . keys

validBucket :: (Hashable k, Ord k, Ord p) => Int -> p -> Bucket k p v -> Bool
validBucket h p (B k _ opsq) =
    OrdPSQ.valid opsq &&
    -- Check that the first element of the bucket has lower priority than all
    -- the other elements.
    and [(p, k) < (p', k') && hash k' == h | (k', p', _) <- OrdPSQ.toList opsq]
