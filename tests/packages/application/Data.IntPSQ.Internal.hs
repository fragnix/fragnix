{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Data/IntPSQ/Internal.hs" #-}























































{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UnboxedTuples     #-}
module Data.IntPSQ.Internal
    ( -- * Type
      Nat
    , Key
    , Mask
    , IntPSQ (..)

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

      -- * Unsafe manipulation
    , unsafeInsertNew
    , unsafeInsertIncreasePriority
    , unsafeInsertIncreasePriorityView
    , unsafeInsertWithIncreasePriority
    , unsafeInsertWithIncreasePriorityView
    , unsafeLookupIncreasePriority

      -- * Testing
    , valid
    , hasBadNils
    , hasDuplicateKeys
    , hasMinHeapProperty
    , validMask
    ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.DeepSeq     (NFData (rnf))

import           Data.Bits
import           Data.BitUtil
import           Data.Foldable       (Foldable (foldr))
import           Data.List           (foldl')
import           Data.Maybe          (isJust)
import           Data.Word           (Word)

import qualified Data.List           as List

import           Prelude             hiding (filter, foldl, foldr, lookup, map,
                                      null)

import           Data.Traversable

-- TODO (SM): get rid of bang patterns

{-
-- Use macros to define strictness of functions.
-- STRICT_x_OF_y denotes an y-ary function strict in the x-th parameter.
-- We do not use BangPatterns, because they are not in any standard and we
-- want the compilers to be compiled by as many compilers as possible.
-}


------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- A "Nat" is a natural machine word (an unsigned Int)
type Nat = Word

type Key = Int

-- | We store masks as the index of the bit that determines the branching.
type Mask = Int

-- | A priority search queue with @Int@ keys and priorities of type @p@ and
-- values of type @v@. It is strict in keys, priorities and values.
data IntPSQ p v
    = Bin {-# UNPACK #-} !Key !p !v {-# UNPACK #-} !Mask !(IntPSQ p v) !(IntPSQ p v)
    | Tip {-# UNPACK #-} !Key !p !v
    | Nil
    deriving (Foldable, Functor, Show, Traversable)

instance (NFData p, NFData v) => NFData (IntPSQ p v) where
    rnf (Bin _k p v _m l r) = rnf p `seq` rnf v `seq` rnf l `seq` rnf r
    rnf (Tip _k p v)        = rnf p `seq` rnf v
    rnf Nil                 = ()

instance (Ord p, Eq v) => Eq (IntPSQ p v) where
    x == y = case (minView x, minView y) of
        (Nothing              , Nothing                ) -> True
        (Just (xk, xp, xv, x'), (Just (yk, yp, yv, y'))) ->
            xk == yk && xp == yp && xv == yv && x' == y'
        (Just _               , Nothing                ) -> False
        (Nothing              , Just _                 ) -> False


-- bit twiddling
----------------

{-# INLINE natFromInt #-}
natFromInt :: Key -> Nat
natFromInt = fromIntegral

{-# INLINE intFromNat #-}
intFromNat :: Nat -> Key
intFromNat = fromIntegral

{-# INLINE zero #-}
zero :: Key -> Mask -> Bool
zero i m
  = (natFromInt i) .&. (natFromInt m) == 0

{-# INLINE nomatch #-}
nomatch :: Key -> Key -> Mask -> Bool
nomatch k1 k2 m =
    natFromInt k1 .&. m' /= natFromInt k2 .&. m'
  where
    m' = maskW (natFromInt m)

{-# INLINE maskW #-}
maskW :: Nat -> Nat
maskW m = complement (m-1) `xor` m

{-# INLINE branchMask #-}
branchMask :: Key -> Key -> Mask
branchMask k1 k2 =
    intFromNat (highestBitMask (natFromInt k1 `xor` natFromInt k2))


------------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------------

-- | /O(1)/ True if the queue is empty.
null :: IntPSQ p v -> Bool
null Nil = True
null _   = False

-- | /O(n)/ The number of elements stored in the queue.
size :: IntPSQ p v -> Int
size Nil               = 0
size (Tip _ _ _)       = 1
size (Bin _ _ _ _ l r) = 1 + size l + size r
-- TODO (SM): benchmark this against a tail-recursive variant

-- | /O(min(n,W))/ Check if a key is present in the the queue.
member :: Int -> IntPSQ p v -> Bool
member k = isJust . lookup k

-- | /O(min(n,W))/ The priority and value of a given key, or 'Nothing' if the
-- key is not bound.
lookup :: Int -> IntPSQ p v -> Maybe (p, v)
lookup k = go
  where
    go t = case t of
        Nil                -> Nothing

        Tip k' p' x'
          | k == k'        -> Just (p', x')
          | otherwise      -> Nothing

        Bin k' p' x' m l r
          | nomatch k k' m -> Nothing
          | k == k'        -> Just (p', x')
          | zero k m       -> go l
          | otherwise      -> go r

-- | /O(1)/ The element with the lowest priority.
findMin :: Ord p => IntPSQ p v -> Maybe (Int, p, v)
findMin t = case t of
    Nil              -> Nothing
    Tip k p x        -> Just (k, p, x)
    Bin k p x _ _ _  -> Just (k, p, x)


------------------------------------------------------------------------------
--- Construction
------------------------------------------------------------------------------

-- | /O(1)/ The empty queue.
empty :: IntPSQ p v
empty = Nil

-- | /O(1)/ Build a queue with one element.
singleton :: Ord p => Int -> p -> v -> IntPSQ p v
singleton = Tip


------------------------------------------------------------------------------
-- Insertion
------------------------------------------------------------------------------

-- | /O(min(n,W))/ Insert a new key, priority and value into the queue. If the key
-- is already present in the queue, the associated priority and value are
-- replaced with the supplied priority and value.
insert :: Ord p => Int -> p -> v -> IntPSQ p v -> IntPSQ p v
insert k p x t0 = unsafeInsertNew k p x (delete k t0)

-- | Internal function to insert a key that is *not* present in the priority
-- queue.
{-# INLINABLE unsafeInsertNew #-}
unsafeInsertNew :: Ord p => Key -> p -> v -> IntPSQ p v -> IntPSQ p v
unsafeInsertNew k p x = go
  where
    go t = case t of
      Nil       -> Tip k p x

      Tip k' p' x'
        | (p, k) < (p', k') -> link k  p  x  k' t           Nil
        | otherwise         -> link k' p' x' k  (Tip k p x) Nil

      Bin k' p' x' m l r
        | nomatch k k' m ->
            if (p, k) < (p', k')
              then link k  p  x  k' t           Nil
              else link k' p' x' k  (Tip k p x) (merge m l r)

        | otherwise ->
            if (p, k) < (p', k')
              then
                if zero k' m
                  then Bin k  p  x  m (unsafeInsertNew k' p' x' l) r
                  else Bin k  p  x  m l (unsafeInsertNew k' p' x' r)
              else
                if zero k m
                  then Bin k' p' x' m (unsafeInsertNew k  p  x  l) r
                  else Bin k' p' x' m l (unsafeInsertNew k  p  x  r)

-- | Link
link :: Key -> p -> v -> Key -> IntPSQ p v -> IntPSQ p v -> IntPSQ p v
link k p x k' k't otherTree
  | zero m k' = Bin k p x m k't       otherTree
  | otherwise = Bin k p x m otherTree k't
  where
    m = branchMask k k'


------------------------------------------------------------------------------
-- Delete/Alter
------------------------------------------------------------------------------

-- | /O(min(n,W))/ Delete a key and its priority and value from the queue. When
-- the key is not a member of the queue, the original queue is returned.
{-# INLINABLE delete #-}
delete :: Ord p => Int -> IntPSQ p v -> IntPSQ p v
delete k = go
  where
    go t = case t of
        Nil           -> Nil

        Tip k' _ _
          | k == k'   -> Nil
          | otherwise -> t

        Bin k' p' x' m l r
          | nomatch k k' m -> t
          | k == k'        -> merge m l r
          | zero k m       -> binShrinkL k' p' x' m (go l) r
          | otherwise      -> binShrinkR k' p' x' m l      (go r)

-- | /O(min(n,W))/ Delete the binding with the least priority, and return the
-- rest of the queue stripped of that binding. In case the queue is empty, the
-- empty queue is returned again.
{-# INLINE deleteMin #-}
deleteMin :: Ord p => IntPSQ p v -> IntPSQ p v
deleteMin t = case minView t of
    Nothing            -> t
    Just (_, _, _, t') -> t'

-- | /O(min(n,W))/ The expression @alter f k queue@ alters the value @x@ at @k@,
-- or absence thereof. 'alter' can be used to insert, delete, or update a value
-- in a queue. It also allows you to calculate an additional value @b@.
{-# INLINE alter #-}
alter
    :: Ord p
    => (Maybe (p, v) -> (b, Maybe (p, v)))
    -> Int
    -> IntPSQ p v
    -> (b, IntPSQ p v)
alter f = \k t0 ->
    let (t, mbX) = case deleteView k t0 of
                            Nothing          -> (t0, Nothing)
                            Just (p, v, t0') -> (t0', Just (p, v))
    in case f mbX of
          (b, mbX') ->
            (b, maybe t (\(p, v) -> unsafeInsertNew k p v t) mbX')

-- | /O(min(n,W))/ A variant of 'alter' which works on the element with the
-- minimum priority. Unlike 'alter', this variant also allows you to change the
-- key of the element.
{-# INLINE alterMin #-}
alterMin :: Ord p
         => (Maybe (Int, p, v) -> (b, Maybe (Int, p, v)))
         -> IntPSQ p v
         -> (b, IntPSQ p v)
alterMin f t = case t of
    Nil             -> case f Nothing of
                         (b, Nothing)           -> (b, Nil)
                         (b, Just (k', p', x')) -> (b, Tip k' p' x')

    Tip k p x       -> case f (Just (k, p, x)) of
                         (b, Nothing)           -> (b, Nil)
                         (b, Just (k', p', x')) -> (b, Tip k' p' x')

    Bin k p x m l r -> case f (Just (k, p, x)) of
                         (b, Nothing)           -> (b, merge m l r)
                         (b, Just (k', p', x'))
                           | k  /= k'  -> (b, insert k' p' x' (merge m l r))
                           | p' <= p   -> (b, Bin k p' x' m l r)
                           | otherwise -> (b, unsafeInsertNew k p' x' (merge m l r))

-- | Smart constructor for a 'Bin' node whose left subtree could have become
-- 'Nil'.
{-# INLINE binShrinkL #-}
binShrinkL :: Key -> p -> v -> Mask -> IntPSQ p v -> IntPSQ p v -> IntPSQ p v
binShrinkL k p x m Nil r = case r of Nil -> Tip k p x; _ -> Bin k p x m Nil r
binShrinkL k p x m l   r = Bin k p x m l r

-- | Smart constructor for a 'Bin' node whose right subtree could have become
-- 'Nil'.
{-# INLINE binShrinkR #-}
binShrinkR :: Key -> p -> v -> Mask -> IntPSQ p v -> IntPSQ p v -> IntPSQ p v
binShrinkR k p x m l Nil = case l of Nil -> Tip k p x; _ -> Bin k p x m l Nil
binShrinkR k p x m l r   = Bin k p x m l r


------------------------------------------------------------------------------
-- Lists
------------------------------------------------------------------------------

-- | /O(n*min(n,W))/ Build a queue from a list of (key, priority, value) tuples.
-- If the list contains more than one priority and value for the same key, the
-- last priority and value for the key is retained.
{-# INLINABLE fromList #-}
fromList :: Ord p => [(Int, p, v)] -> IntPSQ p v
fromList = foldl' (\im (k, p, x) -> insert k p x im) empty

-- | /O(n)/ Convert a queue to a list of (key, priority, value) tuples. The
-- order of the list is not specified.
toList :: IntPSQ p v -> [(Int, p, v)]
toList =
    go []
  where
    go acc Nil                = acc
    go acc (Tip k' p' x')        = (k', p', x') : acc
    go acc (Bin k' p' x' _m l r) = (k', p', x') : go (go acc r) l

-- | /O(n)/ Obtain the list of present keys in the queue.
keys :: IntPSQ p v -> [Int]
keys t = [k | (k, _, _) <- toList t]
-- TODO (jaspervdj): More efficient implementations possible


------------------------------------------------------------------------------
-- Views
------------------------------------------------------------------------------

-- | /O(min(n,W))/ Insert a new key, priority and value into the queue. If the key
-- is already present in the queue, then the evicted priority and value can be
-- found the first element of the returned tuple.
insertView :: Ord p => Int -> p -> v -> IntPSQ p v -> (Maybe (p, v), IntPSQ p v)
insertView k p x t0 = case deleteView k t0 of
    Nothing          -> (Nothing,       unsafeInsertNew k p x t0)
    Just (p', v', t) -> (Just (p', v'), unsafeInsertNew k p x t)

-- | /O(min(n,W))/ Delete a key and its priority and value from the queue. If
-- the key was present, the associated priority and value are returned in
-- addition to the updated queue.
{-# INLINABLE deleteView #-}
deleteView :: Ord p => Int -> IntPSQ p v -> Maybe (p, v, IntPSQ p v)
deleteView k t0 =
    case delFrom t0 of
      (# _, Nothing     #) -> Nothing
      (# t, Just (p, x) #) -> Just (p, x, t)
  where
    delFrom t = case t of
      Nil -> (# Nil, Nothing #)

      Tip k' p' x'
        | k == k'   -> (# Nil, Just (p', x') #)
        | otherwise -> (# t,   Nothing       #)

      Bin k' p' x' m l r
        | nomatch k k' m -> (# t, Nothing #)
        | k == k'   -> let t' = merge m l r
                       in  t' `seq` (# t', Just (p', x') #)

        | zero k m  -> case delFrom l of
                         (# l', mbPX #) -> let t' = binShrinkL k' p' x' m l' r
                                           in  t' `seq` (# t', mbPX #)

        | otherwise -> case delFrom r of
                         (# r', mbPX #) -> let t' = binShrinkR k' p' x' m l  r'
                                           in  t' `seq` (# t', mbPX #)

-- | /O(min(n,W))/ Retrieve the binding with the least priority, and the
-- rest of the queue stripped of that binding.
{-# INLINE minView #-}
minView :: Ord p => IntPSQ p v -> Maybe (Int, p, v, IntPSQ p v)
minView t = case t of
    Nil             -> Nothing
    Tip k p x       -> Just (k, p, x, Nil)
    Bin k p x m l r -> Just (k, p, x, merge m l r)


------------------------------------------------------------------------------
-- Traversal
------------------------------------------------------------------------------

-- | /O(n)/ Modify every value in the queue.
{-# INLINABLE map #-}
map :: (Int -> p -> v -> w) -> IntPSQ p v -> IntPSQ p w
map f =
    go
  where
    go t = case t of
        Nil             -> Nil
        Tip k p x       -> Tip k p (f k p x)
        Bin k p x m l r -> Bin k p (f k p x) m (go l) (go r)

-- | /O(n)/ Strict fold over every key, priority and value in the queue. The order
-- in which the fold is performed is not specified.
{-# INLINABLE fold' #-}
fold' :: (Int -> p -> v -> a -> a) -> a -> IntPSQ p v -> a
fold' f = go
  where
    go !acc Nil                   = acc
    go !acc (Tip k' p' x')        = f k' p' x' acc
    go !acc (Bin k' p' x' _m l r) =
        let !acc1 = f k' p' x' acc
            !acc2 = go acc1 l
            !acc3 = go acc2 r
        in acc3


-- | Internal function that merges two *disjoint* 'IntPSQ's that share the
-- same prefix mask.
{-# INLINABLE merge #-}
merge :: Ord p => Mask -> IntPSQ p v -> IntPSQ p v -> IntPSQ p v
merge m l r = case l of
    Nil -> r

    Tip lk lp lx ->
      case r of
        Nil                     -> l
        Tip rk rp rx
          | (lp, lk) < (rp, rk) -> Bin lk lp lx m Nil r
          | otherwise           -> Bin rk rp rx m l   Nil
        Bin rk rp rx rm rl rr
          | (lp, lk) < (rp, rk) -> Bin lk lp lx m Nil r
          | otherwise           -> Bin rk rp rx m l   (merge rm rl rr)

    Bin lk lp lx lm ll lr ->
      case r of
        Nil                     -> l
        Tip rk rp rx
          | (lp, lk) < (rp, rk) -> Bin lk lp lx m (merge lm ll lr) r
          | otherwise           -> Bin rk rp rx m l                Nil
        Bin rk rp rx rm rl rr
          | (lp, lk) < (rp, rk) -> Bin lk lp lx m (merge lm ll lr) r
          | otherwise           -> Bin rk rp rx m l                (merge rm rl rr)


------------------------------------------------------------------------------
-- Improved insert performance for special cases
------------------------------------------------------------------------------

-- TODO (SM): Make benchmarks run again, integrate this function with insert
-- and test how benchmarks times change.

-- | Internal function to insert a key with priority larger than the
-- maximal priority in the heap. This is always the case when using the PSQ
-- as the basis to implement a LRU cache, which associates a
-- access-tick-number with every element.
{-# INLINE unsafeInsertIncreasePriority #-}
unsafeInsertIncreasePriority
    :: Ord p => Key -> p -> v -> IntPSQ p v -> IntPSQ p v
unsafeInsertIncreasePriority =
    unsafeInsertWithIncreasePriority (\newP newX _ _ -> (newP, newX))

{-# INLINE unsafeInsertIncreasePriorityView #-}
unsafeInsertIncreasePriorityView
    :: Ord p => Key -> p -> v -> IntPSQ p v -> (Maybe (p, v), IntPSQ p v)
unsafeInsertIncreasePriorityView =
    unsafeInsertWithIncreasePriorityView (\newP newX _ _ -> (newP, newX))

-- | This name is not chosen well anymore. This function:
--
-- - Either inserts a value at a new key with a prio higher than the max of the
--   entire PSQ.
-- - Or, overrides the value at the key with a prio strictly higher than the
--   original prio at that key (but not necessarily the max of the entire PSQ).
{-# INLINABLE unsafeInsertWithIncreasePriority #-}
unsafeInsertWithIncreasePriority
    :: Ord p
    => (p -> v -> p -> v -> (p, v))
    -> Key -> p -> v -> IntPSQ p v -> IntPSQ p v
unsafeInsertWithIncreasePriority f k p x t0 =
    -- TODO (jaspervdj): Maybe help inliner a bit here, check core.
    go t0
  where
    go t = case t of
        Nil -> Tip k p x

        Tip k' p' x'
            | k == k'   -> case f p x p' x' of (!fp, !fx) -> Tip k fp fx
            | otherwise -> link k' p' x' k  (Tip k p x) Nil

        Bin k' p' x' m l r
            | nomatch k k' m -> link k' p' x' k (Tip k p x) (merge m l r)
            | k == k'        -> case f p x p' x' of
                (!fp, !fx)
                    | zero k m  -> merge m (unsafeInsertNew k fp fx l) r
                    | otherwise -> merge m l (unsafeInsertNew k fp fx r)
            | zero k m       -> Bin k' p' x' m (go l) r
            | otherwise      -> Bin k' p' x' m l      (go r)

{-# INLINABLE unsafeInsertWithIncreasePriorityView #-}
unsafeInsertWithIncreasePriorityView
    :: Ord p
    => (p -> v -> p -> v -> (p, v))
    -> Key -> p -> v -> IntPSQ p v -> (Maybe (p, v), IntPSQ p v)
unsafeInsertWithIncreasePriorityView f k p x t0 =
    -- TODO (jaspervdj): Maybe help inliner a bit here, check core.
    case go t0 of
        (# t, mbPX #) -> (mbPX, t)
  where
    go t = case t of
        Nil -> (# Tip k p x, Nothing #)

        Tip k' p' x'
            | k == k'   -> case f p x p' x' of
                (!fp, !fx) -> (# Tip k fp fx, Just (p', x') #)
            | otherwise -> (# link k' p' x' k  (Tip k p x) Nil, Nothing #)

        Bin k' p' x' m l r
            | nomatch k k' m ->
                let t' = merge m l r
                in t' `seq`
                    let t'' = link k' p' x' k (Tip k p x) t'
                    in t'' `seq` (# t'', Nothing #)

            | k == k' -> case f p x p' x' of
                (!fp, !fx)
                    | zero k m  ->
                        let t' = merge m (unsafeInsertNew k fp fx l) r
                        in t' `seq` (# t', Just (p', x') #)
                    | otherwise ->
                        let t' = merge m l (unsafeInsertNew k fp fx r)
                        in t' `seq` (# t', Just (p', x') #)

            | zero k m -> case go l of
                (# l', mbPX #) -> l' `seq` (# Bin k' p' x' m l' r, mbPX #)

            | otherwise -> case go r of
                (# r', mbPX #) -> r' `seq` (# Bin k' p' x' m l r', mbPX #)

-- | This can NOT be used to delete elements.
{-# INLINABLE unsafeLookupIncreasePriority #-}
unsafeLookupIncreasePriority
    :: Ord p
    => (p -> v -> (Maybe b, p, v))
    -> Key
    -> IntPSQ p v
    -> (Maybe b, IntPSQ p v)
unsafeLookupIncreasePriority f k t0 =
    -- TODO (jaspervdj): Maybe help inliner a bit here, check core.
    case go t0 of
        (# t, mbB #) -> (mbB, t)
  where
    go t = case t of
        Nil -> (# Nil, Nothing #)

        Tip k' p' x'
            | k == k'   -> case f p' x' of
                (!fb, !fp, !fx) -> (# Tip k fp fx, fb #)
            | otherwise -> (# t, Nothing #)

        Bin k' p' x' m l r
            | nomatch k k' m -> (# t, Nothing #)

            | k == k' -> case f p' x' of
                (!fb, !fp, !fx)
                    | zero k m  ->
                        let t' = merge m (unsafeInsertNew k fp fx l) r
                        in t' `seq` (# t', fb #)
                    | otherwise ->
                        let t' = merge m l (unsafeInsertNew k fp fx r)
                        in t' `seq` (# t', fb #)

            | zero k m -> case go l of
                (# l', mbB #) -> l' `seq` (# Bin k' p' x' m l' r, mbB #)

            | otherwise -> case go r of
                (# r', mbB #) -> r' `seq` (# Bin k' p' x' m l r', mbB #)


------------------------------------------------------------------------------
-- Validity checks for the datastructure invariants
------------------------------------------------------------------------------

-- | /O(n^2)/ Internal function to check if the 'IntPSQ' is valid, i.e. if all
-- invariants hold. This should always be the case.
valid :: Ord p => IntPSQ p v -> Bool
valid psq =
    not (hasBadNils psq) &&
    not (hasDuplicateKeys psq) &&
    hasMinHeapProperty psq &&
    validMask psq

hasBadNils :: IntPSQ p v -> Bool
hasBadNils psq = case psq of
    Nil                 -> False
    Tip _ _ _           -> False
    Bin _ _ _ _ Nil Nil -> True
    Bin _ _ _ _ l r     -> hasBadNils l || hasBadNils r

hasDuplicateKeys :: IntPSQ p v -> Bool
hasDuplicateKeys psq =
    any ((> 1) . length) (List.group . List.sort $ collectKeys [] psq)
  where
    collectKeys :: [Int] -> IntPSQ p v -> [Int]
    collectKeys ks Nil = ks
    collectKeys ks (Tip k _ _) = k : ks
    collectKeys ks (Bin k _ _ _ l r) =
        let ks' = collectKeys (k : ks) l
        in collectKeys ks' r

hasMinHeapProperty :: Ord p => IntPSQ p v -> Bool
hasMinHeapProperty psq = case psq of
    Nil             -> True
    Tip _ _ _       -> True
    Bin _ p _ _ l r -> go p l && go p r
  where
    go :: Ord p => p -> IntPSQ p v -> Bool
    go _ Nil = True
    go parentPrio (Tip _ prio _) = parentPrio <= prio
    go parentPrio (Bin _ prio _  _ l r) =
        parentPrio <= prio && go prio l && go prio r

data Side = L | R

validMask :: IntPSQ p v -> Bool
validMask Nil = True
validMask (Tip _ _ _) = True
validMask (Bin _ _ _ m left right ) =
    maskOk m left right && go m L left && go m R right
  where
    go :: Mask -> Side -> IntPSQ p v -> Bool
    go parentMask side psq = case psq of
        Nil -> True
        Tip k _ _ -> checkMaskAndSideMatchKey parentMask side k
        Bin k _ _ mask l r ->
            checkMaskAndSideMatchKey parentMask side k &&
            maskOk mask l r &&
            go mask L l &&
            go mask R r

    checkMaskAndSideMatchKey parentMask side key =
        case side of
            L -> parentMask .&. key == 0
            R -> parentMask .&. key == parentMask

    maskOk :: Mask -> IntPSQ p v -> IntPSQ p v -> Bool
    maskOk mask l r = case xor <$> childKey l <*> childKey r of
        Nothing -> True
        Just xoredKeys ->
            fromIntegral mask == highestBitMask (fromIntegral xoredKeys)

    childKey Nil = Nothing
    childKey (Tip k _ _) = Just k
    childKey (Bin k _ _ _ _ _) = Just k
