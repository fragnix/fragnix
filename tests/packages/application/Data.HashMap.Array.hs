{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Data/HashMap/Array.hs" #-}




















































{-# LANGUAGE BangPatterns, CPP, MagicHash, Rank2Types, UnboxedTuples #-}
{-# OPTIONS_GHC -fno-full-laziness -funbox-strict-fields #-}

-- | Zero based arrays.
--
-- Note that no bounds checking are performed.
module Data.HashMap.Array
    ( Array
    , MArray

      -- * Creation
    , new
    , new_
    , singleton
    , singletonM
    , pair

      -- * Basic interface
    , length
    , lengthM
    , read
    , write
    , index
    , indexM
    , update
    , updateWith'
    , unsafeUpdateM
    , insert
    , insertM
    , delete

    , unsafeFreeze
    , unsafeThaw
    , run
    , run2
    , copy
    , copyM

      -- * Folds
    , foldl'
    , foldr

    , thaw
    , map
    , map'
    , traverse
    , filter
    , toList
    ) where

import qualified Data.Traversable as Traversable
import Control.DeepSeq
-- GHC 7.7 exports toList/fromList from GHC.Exts
-- In order to avoid warnings on previous GHC versions, we provide
-- an explicit import list instead of only hiding the offending symbols
import GHC.Exts (Array#, Int(..), newArray#, readArray#, writeArray#,
                 indexArray#, unsafeFreezeArray#, unsafeThawArray#,
                 MutableArray#)
import GHC.ST (ST(..))

import Prelude hiding (filter, foldr, length, map, read, traverse)

import GHC.Exts (sizeofArray#, copyArray#, thawArray#, sizeofMutableArray#,
                 copyMutableArray#)


import Data.HashMap.Unsafe (runST)

------------------------------------------------------------------------


data Array a = Array {
      unArray :: !(Array# a)
    }

instance Show a => Show (Array a) where
    show = show . toList

length :: Array a -> Int
length ary = I# (sizeofArray# (unArray ary))
{-# INLINE length #-}

-- | Smart constructor
array :: Array# a -> Int -> Array a
array ary _n = Array ary
{-# INLINE array #-}

data MArray s a = MArray {
      unMArray :: !(MutableArray# s a)
    }

lengthM :: MArray s a -> Int
lengthM mary = I# (sizeofMutableArray# (unMArray mary))
{-# INLINE lengthM #-}

-- | Smart constructor
marray :: MutableArray# s a -> Int -> MArray s a
marray mary _n = MArray mary
{-# INLINE marray #-}

------------------------------------------------------------------------

instance NFData a => NFData (Array a) where
    rnf = rnfArray

rnfArray :: NFData a => Array a -> ()
rnfArray ary0 = go ary0 n0 0
  where
    n0 = length ary0
    go !ary !n !i
        | i >= n = ()
        | otherwise = rnf (index ary i) `seq` go ary n (i+1)
{-# INLINE rnfArray #-}

-- | Create a new mutable array of specified size, in the specified
-- state thread, with each element containing the specified initial
-- value.
new :: Int -> a -> ST s (MArray s a)
new n@(I# n#) b =
    
    ST $ \s ->
        case newArray# n# b s of
            (# s', ary #) -> (# s', marray ary n #)
{-# INLINE new #-}

new_ :: Int -> ST s (MArray s a)
new_ n = new n undefinedElem

singleton :: a -> Array a
singleton x = runST (singletonM x)
{-# INLINE singleton #-}

singletonM :: a -> ST s (Array a)
singletonM x = new 1 x >>= unsafeFreeze
{-# INLINE singletonM #-}

pair :: a -> a -> Array a
pair x y = run $ do
    ary <- new 2 x
    write ary 1 y
    return ary
{-# INLINE pair #-}

read :: MArray s a -> Int -> ST s a
read ary _i@(I# i#) = ST $ \ s ->
    
        readArray# (unMArray ary) i# s
{-# INLINE read #-}

write :: MArray s a -> Int -> a -> ST s ()
write ary _i@(I# i#) b = ST $ \ s ->
    
        case writeArray# (unMArray ary) i# b s of
            s' -> (# s' , () #)
{-# INLINE write #-}

index :: Array a -> Int -> a
index ary _i@(I# i#) =
    
        case indexArray# (unArray ary) i# of (# b #) -> b
{-# INLINE index #-}

indexM :: Array a -> Int -> ST s a
indexM ary _i@(I# i#) =
    
        case indexArray# (unArray ary) i# of (# b #) -> return b
{-# INLINE indexM #-}

unsafeFreeze :: MArray s a -> ST s (Array a)
unsafeFreeze mary
    = ST $ \s -> case unsafeFreezeArray# (unMArray mary) s of
                   (# s', ary #) -> (# s', array ary (lengthM mary) #)
{-# INLINE unsafeFreeze #-}

unsafeThaw :: Array a -> ST s (MArray s a)
unsafeThaw ary
    = ST $ \s -> case unsafeThawArray# (unArray ary) s of
                   (# s', mary #) -> (# s', marray mary (length ary) #)
{-# INLINE unsafeThaw #-}

run :: (forall s . ST s (MArray s e)) -> Array e
run act = runST $ act >>= unsafeFreeze
{-# INLINE run #-}

run2 :: (forall s. ST s (MArray s e, a)) -> (Array e, a)
run2 k = runST (do
                 (marr,b) <- k
                 arr <- unsafeFreeze marr
                 return (arr,b))

-- | Unsafely copy the elements of an array. Array bounds are not checked.
copy :: Array e -> Int -> MArray s e -> Int -> Int -> ST s ()
copy !src !_sidx@(I# sidx#) !dst !_didx@(I# didx#) _n@(I# n#) =
    
    
        ST $ \ s# ->
        case copyArray# (unArray src) sidx# (unMArray dst) didx# n# s# of
            s2 -> (# s2, () #)

-- | Unsafely copy the elements of an array. Array bounds are not checked.
copyM :: MArray s e -> Int -> MArray s e -> Int -> Int -> ST s ()
copyM !src !_sidx@(I# sidx#) !dst !_didx@(I# didx#) _n@(I# n#) =
    
    
    ST $ \ s# ->
    case copyMutableArray# (unMArray src) sidx# (unMArray dst) didx# n# s# of
        s2 -> (# s2, () #)

-- | /O(n)/ Insert an element at the given position in this array,
-- increasing its size by one.
insert :: Array e -> Int -> e -> Array e
insert ary idx b = runST (insertM ary idx b)
{-# INLINE insert #-}

-- | /O(n)/ Insert an element at the given position in this array,
-- increasing its size by one.
insertM :: Array e -> Int -> e -> ST s (Array e)
insertM ary idx b =
    
        do mary <- new_ (count+1)
           copy ary 0 mary 0 idx
           write mary idx b
           copy ary idx mary (idx+1) (count-idx)
           unsafeFreeze mary
  where !count = length ary
{-# INLINE insertM #-}

-- | /O(n)/ Update the element at the given position in this array.
update :: Array e -> Int -> e -> Array e
update ary idx b = runST (updateM ary idx b)
{-# INLINE update #-}

-- | /O(n)/ Update the element at the given position in this array.
updateM :: Array e -> Int -> e -> ST s (Array e)
updateM ary idx b =
    
        do mary <- thaw ary 0 count
           write mary idx b
           unsafeFreeze mary
  where !count = length ary
{-# INLINE updateM #-}

-- | /O(n)/ Update the element at the given positio in this array, by
-- applying a function to it.  Evaluates the element to WHNF before
-- inserting it into the array.
updateWith' :: Array e -> Int -> (e -> e) -> Array e
updateWith' ary idx f = update ary idx $! f (index ary idx)
{-# INLINE updateWith' #-}

-- | /O(1)/ Update the element at the given position in this array,
-- without copying.
unsafeUpdateM :: Array e -> Int -> e -> ST s ()
unsafeUpdateM ary idx b =
    
        do mary <- unsafeThaw ary
           write mary idx b
           _ <- unsafeFreeze mary
           return ()
{-# INLINE unsafeUpdateM #-}

foldl' :: (b -> a -> b) -> b -> Array a -> b
foldl' f = \ z0 ary0 -> go ary0 (length ary0) 0 z0
  where
    go ary n i !z
        | i >= n    = z
        | otherwise = go ary n (i+1) (f z (index ary i))
{-# INLINE foldl' #-}

foldr :: (a -> b -> b) -> b -> Array a -> b
foldr f = \ z0 ary0 -> go ary0 (length ary0) 0 z0
  where
    go ary n i z
        | i >= n    = z
        | otherwise = f (index ary i) (go ary n (i+1) z)
{-# INLINE foldr #-}

undefinedElem :: a
undefinedElem = error "Data.HashMap.Array: Undefined element"
{-# NOINLINE undefinedElem #-}

thaw :: Array e -> Int -> Int -> ST s (MArray s e)
thaw !ary !_o@(I# o#) !n@(I# n#) =
    
        ST $ \ s -> case thawArray# (unArray ary) o# n# s of
            (# s2, mary# #) -> (# s2, marray mary# n #)
{-# INLINE thaw #-}

-- | /O(n)/ Delete an element at the given position in this array,
-- decreasing its size by one.
delete :: Array e -> Int -> Array e
delete ary idx = runST (deleteM ary idx)
{-# INLINE delete #-}

-- | /O(n)/ Delete an element at the given position in this array,
-- decreasing its size by one.
deleteM :: Array e -> Int -> ST s (Array e)
deleteM ary idx = do
    
        do mary <- new_ (count-1)
           copy ary 0 mary 0 idx
           copy ary (idx+1) mary idx (count-(idx+1))
           unsafeFreeze mary
  where !count = length ary
{-# INLINE deleteM #-}

map :: (a -> b) -> Array a -> Array b
map f = \ ary ->
    let !n = length ary
    in run $ do
        mary <- new_ n
        go ary mary 0 n
  where
    go ary mary i n
        | i >= n    = return mary
        | otherwise = do
             write mary i $ f (index ary i)
             go ary mary (i+1) n
{-# INLINE map #-}

-- | Strict version of 'map'.
map' :: (a -> b) -> Array a -> Array b
map' f = \ ary ->
    let !n = length ary
    in run $ do
        mary <- new_ n
        go ary mary 0 n
  where
    go ary mary i n
        | i >= n    = return mary
        | otherwise = do
             write mary i $! f (index ary i)
             go ary mary (i+1) n
{-# INLINE map' #-}

fromList :: Int -> [a] -> Array a
fromList n xs0 =
    
        run $ do
            mary <- new_ n
            go xs0 mary 0
  where
    go [] !mary !_   = return mary
    go (x:xs) mary i = do write mary i x
                          go xs mary (i+1)

toList :: Array a -> [a]
toList = foldr (:) []

traverse :: Applicative f => (a -> f b) -> Array a -> f (Array b)
traverse f = \ ary -> fromList (length ary) `fmap`
                      Traversable.traverse f (toList ary)
{-# INLINE traverse #-}

filter :: (a -> Bool) -> Array a -> Array a
filter p = \ ary ->
    let !n = length ary
    in run $ do
        mary <- new_ n
        go ary mary 0 0 n
  where
    go ary mary i j n
        | i >= n    = if i == j
                      then return mary
                      else do mary2 <- new_ j
                              copyM mary 0 mary2 0 j
                              return mary2
        | p el      = write mary j el >> go ary mary (i+1) (j+1) n
        | otherwise = go ary mary (i+1) j n
      where el = index ary i
{-# INLINE filter #-}
