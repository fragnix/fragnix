{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Data/Vector/Generic/Mutable.hs" #-}























































{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleContexts, BangPatterns, TypeFamilies, ScopedTypeVariables #-}
-- |
-- Module      : Data.Vector.Generic.Mutable
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- Generic interface to mutable vectors
--

module Data.Vector.Generic.Mutable (
  -- * Class of mutable vector types
  MVector(..),

  -- * Accessors

  -- ** Length information
  length, null,

  -- ** Extracting subvectors
  slice, init, tail, take, drop, splitAt,
  unsafeSlice, unsafeInit, unsafeTail, unsafeTake, unsafeDrop,

  -- ** Overlapping
  overlaps,

  -- * Construction

  -- ** Initialisation
  new, unsafeNew, replicate, replicateM, clone,

  -- ** Growing
  grow, unsafeGrow,
  growFront, unsafeGrowFront,

  -- ** Restricting memory usage
  clear,

  -- * Accessing individual elements
  read, write, modify, swap, exchange,
  unsafeRead, unsafeWrite, unsafeModify, unsafeSwap, unsafeExchange,

  -- * Modifying vectors
  nextPermutation,

  -- ** Filling and copying
  set, copy, move, unsafeCopy, unsafeMove,

  -- * Internal operations
  mstream, mstreamR,
  unstream, unstreamR, vunstream,
  munstream, munstreamR,
  transform, transformR,
  fill, fillR,
  unsafeAccum, accum, unsafeUpdate, update, reverse,
  unstablePartition, unstablePartitionBundle, partitionBundle
) where

import           Data.Vector.Generic.Mutable.Base
import qualified Data.Vector.Generic.Base as V

import qualified Data.Vector.Fusion.Bundle      as Bundle
import           Data.Vector.Fusion.Bundle      ( Bundle, MBundle, Chunk(..) )
import qualified Data.Vector.Fusion.Bundle.Monadic as MBundle
import           Data.Vector.Fusion.Stream.Monadic ( Stream )
import qualified Data.Vector.Fusion.Stream.Monadic as Stream
import           Data.Vector.Fusion.Bundle.Size
import           Data.Vector.Fusion.Util        ( delay_inline )

import Control.Monad.Primitive ( PrimMonad, PrimState )

import Prelude hiding ( length, null, replicate, reverse, map, read,
                        take, drop, splitAt, init, tail )



import qualified Data.Vector.Internal.Check as Ck




{-
type family Immutable (v :: * -> * -> *) :: * -> *

-- | Class of mutable vectors parametrised with a primitive state token.
--
class MBundle.Pointer u a => MVector v a where
  -- | Length of the mutable vector. This method should not be
  -- called directly, use 'length' instead.
  basicLength       :: v s a -> Int

  -- | Yield a part of the mutable vector without copying it. This method
  -- should not be called directly, use 'unsafeSlice' instead.
  basicUnsafeSlice :: Int  -- ^ starting index
                   -> Int  -- ^ length of the slice
                   -> v s a
                   -> v s a

  -- Check whether two vectors overlap. This method should not be
  -- called directly, use 'overlaps' instead.
  basicOverlaps    :: v s a -> v s a -> Bool

  -- | Create a mutable vector of the given length. This method should not be
  -- called directly, use 'unsafeNew' instead.
  basicUnsafeNew   :: PrimMonad m => Int -> m (v (PrimState m) a)

  -- | Create a mutable vector of the given length and fill it with an
  -- initial value. This method should not be called directly, use
  -- 'replicate' instead.
  basicUnsafeReplicate :: PrimMonad m => Int -> a -> m (v (PrimState m) a)

  -- | Yield the element at the given position. This method should not be
  -- called directly, use 'unsafeRead' instead.
  basicUnsafeRead  :: PrimMonad m => v (PrimState m) a -> Int -> m a

  -- | Replace the element at the given position. This method should not be
  -- called directly, use 'unsafeWrite' instead.
  basicUnsafeWrite :: PrimMonad m => v (PrimState m) a -> Int -> a -> m ()

  -- | Reset all elements of the vector to some undefined value, clearing all
  -- references to external objects. This is usually a noop for unboxed
  -- vectors. This method should not be called directly, use 'clear' instead.
  basicClear       :: PrimMonad m => v (PrimState m) a -> m ()

  -- | Set all elements of the vector to the given value. This method should
  -- not be called directly, use 'set' instead.
  basicSet         :: PrimMonad m => v (PrimState m) a -> a -> m ()

  basicUnsafeCopyPointer :: PrimMonad m => v (PrimState m) a
                                        -> Immutable v a
                                        -> m ()

  -- | Copy a vector. The two vectors may not overlap. This method should not
  -- be called directly, use 'unsafeCopy' instead.
  basicUnsafeCopy  :: PrimMonad m => v (PrimState m) a   -- ^ target
                                  -> v (PrimState m) a   -- ^ source
                                  -> m ()

  -- | Move the contents of a vector. The two vectors may overlap. This method
  -- should not be called directly, use 'unsafeMove' instead.
  basicUnsafeMove  :: PrimMonad m => v (PrimState m) a   -- ^ target
                                  -> v (PrimState m) a   -- ^ source
                                  -> m ()

  -- | Grow a vector by the given number of elements. This method should not be
  -- called directly, use 'unsafeGrow' instead.
  basicUnsafeGrow  :: PrimMonad m => v (PrimState m) a -> Int
                                                       -> m (v (PrimState m) a)

  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeReplicate n x
    = do
        v <- basicUnsafeNew n
        basicSet v x
        return v

  {-# INLINE basicClear #-}
  basicClear _ = return ()

  {-# INLINE basicSet #-}
  basicSet !v x
    | n == 0    = return ()
    | otherwise = do
                    basicUnsafeWrite v 0 x
                    do_set 1
    where
      !n = basicLength v

      do_set i | 2*i < n = do basicUnsafeCopy (basicUnsafeSlice i i v)
                                              (basicUnsafeSlice 0 i v)
                              do_set (2*i)
               | otherwise = basicUnsafeCopy (basicUnsafeSlice i (n-i) v)
                                             (basicUnsafeSlice 0 (n-i) v)

  {-# INLINE basicUnsafeCopyPointer #-}
  basicUnsafeCopyPointer !dst !src = do_copy 0 src
    where
      do_copy !i p | Just (x,q) <- MBundle.pget p = do
                                                      basicUnsafeWrite dst i x
                                                      do_copy (i+1) q
                   | otherwise = return ()

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy !dst !src = do_copy 0
    where
      !n = basicLength src

      do_copy i | i < n = do
                            x <- basicUnsafeRead src i
                            basicUnsafeWrite dst i x
                            do_copy (i+1)
                | otherwise = return ()

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove !dst !src
    | basicOverlaps dst src = do
        srcCopy <- clone src
        basicUnsafeCopy dst srcCopy
    | otherwise = basicUnsafeCopy dst src

  {-# INLINE basicUnsafeGrow #-}
  basicUnsafeGrow v by
    = do
        v' <- basicUnsafeNew (n+by)
        basicUnsafeCopy (basicUnsafeSlice 0 n v') v
        return v'
    where
      n = basicLength v
-}

-- ------------------
-- Internal functions
-- ------------------

unsafeAppend1 :: (PrimMonad m, MVector v a)
        => v (PrimState m) a -> Int -> a -> m (v (PrimState m) a)
{-# INLINE [0] unsafeAppend1 #-}
    -- NOTE: The case distinction has to be on the outside because
    -- GHC creates a join point for the unsafeWrite even when everything
    -- is inlined. This is bad because with the join point, v isn't getting
    -- unboxed.
unsafeAppend1 v i x
  | i < length v = do
                     unsafeWrite v i x
                     return v
  | otherwise    = do
                     v' <- enlarge v
                     ((Ck.checkIndex "Data/Vector/Generic/Mutable.hs" 226) Ck.Internal) "unsafeAppend1" i (length v')
                       $ unsafeWrite v' i x
                     return v'

unsafePrepend1 :: (PrimMonad m, MVector v a)
        => v (PrimState m) a -> Int -> a -> m (v (PrimState m) a, Int)
{-# INLINE [0] unsafePrepend1 #-}
unsafePrepend1 v i x
  | i /= 0    = do
                  let i' = i-1
                  unsafeWrite v i' x
                  return (v, i')
  | otherwise = do
                  (v', j) <- enlargeFront v
                  let i' = j-1
                  ((Ck.checkIndex "Data/Vector/Generic/Mutable.hs" 241) Ck.Internal) "unsafePrepend1" i' (length v')
                    $ unsafeWrite v' i' x
                  return (v', i')

mstream :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Stream m a
{-# INLINE mstream #-}
mstream v = v `seq` n `seq` (Stream.unfoldrM get 0)
  where
    n = length v

    {-# INLINE [0] get #-}
    get i | i < n     = do x <- unsafeRead v i
                           return $ Just (x, i+1)
          | otherwise = return $ Nothing

fill :: (PrimMonad m, MVector v a)
     => v (PrimState m) a -> Stream m a -> m (v (PrimState m) a)
{-# INLINE fill #-}
fill v s = v `seq` do
                     n' <- Stream.foldM put 0 s
                     return $ unsafeSlice 0 n' v
  where
    {-# INLINE [0] put #-}
    put i x = do
                ((Ck.checkIndex "Data/Vector/Generic/Mutable.hs" 265) Ck.Internal) "fill" i (length v)
                  $ unsafeWrite v i x
                return (i+1)

transform
  :: (PrimMonad m, MVector v a)
  => (Stream m a -> Stream m a) -> v (PrimState m) a -> m (v (PrimState m) a)
{-# INLINE [1] transform #-}
transform f v = fill v (f (mstream v))

mstreamR :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Stream m a
{-# INLINE mstreamR #-}
mstreamR v = v `seq` n `seq` (Stream.unfoldrM get n)
  where
    n = length v

    {-# INLINE [0] get #-}
    get i | j >= 0    = do x <- unsafeRead v j
                           return $ Just (x,j)
          | otherwise = return Nothing
      where
        j = i-1

fillR :: (PrimMonad m, MVector v a)
      => v (PrimState m) a -> Stream m a -> m (v (PrimState m) a)
{-# INLINE fillR #-}
fillR v s = v `seq` do
                      i <- Stream.foldM put n s
                      return $ unsafeSlice i (n-i) v
  where
    n = length v

    {-# INLINE [0] put #-}
    put i x = do
                unsafeWrite v j x
                return j
      where
        j = i-1

transformR
  :: (PrimMonad m, MVector v a)
  => (Stream m a -> Stream m a) -> v (PrimState m) a -> m (v (PrimState m) a)
{-# INLINE [1] transformR #-}
transformR f v = fillR v (f (mstreamR v))

-- | Create a new mutable vector and fill it with elements from the 'Bundle'.
-- The vector will grow exponentially if the maximum size of the 'Bundle' is
-- unknown.
unstream :: (PrimMonad m, MVector v a)
         => Bundle u a -> m (v (PrimState m) a)
-- NOTE: replace INLINE [1] by INLINE? (also in unstreamR)
{-# INLINE [1] unstream #-}
unstream s = munstream (Bundle.lift s)

-- | Create a new mutable vector and fill it with elements from the monadic
-- stream. The vector will grow exponentially if the maximum size of the stream
-- is unknown.
munstream :: (PrimMonad m, MVector v a)
          => MBundle m u a -> m (v (PrimState m) a)
{-# INLINE [1] munstream #-}
munstream s = case upperBound (MBundle.size s) of
               Just n  -> munstreamMax     s n
               Nothing -> munstreamUnknown s

-- FIXME: I can't think of how to prevent GHC from floating out
-- unstreamUnknown. That is bad because SpecConstr then generates two
-- specialisations: one for when it is called from unstream (it doesn't know
-- the shape of the vector) and one for when the vector has grown. To see the
-- problem simply compile this:
--
-- fromList = Data.Vector.Unboxed.unstream . Bundle.fromList
--
-- I'm not sure this still applies (19/04/2010)

munstreamMax :: (PrimMonad m, MVector v a)
             => MBundle m u a -> Int -> m (v (PrimState m) a)
{-# INLINE munstreamMax #-}
munstreamMax s n
  = do
      v <- ((Ck.checkLength "Data/Vector/Generic/Mutable.hs" 344) Ck.Internal) "munstreamMax" n
           $ unsafeNew n
      let put i x = do
                       ((Ck.checkIndex "Data/Vector/Generic/Mutable.hs" 347) Ck.Internal) "munstreamMax" i n
                         $ unsafeWrite v i x
                       return (i+1)
      n' <- MBundle.foldM' put 0 s
      return $ ((Ck.checkSlice "Data/Vector/Generic/Mutable.hs" 351) Ck.Internal) "munstreamMax" 0 n' n
             $ unsafeSlice 0 n' v

munstreamUnknown :: (PrimMonad m, MVector v a)
                 => MBundle m u a -> m (v (PrimState m) a)
{-# INLINE munstreamUnknown #-}
munstreamUnknown s
  = do
      v <- unsafeNew 0
      (v', n) <- MBundle.foldM put (v, 0) s
      return $ ((Ck.checkSlice "Data/Vector/Generic/Mutable.hs" 361) Ck.Internal) "munstreamUnknown" 0 n (length v')
             $ unsafeSlice 0 n v'
  where
    {-# INLINE [0] put #-}
    put (v,i) x = do
                    v' <- unsafeAppend1 v i x
                    return (v',i+1)







-- | Create a new mutable vector and fill it with elements from the 'Bundle'.
-- The vector will grow exponentially if the maximum size of the 'Bundle' is
-- unknown.
vunstream :: (PrimMonad m, V.Vector v a)
         => Bundle v a -> m (V.Mutable v (PrimState m) a)
-- NOTE: replace INLINE [1] by INLINE? (also in unstreamR)
{-# INLINE [1] vunstream #-}
vunstream s = vmunstream (Bundle.lift s)

-- | Create a new mutable vector and fill it with elements from the monadic
-- stream. The vector will grow exponentially if the maximum size of the stream
-- is unknown.
vmunstream :: (PrimMonad m, V.Vector v a)
           => MBundle m v a -> m (V.Mutable v (PrimState m) a)
{-# INLINE [1] vmunstream #-}
vmunstream s = case upperBound (MBundle.size s) of
               Just n  -> vmunstreamMax     s n
               Nothing -> vmunstreamUnknown s

-- FIXME: I can't think of how to prevent GHC from floating out
-- unstreamUnknown. That is bad because SpecConstr then generates two
-- specialisations: one for when it is called from unstream (it doesn't know
-- the shape of the vector) and one for when the vector has grown. To see the
-- problem simply compile this:
--
-- fromList = Data.Vector.Unboxed.unstream . Bundle.fromList
--
-- I'm not sure this still applies (19/04/2010)

vmunstreamMax :: (PrimMonad m, V.Vector v a)
              => MBundle m v a -> Int -> m (V.Mutable v (PrimState m) a)
{-# INLINE vmunstreamMax #-}
vmunstreamMax s n
  = do
      v <- ((Ck.checkLength "Data/Vector/Generic/Mutable.hs" 409) Ck.Internal) "munstreamMax" n
           $ unsafeNew n
      let {-# INLINE [0] copyChunk #-}
          copyChunk i (Chunk m f) =
            ((Ck.checkSlice "Data/Vector/Generic/Mutable.hs" 413) Ck.Internal) "munstreamMax.copyChunk" i m (length v) $ do
              f (basicUnsafeSlice i m v)
              return (i+m)

      n' <- Stream.foldlM' copyChunk 0 (MBundle.chunks s)
      return $ ((Ck.checkSlice "Data/Vector/Generic/Mutable.hs" 418) Ck.Internal) "munstreamMax" 0 n' n
             $ unsafeSlice 0 n' v

vmunstreamUnknown :: (PrimMonad m, V.Vector v a)
                 => MBundle m v a -> m (V.Mutable v (PrimState m) a)
{-# INLINE vmunstreamUnknown #-}
vmunstreamUnknown s
  = do
      v <- unsafeNew 0
      (v', n) <- Stream.foldlM copyChunk (v,0) (MBundle.chunks s)
      return $ ((Ck.checkSlice "Data/Vector/Generic/Mutable.hs" 428) Ck.Internal) "munstreamUnknown" 0 n (length v')
             $ unsafeSlice 0 n v'
  where
    {-# INLINE [0] copyChunk #-}
    copyChunk (v,i) (Chunk n f)
      = do
          let j = i+n
          v' <- if basicLength v < j
                  then unsafeGrow v (delay_inline max (enlarge_delta v) (j - basicLength v))
                  else return v
          ((Ck.checkSlice "Data/Vector/Generic/Mutable.hs" 438) Ck.Internal) "munstreamUnknown.copyChunk" i n (length v')
            $ f (basicUnsafeSlice i n v')
          return (v',j)




-- | Create a new mutable vector and fill it with elements from the 'Bundle'
-- from right to left. The vector will grow exponentially if the maximum size
-- of the 'Bundle' is unknown.
unstreamR :: (PrimMonad m, MVector v a)
          => Bundle u a -> m (v (PrimState m) a)
-- NOTE: replace INLINE [1] by INLINE? (also in unstream)
{-# INLINE [1] unstreamR #-}
unstreamR s = munstreamR (Bundle.lift s)

-- | Create a new mutable vector and fill it with elements from the monadic
-- stream from right to left. The vector will grow exponentially if the maximum
-- size of the stream is unknown.
munstreamR :: (PrimMonad m, MVector v a)
           => MBundle m u a -> m (v (PrimState m) a)
{-# INLINE [1] munstreamR #-}
munstreamR s = case upperBound (MBundle.size s) of
               Just n  -> munstreamRMax     s n
               Nothing -> munstreamRUnknown s

munstreamRMax :: (PrimMonad m, MVector v a)
              => MBundle m u a -> Int -> m (v (PrimState m) a)
{-# INLINE munstreamRMax #-}
munstreamRMax s n
  = do
      v <- ((Ck.checkLength "Data/Vector/Generic/Mutable.hs" 469) Ck.Internal) "munstreamRMax" n
           $ unsafeNew n
      let put i x = do
                      let i' = i-1
                      ((Ck.checkIndex "Data/Vector/Generic/Mutable.hs" 473) Ck.Internal) "munstreamRMax" i' n
                        $ unsafeWrite v i' x
                      return i'
      i <- MBundle.foldM' put n s
      return $ ((Ck.checkSlice "Data/Vector/Generic/Mutable.hs" 477) Ck.Internal) "munstreamRMax" i (n-i) n
             $ unsafeSlice i (n-i) v

munstreamRUnknown :: (PrimMonad m, MVector v a)
                  => MBundle m u a -> m (v (PrimState m) a)
{-# INLINE munstreamRUnknown #-}
munstreamRUnknown s
  = do
      v <- unsafeNew 0
      (v', i) <- MBundle.foldM put (v, 0) s
      let n = length v'
      return $ ((Ck.checkSlice "Data/Vector/Generic/Mutable.hs" 488) Ck.Internal) "unstreamRUnknown" i (n-i) n
             $ unsafeSlice i (n-i) v'
  where
    {-# INLINE [0] put #-}
    put (v,i) x = unsafePrepend1 v i x

-- Length
-- ------

-- | Length of the mutable vector.
length :: MVector v a => v s a -> Int
{-# INLINE length #-}
length = basicLength

-- | Check whether the vector is empty
null :: MVector v a => v s a -> Bool
{-# INLINE null #-}
null v = length v == 0

-- Extracting subvectors
-- ---------------------

-- | Yield a part of the mutable vector without copying it.
slice :: MVector v a => Int -> Int -> v s a -> v s a
{-# INLINE slice #-}
slice i n v = ((Ck.checkSlice "Data/Vector/Generic/Mutable.hs" 513) Ck.Bounds) "slice" i n (length v)
            $ unsafeSlice i n v

take :: MVector v a => Int -> v s a -> v s a
{-# INLINE take #-}
take n v = unsafeSlice 0 (min (max n 0) (length v)) v

drop :: MVector v a => Int -> v s a -> v s a
{-# INLINE drop #-}
drop n v = unsafeSlice (min m n') (max 0 (m - n')) v
  where
    n' = max n 0
    m  = length v

{-# INLINE splitAt #-}
splitAt :: MVector v a => Int -> v s a -> (v s a, v s a)
splitAt n v = ( unsafeSlice 0 m v
              , unsafeSlice m (max 0 (len - n')) v
              )
    where
      m   = min n' len
      n'  = max n 0
      len = length v

init :: MVector v a => v s a -> v s a
{-# INLINE init #-}
init v = slice 0 (length v - 1) v

tail :: MVector v a => v s a -> v s a
{-# INLINE tail #-}
tail v = slice 1 (length v - 1) v

-- | Yield a part of the mutable vector without copying it. No bounds checks
-- are performed.
unsafeSlice :: MVector v a => Int  -- ^ starting index
                           -> Int  -- ^ length of the slice
                           -> v s a
                           -> v s a
{-# INLINE unsafeSlice #-}
unsafeSlice i n v = ((Ck.checkSlice "Data/Vector/Generic/Mutable.hs" 552) Ck.Unsafe) "unsafeSlice" i n (length v)
                  $ basicUnsafeSlice i n v

unsafeInit :: MVector v a => v s a -> v s a
{-# INLINE unsafeInit #-}
unsafeInit v = unsafeSlice 0 (length v - 1) v

unsafeTail :: MVector v a => v s a -> v s a
{-# INLINE unsafeTail #-}
unsafeTail v = unsafeSlice 1 (length v - 1) v

unsafeTake :: MVector v a => Int -> v s a -> v s a
{-# INLINE unsafeTake #-}
unsafeTake n v = unsafeSlice 0 n v

unsafeDrop :: MVector v a => Int -> v s a -> v s a
{-# INLINE unsafeDrop #-}
unsafeDrop n v = unsafeSlice n (length v - n) v

-- Overlapping
-- -----------

-- | Check whether two vectors overlap.
overlaps :: MVector v a => v s a -> v s a -> Bool
{-# INLINE overlaps #-}
overlaps = basicOverlaps

-- Initialisation
-- --------------

-- | Create a mutable vector of the given length.
new :: (PrimMonad m, MVector v a) => Int -> m (v (PrimState m) a)
{-# INLINE new #-}
new n = ((Ck.checkLength "Data/Vector/Generic/Mutable.hs" 585) Ck.Bounds) "new" n
      $ unsafeNew n >>= \v -> basicInitialize v >> return v

-- | Create a mutable vector of the given length. The memory is not initialized.
unsafeNew :: (PrimMonad m, MVector v a) => Int -> m (v (PrimState m) a)
{-# INLINE unsafeNew #-}
unsafeNew n = ((Ck.checkLength "Data/Vector/Generic/Mutable.hs" 591) Ck.Unsafe) "unsafeNew" n
            $ basicUnsafeNew n

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with an initial value.
replicate :: (PrimMonad m, MVector v a) => Int -> a -> m (v (PrimState m) a)
{-# INLINE replicate #-}
replicate n x = basicUnsafeReplicate (delay_inline max 0 n) x

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with values produced by repeatedly executing the monadic action.
replicateM :: (PrimMonad m, MVector v a) => Int -> m a -> m (v (PrimState m) a)
{-# INLINE replicateM #-}
replicateM n m = munstream (MBundle.replicateM n m)

-- | Create a copy of a mutable vector.
clone :: (PrimMonad m, MVector v a) => v (PrimState m) a -> m (v (PrimState m) a)
{-# INLINE clone #-}
clone v = do
            v' <- unsafeNew (length v)
            unsafeCopy v' v
            return v'

-- Growing
-- -------

-- | Grow a vector by the given number of elements. The number must be
-- positive.
grow :: (PrimMonad m, MVector v a)
                => v (PrimState m) a -> Int -> m (v (PrimState m) a)
{-# INLINE grow #-}
grow v by = ((Ck.checkLength "Data/Vector/Generic/Mutable.hs" 622) Ck.Bounds) "grow" by
          $ do vnew <- unsafeGrow v by
               basicInitialize $ basicUnsafeSlice (length v) by vnew
               return vnew

growFront :: (PrimMonad m, MVector v a)
                => v (PrimState m) a -> Int -> m (v (PrimState m) a)
{-# INLINE growFront #-}
growFront v by = ((Ck.checkLength "Data/Vector/Generic/Mutable.hs" 630) Ck.Bounds) "growFront" by
               $ do vnew <- unsafeGrowFront v by
                    basicInitialize $ basicUnsafeSlice 0 by vnew
                    return vnew

enlarge_delta :: MVector v a => v s a -> Int
enlarge_delta v = max (length v) 1

-- | Grow a vector logarithmically
enlarge :: (PrimMonad m, MVector v a)
                => v (PrimState m) a -> m (v (PrimState m) a)
{-# INLINE enlarge #-}
enlarge v = do vnew <- unsafeGrow v by
               basicInitialize $ basicUnsafeSlice (length v) by vnew
               return vnew
  where
    by = enlarge_delta v

enlargeFront :: (PrimMonad m, MVector v a)
                => v (PrimState m) a -> m (v (PrimState m) a, Int)
{-# INLINE enlargeFront #-}
enlargeFront v = do
                   v' <- unsafeGrowFront v by
                   basicInitialize $ basicUnsafeSlice 0 by v'
                   return (v', by)
  where
    by = enlarge_delta v

-- | Grow a vector by the given number of elements. The number must be
-- positive but this is not checked.
unsafeGrow :: (PrimMonad m, MVector v a)
                        => v (PrimState m) a -> Int -> m (v (PrimState m) a)
{-# INLINE unsafeGrow #-}
unsafeGrow v n = ((Ck.checkLength "Data/Vector/Generic/Mutable.hs" 663) Ck.Unsafe) "unsafeGrow" n
               $ basicUnsafeGrow v n

unsafeGrowFront :: (PrimMonad m, MVector v a)
                        => v (PrimState m) a -> Int -> m (v (PrimState m) a)
{-# INLINE unsafeGrowFront #-}
unsafeGrowFront v by = ((Ck.checkLength "Data/Vector/Generic/Mutable.hs" 669) Ck.Unsafe) "unsafeGrowFront" by
                     $ do
                         let n = length v
                         v' <- basicUnsafeNew (by+n)
                         basicUnsafeCopy (basicUnsafeSlice by n v') v
                         return v'

-- Restricting memory usage
-- ------------------------

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects. This is usually a noop for unboxed vectors.
clear :: (PrimMonad m, MVector v a) => v (PrimState m) a -> m ()
{-# INLINE clear #-}
clear = basicClear

-- Accessing individual elements
-- -----------------------------

-- | Yield the element at the given position.
read :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> m a
{-# INLINE read #-}
read v i = ((Ck.checkIndex "Data/Vector/Generic/Mutable.hs" 691) Ck.Bounds) "read" i (length v)
         $ unsafeRead v i

-- | Replace the element at the given position.
write :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> a -> m ()
{-# INLINE write #-}
write v i x = ((Ck.checkIndex "Data/Vector/Generic/Mutable.hs" 697) Ck.Bounds) "write" i (length v)
            $ unsafeWrite v i x

-- | Modify the element at the given position.
modify :: (PrimMonad m, MVector v a) => v (PrimState m) a -> (a -> a) -> Int -> m ()
{-# INLINE modify #-}
modify v f i = ((Ck.checkIndex "Data/Vector/Generic/Mutable.hs" 703) Ck.Bounds) "modify" i (length v)
             $ unsafeModify v f i

-- | Swap the elements at the given positions.
swap :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> Int -> m ()
{-# INLINE swap #-}
swap v i j = ((Ck.checkIndex "Data/Vector/Generic/Mutable.hs" 709) Ck.Bounds) "swap" i (length v)
           $ ((Ck.checkIndex "Data/Vector/Generic/Mutable.hs" 710) Ck.Bounds) "swap" j (length v)
           $ unsafeSwap v i j

-- | Replace the element at the give position and return the old element.
exchange :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> a -> m a
{-# INLINE exchange #-}
exchange v i x = ((Ck.checkIndex "Data/Vector/Generic/Mutable.hs" 716) Ck.Bounds) "exchange" i (length v)
               $ unsafeExchange v i x

-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> m a
{-# INLINE unsafeRead #-}
unsafeRead v i = ((Ck.checkIndex "Data/Vector/Generic/Mutable.hs" 722) Ck.Unsafe) "unsafeRead" i (length v)
               $ basicUnsafeRead v i

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: (PrimMonad m, MVector v a)
                                => v (PrimState m) a -> Int -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite v i x = ((Ck.checkIndex "Data/Vector/Generic/Mutable.hs" 729) Ck.Unsafe) "unsafeWrite" i (length v)
                  $ basicUnsafeWrite v i x

-- | Modify the element at the given position. No bounds checks are performed.
unsafeModify :: (PrimMonad m, MVector v a) => v (PrimState m) a -> (a -> a) -> Int -> m ()
{-# INLINE unsafeModify #-}
unsafeModify v f i = ((Ck.checkIndex "Data/Vector/Generic/Mutable.hs" 735) Ck.Unsafe) "unsafeModify" i (length v)
                   $ basicUnsafeRead v i >>= \x ->
                     basicUnsafeWrite v i (f x)

-- | Swap the elements at the given positions. No bounds checks are performed.
unsafeSwap :: (PrimMonad m, MVector v a)
                => v (PrimState m) a -> Int -> Int -> m ()
{-# INLINE unsafeSwap #-}
unsafeSwap v i j = ((Ck.checkIndex "Data/Vector/Generic/Mutable.hs" 743) Ck.Unsafe) "unsafeSwap" i (length v)
                 $ ((Ck.checkIndex "Data/Vector/Generic/Mutable.hs" 744) Ck.Unsafe) "unsafeSwap" j (length v)
                 $ do
                     x <- unsafeRead v i
                     y <- unsafeRead v j
                     unsafeWrite v i y
                     unsafeWrite v j x

-- | Replace the element at the give position and return the old element. No
-- bounds checks are performed.
unsafeExchange :: (PrimMonad m, MVector v a)
                                => v (PrimState m) a -> Int -> a -> m a
{-# INLINE unsafeExchange #-}
unsafeExchange v i x = ((Ck.checkIndex "Data/Vector/Generic/Mutable.hs" 756) Ck.Unsafe) "unsafeExchange" i (length v)
                     $ do
                         y <- unsafeRead v i
                         unsafeWrite v i x
                         return y

-- Filling and copying
-- -------------------

-- | Set all elements of the vector to the given value.
set :: (PrimMonad m, MVector v a) => v (PrimState m) a -> a -> m ()
{-# INLINE set #-}
set = basicSet

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
copy :: (PrimMonad m, MVector v a) => v (PrimState m) a   -- ^ target
                                   -> v (PrimState m) a   -- ^ source
                                   -> m ()
{-# INLINE copy #-}
copy dst src = ((Ck.check "Data/Vector/Generic/Mutable.hs" 776) Ck.Bounds) "copy" "overlapping vectors"
                                          (not (dst `overlaps` src))
             $ ((Ck.check "Data/Vector/Generic/Mutable.hs" 778) Ck.Bounds) "copy" "length mismatch"
                                          (length dst == length src)
             $ unsafeCopy dst src

-- | Move the contents of a vector. The two vectors must have the same
-- length.
--
-- If the vectors do not overlap, then this is equivalent to 'copy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
move :: (PrimMonad m, MVector v a)
                => v (PrimState m) a -> v (PrimState m) a -> m ()
{-# INLINE move #-}
move dst src = ((Ck.check "Data/Vector/Generic/Mutable.hs" 792) Ck.Bounds) "move" "length mismatch"
                                          (length dst == length src)
             $ unsafeMove dst src

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap. This is not checked.
unsafeCopy :: (PrimMonad m, MVector v a) => v (PrimState m) a   -- ^ target
                                         -> v (PrimState m) a   -- ^ source
                                         -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy dst src = ((Ck.check "Data/Vector/Generic/Mutable.hs" 802) Ck.Unsafe) "unsafeCopy" "length mismatch"
                                         (length dst == length src)
                   $ ((Ck.check "Data/Vector/Generic/Mutable.hs" 804) Ck.Unsafe) "unsafeCopy" "overlapping vectors"
                                         (not (dst `overlaps` src))
                   $ (dst `seq` src `seq` basicUnsafeCopy dst src)

-- | Move the contents of a vector. The two vectors must have the same
-- length, but this is not checked.
--
-- If the vectors do not overlap, then this is equivalent to 'unsafeCopy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
unsafeMove :: (PrimMonad m, MVector v a) => v (PrimState m) a   -- ^ target
                                         -> v (PrimState m) a   -- ^ source
                                         -> m ()
{-# INLINE unsafeMove #-}
unsafeMove dst src = ((Ck.check "Data/Vector/Generic/Mutable.hs" 819) Ck.Unsafe) "unsafeMove" "length mismatch"
                                         (length dst == length src)
                   $ (dst `seq` src `seq` basicUnsafeMove dst src)

-- Permutations
-- ------------

accum :: (PrimMonad m, MVector v a)
      => (a -> b -> a) -> v (PrimState m) a -> Bundle u (Int, b) -> m ()
{-# INLINE accum #-}
accum f !v s = Bundle.mapM_ upd s
  where
    {-# INLINE [0] upd #-}
    upd (i,b) = do
                  a <- ((Ck.checkIndex "Data/Vector/Generic/Mutable.hs" 833) Ck.Bounds) "accum" i n
                     $ unsafeRead v i
                  unsafeWrite v i (f a b)

    !n = length v

update :: (PrimMonad m, MVector v a)
                        => v (PrimState m) a -> Bundle u (Int, a) -> m ()
{-# INLINE update #-}
update !v s = Bundle.mapM_ upd s
  where
    {-# INLINE [0] upd #-}
    upd (i,b) = ((Ck.checkIndex "Data/Vector/Generic/Mutable.hs" 845) Ck.Bounds) "update" i n
              $ unsafeWrite v i b

    !n = length v

unsafeAccum :: (PrimMonad m, MVector v a)
            => (a -> b -> a) -> v (PrimState m) a -> Bundle u (Int, b) -> m ()
{-# INLINE unsafeAccum #-}
unsafeAccum f !v s = Bundle.mapM_ upd s
  where
    {-# INLINE [0] upd #-}
    upd (i,b) = do
                  a <- ((Ck.checkIndex "Data/Vector/Generic/Mutable.hs" 857) Ck.Unsafe) "accum" i n
                     $ unsafeRead v i
                  unsafeWrite v i (f a b)

    !n = length v

unsafeUpdate :: (PrimMonad m, MVector v a)
                        => v (PrimState m) a -> Bundle u (Int, a) -> m ()
{-# INLINE unsafeUpdate #-}
unsafeUpdate !v s = Bundle.mapM_ upd s
  where
    {-# INLINE [0] upd #-}
    upd (i,b) = ((Ck.checkIndex "Data/Vector/Generic/Mutable.hs" 869) Ck.Unsafe) "accum" i n
                  $ unsafeWrite v i b

    !n = length v

reverse :: (PrimMonad m, MVector v a) => v (PrimState m) a -> m ()
{-# INLINE reverse #-}
reverse !v = reverse_loop 0 (length v - 1)
  where
    reverse_loop i j | i < j = do
                                 unsafeSwap v i j
                                 reverse_loop (i + 1) (j - 1)
    reverse_loop _ _ = return ()

unstablePartition :: forall m v a. (PrimMonad m, MVector v a)
                  => (a -> Bool) -> v (PrimState m) a -> m Int
{-# INLINE unstablePartition #-}
unstablePartition f !v = from_left 0 (length v)
  where
    -- NOTE: GHC 6.10.4 panics without the signatures on from_left and
    -- from_right
    from_left :: Int -> Int -> m Int
    from_left i j
      | i == j    = return i
      | otherwise = do
                      x <- unsafeRead v i
                      if f x
                        then from_left (i+1) j
                        else from_right i (j-1)

    from_right :: Int -> Int -> m Int
    from_right i j
      | i == j    = return i
      | otherwise = do
                      x <- unsafeRead v j
                      if f x
                        then do
                               y <- unsafeRead v i
                               unsafeWrite v i x
                               unsafeWrite v j y
                               from_left (i+1) j
                        else from_right i (j-1)

unstablePartitionBundle :: (PrimMonad m, MVector v a)
        => (a -> Bool) -> Bundle u a -> m (v (PrimState m) a, v (PrimState m) a)
{-# INLINE unstablePartitionBundle #-}
unstablePartitionBundle f s
  = case upperBound (Bundle.size s) of
      Just n  -> unstablePartitionMax f s n
      Nothing -> partitionUnknown f s

unstablePartitionMax :: (PrimMonad m, MVector v a)
        => (a -> Bool) -> Bundle u a -> Int
        -> m (v (PrimState m) a, v (PrimState m) a)
{-# INLINE unstablePartitionMax #-}
unstablePartitionMax f s n
  = do
      v <- ((Ck.checkLength "Data/Vector/Generic/Mutable.hs" 926) Ck.Internal) "unstablePartitionMax" n
           $ unsafeNew n
      let {-# INLINE [0] put #-}
          put (i, j) x
            | f x       = do
                            unsafeWrite v i x
                            return (i+1, j)
            | otherwise = do
                            unsafeWrite v (j-1) x
                            return (i, j-1)

      (i,j) <- Bundle.foldM' put (0, n) s
      return (unsafeSlice 0 i v, unsafeSlice j (n-j) v)

partitionBundle :: (PrimMonad m, MVector v a)
        => (a -> Bool) -> Bundle u a -> m (v (PrimState m) a, v (PrimState m) a)
{-# INLINE partitionBundle #-}
partitionBundle f s
  = case upperBound (Bundle.size s) of
      Just n  -> partitionMax f s n
      Nothing -> partitionUnknown f s

partitionMax :: (PrimMonad m, MVector v a)
  => (a -> Bool) -> Bundle u a -> Int -> m (v (PrimState m) a, v (PrimState m) a)
{-# INLINE partitionMax #-}
partitionMax f s n
  = do
      v <- ((Ck.checkLength "Data/Vector/Generic/Mutable.hs" 953) Ck.Internal) "unstablePartitionMax" n
         $ unsafeNew n

      let {-# INLINE [0] put #-}
          put (i,j) x
            | f x       = do
                            unsafeWrite v i x
                            return (i+1,j)

            | otherwise = let j' = j-1 in
                          do
                            unsafeWrite v j' x
                            return (i,j')

      (i,j) <- Bundle.foldM' put (0,n) s
      ((Ck.check "Data/Vector/Generic/Mutable.hs" 968) Ck.Internal) "partitionMax" "invalid indices" (i <= j)
        $ return ()
      let l = unsafeSlice 0 i v
          r = unsafeSlice j (n-j) v
      reverse r
      return (l,r)

partitionUnknown :: (PrimMonad m, MVector v a)
        => (a -> Bool) -> Bundle u a -> m (v (PrimState m) a, v (PrimState m) a)
{-# INLINE partitionUnknown #-}
partitionUnknown f s
  = do
      v1 <- unsafeNew 0
      v2 <- unsafeNew 0
      (v1', n1, v2', n2) <- Bundle.foldM' put (v1, 0, v2, 0) s
      ((Ck.checkSlice "Data/Vector/Generic/Mutable.hs" 983) Ck.Internal) "partitionUnknown" 0 n1 (length v1')
        $ ((Ck.checkSlice "Data/Vector/Generic/Mutable.hs" 984) Ck.Internal) "partitionUnknown" 0 n2 (length v2')
        $ return (unsafeSlice 0 n1 v1', unsafeSlice 0 n2 v2')
  where
    -- NOTE: The case distinction has to be on the outside because
    -- GHC creates a join point for the unsafeWrite even when everything
    -- is inlined. This is bad because with the join point, v isn't getting
    -- unboxed.
    {-# INLINE [0] put #-}
    put (v1, i1, v2, i2) x
      | f x       = do
                      v1' <- unsafeAppend1 v1 i1 x
                      return (v1', i1+1, v2, i2)
      | otherwise = do
                      v2' <- unsafeAppend1 v2 i2 x
                      return (v1, i1, v2', i2+1)

{-
http://en.wikipedia.org/wiki/Permutation#Algorithms_to_generate_permutations

The following algorithm generates the next permutation lexicographically after
a given permutation. It changes the given permutation in-place.

1. Find the largest index k such that a[k] < a[k + 1]. If no such index exists,
   the permutation is the last permutation.
2. Find the largest index l greater than k such that a[k] < a[l].
3. Swap the value of a[k] with that of a[l].
4. Reverse the sequence from a[k + 1] up to and including the final element a[n]
-}

-- | Compute the next (lexicographically) permutation of given vector in-place.
--   Returns False when input is the last permtuation
nextPermutation :: (PrimMonad m,Ord e,MVector v e) => v (PrimState m) e -> m Bool
nextPermutation v
    | dim < 2 = return False
    | otherwise = do
        val <- unsafeRead v 0
        (k,l) <- loop val (-1) 0 val 1
        if k < 0
         then return False
         else unsafeSwap v k l >>
              reverse (unsafeSlice (k+1) (dim-k-1) v) >>
              return True
    where loop !kval !k !l !prev !i
              | i == dim = return (k,l)
              | otherwise  = do
                  cur <- unsafeRead v i
                  -- TODO: make tuple unboxed
                  let (kval',k') = if prev < cur then (prev,i-1) else (kval,k)
                      l' = if kval' < cur then i else l
                  loop kval' k' l' cur (i+1)
          dim = length v
