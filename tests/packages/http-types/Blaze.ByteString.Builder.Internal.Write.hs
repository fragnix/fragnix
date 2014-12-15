{-# LINE 1 "./Blaze/ByteString/Builder/Internal/Write.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                                 






                          






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Blaze/ByteString/Builder/Internal/Write.hs" #-}
{-# LINE 1 "./Blaze/ByteString/Builder/Internal/Write.hs" #-}
{-# LANGUAGE CPP, BangPatterns #-}




-- |
-- Module      : Blaze.ByteString.Builder.Internal.Poke
-- Copyright   : (c) 2010 Simon Meier
--               (c) 2010 Jasper van der Jeugt
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- A general and efficient write type that allows for the easy construction of
-- builders for (smallish) bounded size writes to a buffer.
--
-- FIXME: Improve documentation.
--
module Blaze.ByteString.Builder.Internal.Write (
  -- * Poking a buffer
    Poke
  , runPoke
  , pokeN

  -- * Writing to abuffer
  , Write
  , runWrite
  , getBound
  , getBound'
  , getPoke

  , exactWrite
  , boundedWrite
  , writeLiftIO
  , writeIf
  , writeEq
  , writeOrdering
  , writeOrd

  -- * Constructing builders from writes
  , fromWrite
  , fromWriteSingleton
  , fromWriteList

  -- * Writing 'Storable's
  , writeStorable
  , fromStorable
  , fromStorables

  ) where

import Foreign

import Data.Monoid

import Control.Monad

import Blaze.ByteString.Builder.Internal.Types


------------------------------------------------------------------------------
-- Poking a buffer and writing to a buffer
------------------------------------------------------------------------------

-- Sadly GHC is not smart enough: code where we branch and each branch should
-- execute a few IO actions and then return a value cannot be taught to GHC. At
-- least not such that it returns the value of the branches unpacked.
--
-- Hmm.. at least he behaves much better for the Monoid instance of Write
-- than the one for Poke. Serializing UTF-8 chars gets a slowdown of a
-- factor 2 when 2 chars are composed. Perhaps I should try out the writeList
-- instances also, as they may be more sensitive to to much work per Char.
--

-- | Changing a sequence of bytes starting from the given pointer. 'Poke's are
-- the most primitive buffer manipulation. In most cases, you don't use the
-- explicitely but as part of a 'Write', which also tells how many bytes will
-- be changed at most.
newtype Poke =
    Poke { runPoke :: Ptr Word8 -> IO (Ptr Word8) }

-- | A write of a bounded number of bytes.
--
-- When defining a function @write :: a -> Write@ for some @a@, then it is
-- important to ensure that the bound on the number of bytes written is
-- data-independent. Formally,
--
--  @ forall x y. getBound (write x) = getBound (write y) @
--
-- The idea is that this data-independent bound is specified such that the
-- compiler can optimize the check, if there are enough free bytes in the buffer,
-- to a single subtraction between the pointer to the next free byte and the
-- pointer to the end of the buffer with this constant bound of the maximal
-- number of bytes to be written.
--
data Write = Write {-# UNPACK #-} !Int Poke

-- | Extract the 'Poke' action of a write.
{-# INLINE getPoke #-}
getPoke :: Write -> Poke
getPoke (Write _ wio) = wio

-- | Run the 'Poke' action of a write.
{-# INLINE runWrite #-}
runWrite :: Write -> Ptr Word8 -> IO (Ptr Word8)
runWrite = runPoke . getPoke

-- | Extract the maximal number of bytes that this write could write.
{-# INLINE getBound #-}
getBound :: Write -> Int
getBound (Write bound _) = bound

-- | Extract the maximal number of bytes that this write could write in any
-- case. Assumes that the bound of the write is data-independent.
{-# INLINE getBound' #-}
getBound' :: String             -- ^ Name of caller: for debugging purposes.
          -> (a -> Write)
          -> Int
getBound' msg write =
    getBound $ write $ error $
    "getBound' called from " ++ msg ++ ": write bound is not data-independent."

instance Monoid Poke where
  {-# INLINE mempty #-}
  mempty = Poke $ return

  {-# INLINE mappend #-}
  (Poke po1) `mappend` (Poke po2) = Poke $ po1 >=> po2

  {-# INLINE mconcat #-}
  mconcat = foldr mappend mempty

instance Monoid Write where
  {-# INLINE mempty #-}
  mempty = Write 0 mempty

  {-# INLINE mappend #-}
  (Write bound1 w1) `mappend` (Write bound2 w2) =
    Write (bound1 + bound2) (w1 `mappend` w2)

  {-# INLINE mconcat #-}
  mconcat = foldr mappend mempty


-- | @pokeN size io@ creates a write that denotes the writing of @size@ bytes
-- to a buffer using the IO action @io@. Note that @io@ MUST write EXACTLY @size@
-- bytes to the buffer!
{-# INLINE pokeN #-}
pokeN :: Int
       -> (Ptr Word8 -> IO ()) -> Poke
pokeN size io = Poke $ \op -> io op >> return (op `plusPtr` size)


-- | @exactWrite size io@ creates a bounded write that can later be converted to
-- a builder that writes exactly @size@ bytes. Note that @io@ MUST write
-- EXACTLY @size@ bytes to the buffer!
{-# INLINE exactWrite #-}
exactWrite :: Int
           -> (Ptr Word8 -> IO ())
           -> Write
exactWrite size io = Write size (pokeN size io)

-- | @boundedWrite size write@ creates a bounded write from a @write@ that does
-- not write more than @size@ bytes.
{-# INLINE boundedWrite #-}
boundedWrite :: Int -> Poke -> Write
boundedWrite = Write

-- | @writeLiftIO io write@ creates a write executes the @io@ action to compute
-- the value that is then written.
{-# INLINE writeLiftIO #-}
writeLiftIO :: (a -> Write) -> IO a -> Write
writeLiftIO write io =
    Write (getBound' "writeLiftIO" write)
          (Poke $ \pf -> do x <- io; runWrite (write x) pf)

-- | @writeIf p wTrue wFalse x@ creates a 'Write' with a 'Poke' equal to @wTrue
-- x@, if @p x@ and equal to @wFalse x@ otherwise. The bound of this new
-- 'Write' is the maximum of the bounds for either 'Write'. This yields a data
-- independent bound, if the bound for @wTrue@ and @wFalse@ is already data
-- independent.
{-# INLINE writeIf #-}
writeIf :: (a -> Bool) -> (a -> Write) -> (a -> Write) -> (a -> Write)
writeIf p wTrue wFalse x =
    boundedWrite (max (getBound $ wTrue x) (getBound $ wFalse x))
                 (if p x then getPoke $ wTrue x else getPoke $ wFalse x)

-- | Compare the value to a test value and use the first write action for the
-- equal case and the second write action for the non-equal case.
{-# INLINE writeEq #-}
writeEq :: Eq a => a -> (a -> Write) -> (a -> Write) -> (a -> Write)
writeEq test = writeIf (test ==)

-- | TODO: Test this. It might well be too difficult to use.
--   FIXME: Better name required!
{-# INLINE writeOrdering #-}
writeOrdering :: (a -> Ordering)
              -> (a -> Write) -> (a -> Write) -> (a -> Write)
              -> (a -> Write)
writeOrdering ord wLT wEQ wGT x =
    boundedWrite bound (case ord x of LT -> getPoke $ wLT x;
                                      EQ -> getPoke $ wEQ x;
                                      GT -> getPoke $ wGT x)
  where
    bound = max (getBound $ wLT x) (max (getBound $ wEQ x) (getBound $ wGT x))

-- | A write combinator useful to build decision trees for deciding what value
-- to write with a constant bound on the maximal number of bytes written.
{-# INLINE writeOrd #-}
writeOrd :: Ord a
       => a
       -> (a -> Write) -> (a -> Write) -> (a -> Write)
       -> (a -> Write)
writeOrd test = writeOrdering (`compare` test)

-- | Create a builder that execute a single 'Write'.
{-# INLINE fromWrite #-}
fromWrite :: Write -> Builder
fromWrite (Write maxSize wio) =
    fromBuildStepCont step
  where
    step k (BufRange op ope)
      | op `plusPtr` maxSize <= ope = do
          op' <- runPoke wio op
          let !br' = BufRange op' ope
          k br'
      | otherwise = return $ bufferFull maxSize op (step k)

{-# INLINE fromWriteSingleton #-}
fromWriteSingleton :: (a -> Write) -> (a -> Builder)
fromWriteSingleton write =
    mkBuilder
  where
    mkBuilder x = fromBuildStepCont step
      where
        step k (BufRange op ope)
          | op `plusPtr` maxSize <= ope = do
              op' <- runPoke wio op
              let !br' = BufRange op' ope
              k br'
          | otherwise = return $ bufferFull maxSize op (step k)
          where
            Write maxSize wio = write x

-- | Construct a 'Builder' writing a list of data one element at a time.
fromWriteList :: (a -> Write) -> [a] -> Builder
fromWriteList write =
    makeBuilder
  where
    makeBuilder xs0 = fromBuildStepCont $ step xs0
      where
        step xs1 k !(BufRange op0 ope0) = go xs1 op0
          where
            go [] !op = do
               let !br' = BufRange op ope0
               k br'

            go xs@(x':xs') !op
              | op `plusPtr` maxSize <= ope0 = do
                  !op' <- runPoke wio op
                  go xs' op'
              | otherwise = return $ bufferFull maxSize op (step xs k)
              where
                Write maxSize wio = write x'
{-# INLINE fromWriteList #-}



------------------------------------------------------------------------------
-- Writing storables
------------------------------------------------------------------------------


-- | Write a storable value.
{-# INLINE writeStorable #-}
writeStorable :: Storable a => a -> Write
writeStorable x = exactWrite (sizeOf x) (\op -> poke (castPtr op) x)

-- | A builder that serializes a storable value. No alignment is done.
{-# INLINE fromStorable #-}
fromStorable :: Storable a => a -> Builder
fromStorable = fromWriteSingleton writeStorable

-- | A builder that serializes a list of storable values by writing them
-- consecutively. No alignment is done. Parsing information needs to be
-- provided externally.
fromStorables :: Storable a => [a] -> Builder
fromStorables = fromWriteList writeStorable
