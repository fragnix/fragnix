{-# LINE 1 "Blaze/ByteString/Builder/Internal/Types.hs" #-}
# 1 "Blaze/ByteString/Builder/Internal/Types.hs"
# 1 "<command-line>"
# 9 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 9 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1





























































































# 9 "<command-line>" 2
# 1 "Blaze/ByteString/Builder/Internal/Types.hs"
{-# LANGUAGE CPP, BangPatterns, Rank2Types #-}





-- |
-- Module      : Blaze.ByteString.Builder.Internal.Types
-- Copyright   : (c) 2010 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Core types and functions for the 'Builder' monoid and the 'Put' monad based
-- based on the 'blaze-builder' library by Jasper van der Jeugt and Simon
-- Meier.
--
module Blaze.ByteString.Builder.Internal.Types where

import Control.Applicative

import Data.Monoid
import qualified Data.ByteString      as S

import Foreign

------------------------------------------------------------------------------
-- The core: BuildSteps
------------------------------------------------------------------------------

data BufRange = BufRange {-# UNPACK #-} !(Ptr Word8) {-# UNPACK #-} !(Ptr Word8)

data BuildSignal a =
    Done {-# UNPACK #-} !(Ptr Word8) a
  | BufferFull
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !(Ptr Word8)
                     !(BuildStep a)
  | InsertByteString
      {-# UNPACK #-} !(Ptr Word8)
                     !S.ByteString
                     !(BuildStep a)

newtype BuildStep a =
    BuildStep { runBuildStep :: BufRange -> IO (BuildSignal a) }

-- Hiding the implementation of 'BuildStep's

done :: Ptr Word8 -> a -> BuildSignal a
done = Done

bufferFull :: Int -> Ptr Word8 -> (BufRange -> IO (BuildSignal a)) -> BuildSignal a
bufferFull size op step = BufferFull size op (buildStep step)

insertByteString :: Ptr Word8 -> S.ByteString -> (BufRange -> IO (BuildSignal a)) -> BuildSignal a
insertByteString op bs step = InsertByteString op bs (buildStep step)

buildStep :: (BufRange -> IO (BuildSignal a)) -> BuildStep a
buildStep = BuildStep

------------------------------------------------------------------------------
-- The 'Builder' Monoid and the 'Put' Monad
------------------------------------------------------------------------------

newtype Builder = Builder {
    unBuilder :: forall r. BuildStep r -> BuildStep r
  }

instance Monoid Builder where
  mempty = Builder id
  {-# INLINE mempty #-}
  (Builder b1) `mappend` (Builder b2) = Builder $ b1 . b2
  {-# INLINE mappend #-}
  mconcat = foldr mappend mempty
  {-# INLINE mconcat #-}

newtype Put a = Put {
    unPut :: forall r. (a -> BuildStep r) -> BuildStep r
  }

instance Functor Put where
  fmap f (Put put) = Put $ \k -> put (\x -> k (f x))
  {-# INLINE fmap #-}

instance Applicative Put where
  pure x = Put $ \k -> k x
  {-# INLINE pure #-}
  f <*> a = Put $ \k -> unPut f (\f' -> unPut a (\a' -> k (f' a')))
  {-# INLINE (<*>) #-}
  a <* b = Put $ \k -> unPut a (\a' -> unPut b (\_ -> k a'))
  {-# INLINE (<*) #-}
  a *> b = Put $ \k -> unPut a (\_ -> unPut b k)
  {-# INLINE (*>) #-}

instance Monad Put where
  return x = Put $ \k -> k x
  {-# INLINE return #-}
  m >>= f  = Put $ \k -> unPut m (\m' -> unPut (f m') k)
  {-# INLINE (>>=) #-}
  m >>  n  = Put $ \k -> unPut m (\_ -> unPut n k)
  {-# INLINE (>>) #-}


-- Creation from concrete 'BuildStep's
------------------------------------------------------------------------------

putBuildStepCont :: (forall r. (a -> BufRange -> IO (BuildSignal r)) ->
                               (     BufRange -> IO (BuildSignal r))
                    ) -> Put a
putBuildStepCont step = Put step'
  where
    step' k = BuildStep $ step (\x -> runBuildStep (k x))


fromBuildStepCont :: (forall r. (BufRange -> IO (BuildSignal r)) ->
                                (BufRange -> IO (BuildSignal r))
                     ) -> Builder
fromBuildStepCont step = Builder step'
  where
    step' k = BuildStep $ step (runBuildStep k)



-- Conversion between Put and Builder
------------------------------------------------------------------------------

-- | Put the given builder.
putBuilder :: Builder -> Put ()
putBuilder (Builder build) = Put $ \k -> build (k ())


-- | Ignore the value of a put and only exploit its output side effect.
fromPut :: Put a -> Builder
fromPut (Put put) = Builder $ \k -> put (\_ -> k)

-- Lifting IO actions
---------------------

-- | Lift the given IO action.
{-# INLINE putLiftIO #-}
putLiftIO :: IO a -> Put a
putLiftIO io = putBuildStepCont $ \k br -> io >>= (`k` br)
