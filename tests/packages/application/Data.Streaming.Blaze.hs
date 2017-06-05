{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Data/Streaming/Blaze.hs" #-}





















































































{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
-- | Convert a stream of blaze-builder @Builder@s into a stream of @ByteString@s.
--
-- Adapted from blaze-builder-enumerator, written by myself and Simon Meier.
--
-- Note: if you have blaze-builder >= 0.4, 'newBlazeRecv' just calls
-- 'Data.Streaming.ByteString.Builder.newByteStringBuilderRecv'

-- Note that the functions here can work in any monad built on top of @IO@ or
-- @ST@.
module Data.Streaming.Blaze
    ( BlazeRecv
    , BlazePopper
    , BlazeFinish
    , newBlazeRecv

  -- * Buffers
  , Buffer

  -- ** Status information
  , freeSize
  , sliceSize
  , bufferSize

  -- ** Creation and modification
  , allocBuffer
  , reuseBuffer
  , nextSlice

  -- ** Conversion to bytestings
  , unsafeFreezeBuffer
  , unsafeFreezeNonEmptyBuffer

  -- * Buffer allocation strategies
  , BufferAllocStrategy
  , allNewBuffersStrategy
  , reuseBufferStrategy
  , defaultStrategy
    ) where

import Blaze.ByteString.Builder
import qualified Data.ByteString as S


import Data.Streaming.ByteString.Builder

newBlazeRecv :: BufferAllocStrategy -> IO (BlazeRecv, BlazeFinish)
newBlazeRecv = newByteStringBuilderRecv
{-# INLINE newBlazeRecv #-}


-- | Provides a series of @ByteString@s until empty, at which point it provides
-- an empty @ByteString@.
--
-- Since 0.1.2
type BlazePopper = IO S.ByteString

type BlazeRecv = Builder -> IO BlazePopper

type BlazeFinish = IO (Maybe S.ByteString)
