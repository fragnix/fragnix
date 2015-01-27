{-# LINE 1 "Blaze/ByteString/Builder/Internal/Buffer.hs" #-}













































{-# LANGUAGE CPP, BangPatterns, Rank2Types #-}


-- |
-- Module      : Blaze.ByteString.Builder.Internal.Buffer
-- Copyright   : (c) 2010 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Execution of the 'Put' monad and hence also 'Builder's with respect to
-- buffers.
--
module Blaze.ByteString.Builder.Internal.Buffer (
  -- * Buffers
    Buffer (..)

  -- ** Status information
  , freeSize
  , sliceSize
  , bufferSize

  -- ** Creation and modification
  , allocBuffer
  , reuseBuffer
  , nextSlice
  , updateEndOfSlice
  , execBuildStep

  -- ** Conversion to bytestings
  , unsafeFreezeBuffer
  , unsafeFreezeNonEmptyBuffer

  -- * Buffer allocation strategies
  , BufferAllocStrategy
  , allNewBuffersStrategy
  , reuseBufferStrategy

  -- * Executing puts respect to some monad
  , runPut
  ) where

import Foreign                   (Word8, ForeignPtr, Ptr, plusPtr, minusPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)

import qualified Data.ByteString      as S

import qualified Data.ByteString.Internal as S

import Blaze.ByteString.Builder.Internal.Types
------------------------------------------------------------------------------
-- Buffers
------------------------------------------------------------------------------

-- | A buffer @Buffer fpbuf p0 op ope@ describes a buffer with the underlying
-- byte array @fpbuf..ope@, the currently written slice @p0..op@ and the free
-- space @op..ope@.
data Buffer = Buffer {-# UNPACK #-} !(ForeignPtr Word8) -- underlying pinned array
                     {-# UNPACK #-} !(Ptr Word8)        -- beginning of slice
                     {-# UNPACK #-} !(Ptr Word8)        -- next free byte
                     {-# UNPACK #-} !(Ptr Word8)        -- first byte after buffer

-- | The size of the free space of the buffer.
freeSize :: Buffer -> Int
freeSize (Buffer _ _ op ope) = ope `minusPtr` op

-- | The size of the written slice in the buffer.
sliceSize :: Buffer -> Int
sliceSize (Buffer _ p0 op _) = op `minusPtr` p0

-- | The size of the whole byte array underlying the buffer.
bufferSize :: Buffer -> Int
bufferSize (Buffer fpbuf _ _ ope) =
    ope `minusPtr` unsafeForeignPtrToPtr fpbuf

-- | @allocBuffer size@ allocates a new buffer of size @size@.
{-# INLINE allocBuffer #-}
allocBuffer :: Int -> IO Buffer
allocBuffer size = do
    fpbuf <- S.mallocByteString size
    let !pbuf = unsafeForeignPtrToPtr fpbuf
    return $! Buffer fpbuf pbuf pbuf (pbuf `plusPtr` size)

-- | Resets the beginning of the next slice and the next free byte such that
-- the whole buffer can be filled again.
{-# INLINE reuseBuffer #-}
reuseBuffer :: Buffer -> Buffer
reuseBuffer (Buffer fpbuf _ _ ope) = Buffer fpbuf p0 p0 ope
  where
    p0 = unsafeForeignPtrToPtr fpbuf

-- | Convert the buffer to a bytestring. This operation is unsafe in the sense
-- that created bytestring shares the underlying byte array with the buffer.
-- Hence, depending on the later use of this buffer (e.g., if it gets reset and
-- filled again) referential transparency may be lost.
{-# INLINE unsafeFreezeBuffer #-}
unsafeFreezeBuffer :: Buffer -> S.ByteString
unsafeFreezeBuffer (Buffer fpbuf p0 op _) =
    S.PS fpbuf (p0 `minusPtr` unsafeForeignPtrToPtr fpbuf) (op `minusPtr` p0)

-- | Convert a buffer to a non-empty bytestring. See 'unsafeFreezeBuffer' for
-- the explanation of why this operation may be unsafe.
{-# INLINE unsafeFreezeNonEmptyBuffer #-}
unsafeFreezeNonEmptyBuffer :: Buffer -> Maybe S.ByteString
unsafeFreezeNonEmptyBuffer buf
  | sliceSize buf <= 0 = Nothing
  | otherwise          = Just $ unsafeFreezeBuffer buf

-- | Update the end of slice pointer.
{-# INLINE updateEndOfSlice #-}
updateEndOfSlice :: Buffer    -- Old buffer
                 -> Ptr Word8 -- New end of slice
                 -> Buffer    -- Updated buffer
updateEndOfSlice (Buffer fpbuf p0 _ ope) op' = Buffer fpbuf p0 op' ope

-- | Execute a build step on the given buffer.
{-# INLINE execBuildStep #-}
execBuildStep :: BuildStep a
              -> Buffer
              -> IO (BuildSignal a)
execBuildStep step (Buffer _ _ op ope) = runBuildStep step (BufRange op ope)

-- | Move the beginning of the slice to the next free byte such that the
-- remaining free space of the buffer can be filled further. This operation
-- is safe and can be used to fill the remaining part of the buffer after a
-- direct insertion of a bytestring or a flush.
{-# INLINE nextSlice #-}
nextSlice :: Int -> Buffer -> Maybe Buffer
nextSlice minSize (Buffer fpbuf _ op ope)
  | ope `minusPtr` op <= minSize = Nothing
  | otherwise                    = Just (Buffer fpbuf op op ope)

------------------------------------------------------------------------------
-- Buffer allocation strategies
------------------------------------------------------------------------------

-- | A buffer allocation strategy @(buf0, nextBuf)@ specifies the initial
-- buffer to use and how to compute a new buffer @nextBuf minSize buf@ with at
-- least size @minSize@ from a filled buffer @buf@. The double nesting of the
-- @IO@ monad helps to ensure that the reference to the filled buffer @buf@ is
-- lost as soon as possible, but the new buffer doesn't have to be allocated
-- too early.
type BufferAllocStrategy = (IO Buffer, Int -> Buffer -> IO (IO Buffer))

-- | The simplest buffer allocation strategy: whenever a buffer is requested,
-- allocate a new one that is big enough for the next build step to execute.
--
-- NOTE that this allocation strategy may spill quite some memory upon direct
-- insertion of a bytestring by the builder. Thats no problem for garbage
-- collection, but it may lead to unreasonably high memory consumption in
-- special circumstances.
allNewBuffersStrategy :: Int                 -- Minimal buffer size.
                      -> BufferAllocStrategy
allNewBuffersStrategy bufSize =
    ( allocBuffer bufSize
    , \reqSize _ -> return (allocBuffer (max reqSize bufSize)) )

-- | An unsafe, but possibly more efficient buffer allocation strategy:
-- reuse the buffer, if it is big enough for the next build step to execute.
reuseBufferStrategy :: IO Buffer
                    -> BufferAllocStrategy
reuseBufferStrategy buf0 =
    (buf0, tryReuseBuffer)
  where
    tryReuseBuffer reqSize buf
      | bufferSize buf >= reqSize = return $ return (reuseBuffer buf)
      | otherwise                 = return $ allocBuffer reqSize

------------------------------------------------------------------------------
-- Executing puts on a buffer
------------------------------------------------------------------------------


-- | Execute a put on a buffer.
--
-- TODO: Generalize over buffer allocation strategy.
{-# INLINE runPut #-}
runPut :: Monad m
       => (IO (BuildSignal a) -> m (BuildSignal a)) -- lifting of buildsteps
       -> (Int -> Buffer -> m Buffer) -- output function for a guaranteedly non-empty buffer, the returned buffer will be filled next
       -> (S.ByteString -> m ())    -- output function for guaranteedly non-empty bytestrings, that are inserted directly into the stream
       -> Put a                     -- put to execute
       -> Buffer                    -- initial buffer to be used
       -> m (a, Buffer)             -- result of put and remaining buffer
runPut liftIO outputBuf outputBS (Put put) =
    runStep (put (finalStep))
  where
    finalStep x = buildStep $ \(BufRange op _) -> return $ Done op x

    runStep step buf@(Buffer fpbuf p0 op ope) = do
        let !br = BufRange op ope
        signal <- liftIO $ runBuildStep step br
        case signal of
            Done op' x ->         -- put completed, buffer partially runSteped
                return (x, Buffer fpbuf p0 op' ope)

            BufferFull minSize op' nextStep -> do
                buf' <- outputBuf minSize (Buffer fpbuf p0 op' ope)
                runStep nextStep buf'

            InsertByteString op' bs nextStep
              | S.null bs ->   -- flushing of buffer required
                  outputBuf 1 (Buffer fpbuf p0 op' ope) >>= runStep nextStep
              | p0 == op' -> do -- no bytes written: just insert bytestring
                  outputBS bs
                  runStep nextStep buf
              | otherwise -> do   -- bytes written, insert buffer and bytestring
                  buf' <- outputBuf 1 (Buffer fpbuf p0 op' ope)
                  outputBS bs
                  runStep nextStep buf'
