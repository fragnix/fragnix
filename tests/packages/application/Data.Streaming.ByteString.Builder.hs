{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Data/Streaming/ByteString/Builder.hs" #-}
-- | Provides @toByteStringIO*@ like "Blaze.ByteString.Builder"s, for "Data.ByteString.Builder".
--
-- Since 0.1.9
module Data.Streaming.ByteString.Builder
    ( toByteStringIO
    , toByteStringIOWith
    , toByteStringIOWithBuffer )
    where

import Control.Monad (when)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder.Extra (runBuilder, BufferWriter, Next(Done, More, Chunk))
import Data.ByteString.Internal (mallocByteString, ByteString(PS))
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)

-- | Use a pre-existing buffer to 'toByteStringIOWith'.
--
-- Since 0.1.9
toByteStringIOWithBuffer :: Int
                         -> (ByteString -> IO ())
                         -> Builder
                         -> ForeignPtr Word8
                         -> IO ()
toByteStringIOWithBuffer initBufSize io b initBuf = do
    go initBufSize initBuf (runBuilder b)
  where
    go bufSize buf = loop
      where
        loop :: BufferWriter -> IO ()
        loop wr = do
            (len, next) <- withForeignPtr buf (flip wr bufSize)
            when (len > 0) (io (PS buf 0 len))
            case next of
                Done -> return ()
                More newBufSize nextWr
                    | newBufSize > bufSize -> do
                        newBuf <- mallocByteString newBufSize
                        go newBufSize newBuf nextWr
                    | otherwise -> loop nextWr
                Chunk s nextWr -> do
                    io s
                    loop nextWr

-- | @toByteStringIOWith bufSize io b@ runs the builder @b@ with a buffer of
-- at least the size @bufSize@ and executes the 'IO' action @io@ whenever the
-- buffer is full.
--
-- Compared to 'toLazyByteStringWith' this function requires less allocation,
-- as the output buffer is only allocated once at the start of the
-- serialization and whenever something bigger than the current buffer size has
-- to be copied into the buffer, which should happen very seldomly for the
-- default buffer size of 32kb. Hence, the pressure on the garbage collector is
-- reduced, which can be an advantage when building long sequences of bytes.
--
-- Since 0.1.9
--
toByteStringIOWith :: Int                    -- ^ Buffer size (upper bounds
                                             -- the number of bytes forced
                                             -- per call to the 'IO' action).
                   -> (ByteString -> IO ())  -- ^ 'IO' action to execute per
                                             -- full buffer, which is
                                             -- referenced by a strict
                                             -- 'S.ByteString'.
                   -> Builder                -- ^ 'Builder' to run.
                   -> IO ()
toByteStringIOWith bufSize io b =
    toByteStringIOWithBuffer bufSize io b =<< mallocByteString bufSize

-- | Run the builder with a 'defaultChunkSize'd buffer and execute the given
-- 'IO' action whenever the buffer is full or gets flushed.
--
-- @ 'toByteStringIO' = 'toByteStringIOWith' 'defaultChunkSize'@
--
-- Since 0.1.9
--
toByteStringIO :: (ByteString -> IO ())
               -> Builder
               -> IO ()
toByteStringIO = toByteStringIOWith defaultChunkSize
{-# INLINE toByteStringIO #-}
