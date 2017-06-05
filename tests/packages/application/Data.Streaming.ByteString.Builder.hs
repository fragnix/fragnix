{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Data/Streaming/ByteString/Builder.hs" #-}





















































































{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

-- | Convert a stream of bytestring @Builder@s into a stream of @ByteString@s.
--
-- Adapted from blaze-builder-enumerator, written by Michael Snoyman and Simon Meier.
--
-- Note that the functions here can work in any monad built on top of @IO@ or
-- @ST@.
--
-- Also provides @toByteStringIO*@ like "Blaze.ByteString.Builder"s, for
-- "Data.ByteString.Builder".
--
-- Since 0.1.9
--
module Data.Streaming.ByteString.Builder
    ( BuilderRecv
    , BuilderPopper
    , BuilderFinish
    , newByteStringBuilderRecv

    -- * toByteStringIO
    , toByteStringIO
    , toByteStringIOWith
    , toByteStringIOWithBuffer

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
    )
    where

import Control.Monad (when,unless)
import qualified Data.ByteString as S
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder.Extra (runBuilder, BufferWriter, Next(Done, More, Chunk))
import Data.ByteString.Internal (mallocByteString, ByteString(PS))
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.IORef (newIORef, writeIORef, readIORef)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (plusPtr, minusPtr)

import Data.Streaming.ByteString.Builder.Buffer

-- | Provides a series of @ByteString@s until empty, at which point it provides
-- an empty @ByteString@.
--
-- Since 0.1.10.0
--
type BuilderPopper = IO S.ByteString

type BuilderRecv = Builder -> IO BuilderPopper

type BuilderFinish = IO (Maybe S.ByteString)

newByteStringBuilderRecv :: BufferAllocStrategy -> IO (BuilderRecv, BuilderFinish)
newByteStringBuilderRecv (ioBufInit, nextBuf) = do
    refBuf <- newIORef ioBufInit
    return (push refBuf, finish refBuf)
  where
    finish refBuf = do
        ioBuf <- readIORef refBuf
        buf <- ioBuf
        return $ unsafeFreezeNonEmptyBuffer buf

    push refBuf builder = do
        refWri <- newIORef $ Left $ runBuilder builder
        return $ popper refBuf refWri

    popper refBuf refWri = do
        ioBuf <- readIORef refBuf
        ebWri <- readIORef refWri
        case ebWri of
            Left bWri -> do
                !buf@(Buffer _ _ op ope) <- ioBuf
                (bytes, next) <- bWri op (ope `minusPtr` op)
                let op' = op `plusPtr` bytes
                case next of
                    Done -> do
                        writeIORef refBuf $ return $ updateEndOfSlice buf op'
                        return S.empty
                    More minSize bWri' -> do
                        let buf' = updateEndOfSlice buf op'
                            {-# INLINE cont #-}
                            cont mbs = do
                                -- sequencing the computation of the next buffer
                                -- construction here ensures that the reference to the
                                -- foreign pointer `fp` is lost as soon as possible.
                                ioBuf' <- nextBuf minSize buf'
                                writeIORef refBuf ioBuf'
                                writeIORef refWri $ Left bWri'
                                case mbs of
                                    Just bs | not $ S.null bs -> return bs
                                    _ -> popper refBuf refWri
                        cont $ unsafeFreezeNonEmptyBuffer buf'
                    Chunk bs bWri' -> do
                        let buf' = updateEndOfSlice buf op'
                        let yieldBS = do
                                nextBuf 1 buf' >>= writeIORef refBuf
                                writeIORef refWri $ Left bWri'
                                if S.null bs
                                    then popper refBuf refWri
                                    else return bs
                        case unsafeFreezeNonEmptyBuffer buf' of
                            Nothing -> yieldBS
                            Just bs' -> do
                                writeIORef refWri $ Right yieldBS
                                return bs'
            Right action -> action

-- | Use a pre-existing buffer to 'toByteStringIOWith'.
--
-- Since 0.1.9
--
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
            when (len > 0) (io $! PS buf 0 len)
            case next of
                Done -> return ()
                More newBufSize nextWr
                    | newBufSize > bufSize -> do
                        newBuf <- mallocByteString newBufSize
                        go newBufSize newBuf nextWr
                    | otherwise -> loop nextWr
                Chunk s nextWr -> do
                    unless (S.null s) (io s)
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
{-# INLINE toByteStringIOWith #-}

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
