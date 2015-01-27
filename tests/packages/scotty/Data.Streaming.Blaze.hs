{-# LINE 1 "Data/Streaming/Blaze.hs" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
-- | Convert a stream of blaze-builder @Builder@s into a stream of @ByteString@s.
--
-- Adapted from blaze-builder-enumerator, written by myself and Simon Meier.
--
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

import Data.IORef

import qualified Data.ByteString                   as S

import Blaze.ByteString.Builder.Internal
import Blaze.ByteString.Builder.Internal.Types
import Blaze.ByteString.Builder.Internal.Buffer

-- | Provides a series of @ByteString@s until empty, at which point it provides
-- an empty @ByteString@.
--
-- Since 0.1.2
type BlazePopper = IO S.ByteString

type BlazeRecv = Builder -> IO BlazePopper

type BlazeFinish = IO (Maybe S.ByteString)

defaultStrategy :: BufferAllocStrategy
defaultStrategy = allNewBuffersStrategy defaultBufferSize

newBlazeRecv :: BufferAllocStrategy -> IO (BlazeRecv, BlazeFinish)
newBlazeRecv (ioBufInit, nextBuf) = do
    refBuf <- newIORef ioBufInit
    return (push refBuf, finish refBuf)
  where
    finish refBuf = do
        ioBuf <- readIORef refBuf
        buf <- ioBuf
        return $ unsafeFreezeNonEmptyBuffer buf

    push refBuf builder = do
        refStep <- newIORef $ Left $ unBuilder builder (buildStep finalStep)
        return $ popper refBuf refStep
      where
        finalStep !(BufRange pf _) = return $ Done pf ()

    popper refBuf refStep = do
        ioBuf <- readIORef refBuf
        ebStep <- readIORef refStep
        case ebStep of
            Left bStep -> do
                !buf   <- ioBuf
                signal <- execBuildStep bStep buf
                case signal of
                    Done op' _ -> do
                        writeIORef refBuf $ return $ updateEndOfSlice buf op'
                        return S.empty
                    BufferFull minSize op' bStep' -> do
                        let buf' = updateEndOfSlice buf op'
                            {-# INLINE cont #-}
                            cont mbs = do
                                -- sequencing the computation of the next buffer
                                -- construction here ensures that the reference to the
                                -- foreign pointer `fp` is lost as soon as possible.
                                ioBuf' <- nextBuf minSize buf'
                                writeIORef refBuf ioBuf'
                                writeIORef refStep $ Left bStep'
                                case mbs of
                                    Just bs | not $ S.null bs -> return bs
                                    _ -> popper refBuf refStep
                        cont $ unsafeFreezeNonEmptyBuffer buf'
                    InsertByteString op' bs bStep' -> do
                        let buf' = updateEndOfSlice buf op'
                        let yieldBS = do
                                nextBuf 1 buf' >>= writeIORef refBuf
                                writeIORef refStep $ Left bStep'
                                if S.null bs
                                    then popper refBuf refStep
                                    else return bs
                        case unsafeFreezeNonEmptyBuffer buf' of
                            Nothing -> yieldBS
                            Just bs' -> do
                                writeIORef refStep $ Right yieldBS
                                return bs'
            Right action -> action

{-
helper :: (MonadBase base m, PrimMonad base, Monad (t m), MonadTrans t)
       => t m (Maybe (Flush Builder))
       -> (Flush S.ByteString -> t m ())
       -> BufferAllocStrategy
       -> t m ()
helper await' yield' (ioBufInit, nextBuf) =
    loop ioBufInit
  where
    loop ioBuf = do
        await' >>= maybe (close ioBuf) (cont' ioBuf)

    cont' ioBuf Flush = push ioBuf flush $ \ioBuf' -> yield' Flush >> loop ioBuf'
    cont' ioBuf (Chunk builder) = push ioBuf builder loop

    close ioBuf = do
        buf <- lift $ unsafeLiftIO $ ioBuf
        maybe (return ()) (yield' . Chunk) (unsafeFreezeNonEmptyBuffer buf)
-}
