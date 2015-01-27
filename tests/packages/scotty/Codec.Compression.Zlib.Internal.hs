{-# LINE 1 "Codec/Compression/Zlib/Internal.hs" #-}
# 1 "Codec/Compression/Zlib/Internal.hs"
# 1 "<command-line>"
# 8 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 8 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1






















































































# 8 "<command-line>" 2
# 1 "Codec/Compression/Zlib/Internal.hs"
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006-2008 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable (H98 + FFI)
--
-- Pure stream based interface to lower level zlib wrapper
--
-----------------------------------------------------------------------------
module Codec.Compression.Zlib.Internal (

  -- * Compression
  compress,
  CompressParams(..),
  defaultCompressParams,

  -- * Decompression
  decompress,
  DecompressParams(..),
  defaultDecompressParams,

  -- * The compression parameter types
  Stream.Format(..),
    Stream.gzipFormat,
    Stream.zlibFormat,
    Stream.rawFormat,
    Stream.gzipOrZlibFormat,
  Stream.CompressionLevel(..),
    Stream.defaultCompression,
    Stream.noCompression,
    Stream.bestSpeed,
    Stream.bestCompression,
    Stream.compressionLevel,
  Stream.Method(..),
    Stream.deflateMethod,
  Stream.WindowBits(..),
    Stream.defaultWindowBits,
    Stream.windowBits,
  Stream.MemoryLevel(..),
    Stream.defaultMemoryLevel,
    Stream.minMemoryLevel,
    Stream.maxMemoryLevel,
    Stream.memoryLevel,
  Stream.CompressionStrategy(..),
    Stream.defaultStrategy,
    Stream.filteredStrategy,
    Stream.huffmanOnlyStrategy,

  -- * Low-level API to get explicit error reports
  decompressWithErrors,
  DecompressStream(..),
  DecompressError(..),
  foldDecompressStream,
  fromDecompressStream,
  ) where

import Prelude hiding (length)
import Control.Monad (when)
import Control.Exception (assert)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString.Internal as S

import qualified Codec.Compression.Zlib.Stream as Stream
import Codec.Compression.Zlib.Stream (Stream)

-- | The full set of parameters for compression. The defaults are
-- 'defaultCompressParams'.
--
-- The 'compressBufferSize' is the size of the first output buffer containing
-- the compressed data. If you know an approximate upper bound on the size of
-- the compressed data then setting this parameter can save memory. The default
-- compression output buffer size is @16k@. If your extimate is wrong it does
-- not matter too much, the default buffer size will be used for the remaining
-- chunks.
--
data CompressParams = CompressParams {
  compressLevel       :: !Stream.CompressionLevel,
  compressMethod      :: !Stream.Method,
  compressWindowBits  :: !Stream.WindowBits,
  compressMemoryLevel :: !Stream.MemoryLevel,
  compressStrategy    :: !Stream.CompressionStrategy,
  compressBufferSize  :: !Int,
  compressDictionary  :: Maybe S.ByteString
}

-- | The full set of parameters for decompression. The defaults are
-- 'defaultDecompressParams'.
--
-- The 'decompressBufferSize' is the size of the first output buffer,
-- containing the uncompressed data. If you know an exact or approximate upper
-- bound on the size of the decompressed data then setting this parameter can
-- save memory. The default decompression output buffer size is @32k@. If your
-- extimate is wrong it does not matter too much, the default buffer size will
-- be used for the remaining chunks.
--
-- One particular use case for setting the 'decompressBufferSize' is if you
-- know the exact size of the decompressed data and want to produce a strict
-- 'Data.ByteString.ByteString'. The compression and deccompression functions
-- use lazy 'Data.ByteString.Lazy.ByteString's but if you set the
-- 'decompressBufferSize' correctly then you can generate a lazy
-- 'Data.ByteString.Lazy.ByteString' with exactly one chunk, which can be
-- converted to a strict 'Data.ByteString.ByteString' in @O(1)@ time using
-- @'Data.ByteString.concat' . 'Data.ByteString.Lazy.toChunks'@.
--
data DecompressParams = DecompressParams {
  decompressWindowBits :: !Stream.WindowBits,
  decompressBufferSize :: !Int,
  decompressDictionary :: Maybe S.ByteString
}

-- | The default set of parameters for compression. This is typically used with
-- the @compressWith@ function with specific parameters overridden.
--
defaultCompressParams :: CompressParams
defaultCompressParams = CompressParams {
  compressLevel       = Stream.defaultCompression,
  compressMethod      = Stream.deflateMethod,
  compressWindowBits  = Stream.defaultWindowBits,
  compressMemoryLevel = Stream.defaultMemoryLevel,
  compressStrategy    = Stream.defaultStrategy,
  compressBufferSize  = defaultCompressBufferSize,
  compressDictionary  = Nothing
}

-- | The default set of parameters for decompression. This is typically used with
-- the @compressWith@ function with specific parameters overridden.
--
defaultDecompressParams :: DecompressParams
defaultDecompressParams = DecompressParams {
  decompressWindowBits = Stream.defaultWindowBits,
  decompressBufferSize = defaultDecompressBufferSize,
  decompressDictionary = Nothing
}

-- | The default chunk sizes for the output of compression and decompression
-- are 16k and 32k respectively (less a small accounting overhead).
--
defaultCompressBufferSize, defaultDecompressBufferSize :: Int
defaultCompressBufferSize   = 16 * 1024 - L.chunkOverhead
defaultDecompressBufferSize = 32 * 1024 - L.chunkOverhead

-- | A sequence of chunks of data produced from decompression.
--
-- The difference from a simple list is that it contains a representation of
-- errors as data rather than as exceptions. This allows you to handle error
-- conditions explicitly.
--
data DecompressStream = StreamEnd
                      | StreamChunk S.ByteString DecompressStream

                        -- | An error code and a human readable error message.
                      | StreamError DecompressError String

-- | The possible error cases when decompressing a stream.
--
data DecompressError =
     -- | The compressed data stream ended prematurely. This may happen if the
     -- input data stream was truncated.
     TruncatedInput

     -- | It is possible to do zlib compression with a custom dictionary. This
     -- allows slightly higher compression ratios for short files. However such
     -- compressed streams require the same dictionary when decompressing. This
     -- error is for when we encounter a compressed stream that needs a
     -- dictionary, and it's not provided.
   | DictionaryRequired

     -- | If the compressed data stream is corrupted in any way then you will
     -- get this error, for example if the input data just isn't a compressed
     -- zlib data stream. In particular if the data checksum turns out to be
     -- wrong then you will get all the decompressed data but this error at the
     -- end, instead of the normal sucessful 'StreamEnd'.
   | DataError

-- | Fold an 'DecompressionStream'. Just like 'foldr' but with an extra error
-- case. For example to convert to a list and translate the errors into
-- exceptions:
--
-- > foldDecompressStream (:) [] (\code msg -> error msg)
--
foldDecompressStream :: (S.ByteString -> a -> a) -> a
                 -> (DecompressError -> String -> a)
                 -> DecompressStream -> a
foldDecompressStream chunk end err = fold
  where
    fold StreamEnd               = end
    fold (StreamChunk bs stream) = chunk bs (fold stream)
    fold (StreamError code msg)  = err code msg

-- | Convert a 'DecompressStream' to a lazy 'ByteString'. If any decompression
-- errors are encountered then they are thrown as exceptions.
--
-- This is a special case of 'foldDecompressStream'.
--
fromDecompressStream :: DecompressStream -> L.ByteString
fromDecompressStream =
  foldDecompressStream L.Chunk L.Empty
    (\_code msg -> error ("Codec.Compression.Zlib: " ++ msg))

--TODO: throw DecompressError as an Exception class type and document that it
-- does this.

-- | Compress a data stream.
--
-- There are no expected error conditions. All input data streams are valid. It
-- is possible for unexpected errors to occur, such as running out of memory,
-- or finding the wrong version of the zlib C library, these are thrown as
-- exceptions.
--
compress
  :: Stream.Format
  -> CompressParams
  -> L.ByteString
  -> L.ByteString
compress format
  (CompressParams compLevel method bits memLevel strategy initChunkSize mdict)
  input =
  L.fromChunks $ Stream.run $ do
    Stream.deflateInit format compLevel method bits memLevel strategy
    setDictionary mdict
    case L.toChunks input of
      [] -> fillBuffers 20 [] --gzip header is 20 bytes, others even smaller
      S.PS inFPtr offset length : chunks -> do
        Stream.pushInputBuffer inFPtr offset length
        r <- fillBuffers initChunkSize chunks
        return r

  where
    -- we flick between two states:
    --   * where one or other buffer is empty
    --       - in which case we refill one or both
    --   * where both buffers are non-empty
    --       - in which case we compress until a buffer is empty

  fillBuffers :: Int
              -> [S.ByteString]
              -> Stream [S.ByteString]
  fillBuffers outChunkSize inChunks = do




    -- in this state there are two possabilities:
    --   * no outbut buffer space is available
    --       - in which case we must make more available
    --   * no input buffer is available
    --       - in which case we must supply more
    inputBufferEmpty <- Stream.inputBufferEmpty
    outputBufferFull <- Stream.outputBufferFull

    assert (inputBufferEmpty || outputBufferFull) $ return ()

    when outputBufferFull $ do
      outFPtr <- Stream.unsafeLiftIO (S.mallocByteString outChunkSize)
      Stream.pushOutputBuffer outFPtr 0 outChunkSize

    if inputBufferEmpty
      then case inChunks of
             [] -> drainBuffers []
             S.PS inFPtr offset length : inChunks' -> do
                Stream.pushInputBuffer inFPtr offset length
                drainBuffers inChunks'
      else drainBuffers inChunks


  drainBuffers ::
      [S.ByteString]
   -> Stream [S.ByteString]
  drainBuffers inChunks = do

    inputBufferEmpty' <- Stream.inputBufferEmpty
    outputBufferFull' <- Stream.outputBufferFull
    assert(not outputBufferFull'
       && (null inChunks || not inputBufferEmpty')) $ return ()
    -- this invariant guarantees we can always make forward progress
    -- and that therefore a BufferError is impossible

    let flush = if null inChunks then Stream.Finish else Stream.NoFlush
    status <- Stream.deflate flush

    case status of
      Stream.Ok -> do
        outputBufferFull <- Stream.outputBufferFull
        if outputBufferFull
          then do (outFPtr, offset, length) <- Stream.popOutputBuffer
                  outChunks <- Stream.unsafeInterleave
                    (fillBuffers defaultCompressBufferSize inChunks)
                  return (S.PS outFPtr offset length : outChunks)
          else do fillBuffers defaultCompressBufferSize inChunks

      Stream.StreamEnd -> do
        inputBufferEmpty <- Stream.inputBufferEmpty
        assert inputBufferEmpty $ return ()
        outputBufferBytesAvailable <- Stream.outputBufferBytesAvailable
        if outputBufferBytesAvailable > 0
          then do (outFPtr, offset, length) <- Stream.popOutputBuffer
                  Stream.finalise
                  return [S.PS outFPtr offset length]
          else do Stream.finalise
                  return []

      Stream.Error code msg -> case code of
        Stream.BufferError  -> fail "BufferError should be impossible!"
        Stream.NeedDict _   -> fail "NeedDict is impossible!"
        _                   -> fail msg

  -- Set the custom dictionary, if we were provided with one
  -- and if the format supports it (zlib and raw, not gzip).
  setDictionary :: Maybe S.ByteString -> Stream ()
  setDictionary (Just dict)
    | Stream.formatSupportsDictionary format = do
        status <- Stream.deflateSetDictionary dict
        case status of
          Stream.Ok          -> return ()
          Stream.Error _ msg -> fail msg
          _                  -> fail "error when setting deflate dictionary"
  setDictionary _ = return ()


-- | Decompress a data stream.
--
-- It will throw an exception if any error is encountered in the input data. If
-- you need more control over error handling then use 'decompressWithErrors'.
--
decompress
  :: Stream.Format
  -> DecompressParams
  -> L.ByteString
  -> L.ByteString
decompress format params = fromDecompressStream
                         . decompressWithErrors format params

-- | Like 'decompress' but returns a 'DecompressStream' data structure that
-- contains an explicit representation of the error conditions that one may
-- encounter when decompressing.
--
-- Note that in addition to errors in the input data, it is possible for other
-- unexpected errors to occur, such as out of memory, or finding the wrong
-- version of the zlib C library, these are still thrown as exceptions (because
-- representing them as data would make this function impure).
--
decompressWithErrors
  :: Stream.Format
  -> DecompressParams
  -> L.ByteString
  -> DecompressStream
decompressWithErrors format (DecompressParams bits initChunkSize mdict) input =
  Stream.run $ do
    Stream.inflateInit format bits
    case L.toChunks input of
      [] -> fillBuffers 4 [] --always an error anyway
      S.PS inFPtr offset length : chunks -> do
        Stream.pushInputBuffer inFPtr offset length
        fillBuffers initChunkSize chunks

  where
    -- we flick between two states:
    --   * where one or other buffer is empty
    --       - in which case we refill one or both
    --   * where both buffers are non-empty
    --       - in which case we compress until a buffer is empty

  fillBuffers :: Int
              -> [S.ByteString]
              -> Stream DecompressStream
  fillBuffers outChunkSize inChunks = do




    -- in this state there are two possabilities:
    --   * no outbut buffer space is available
    --       - in which case we must make more available
    --   * no input buffer is available
    --       - in which case we must supply more
    inputBufferEmpty <- Stream.inputBufferEmpty
    outputBufferFull <- Stream.outputBufferFull

    assert (inputBufferEmpty || outputBufferFull) $ return ()

    when outputBufferFull $ do
      outFPtr <- Stream.unsafeLiftIO (S.mallocByteString outChunkSize)
      Stream.pushOutputBuffer outFPtr 0 outChunkSize

    if inputBufferEmpty
      then case inChunks of
             [] -> drainBuffers []
             S.PS inFPtr offset length : inChunks' -> do
                Stream.pushInputBuffer inFPtr offset length
                drainBuffers inChunks'
      else drainBuffers inChunks


  drainBuffers ::
      [S.ByteString]
   -> Stream DecompressStream
  drainBuffers inChunks = do

    inputBufferEmpty' <- Stream.inputBufferEmpty
    outputBufferFull' <- Stream.outputBufferFull
    assert(not outputBufferFull'
       && (null inChunks || not inputBufferEmpty')) $ return ()
    -- this invariant guarantees we can always make forward progress or at
    -- least if a BufferError does occur that it must be due to a premature EOF

    status <- Stream.inflate Stream.NoFlush

    case status of
      Stream.Ok -> do
        outputBufferFull <- Stream.outputBufferFull
        if outputBufferFull
          then do (outFPtr, offset, length) <- Stream.popOutputBuffer
                  outChunks <- Stream.unsafeInterleave
                    (fillBuffers defaultDecompressBufferSize inChunks)
                  return $ StreamChunk (S.PS outFPtr offset length) outChunks
          else do fillBuffers defaultDecompressBufferSize inChunks

      Stream.StreamEnd      -> inChunks `seq` finish StreamEnd
        -- The decompressor tells us we're done, but that doesn't mean we have
        -- consumed all the input (there could be trailing data). But more
        -- subtle than that, the decompressor will actually never demand the
        -- tail of the input (in the usual case where it's empty) because
        -- the zlib and gzip formats know their own length. So we force the
        -- tail of the input here because this can be important for closing
        -- file handles etc.

      Stream.Error code msg -> case code of
        Stream.BufferError  -> finish (StreamError TruncatedInput msg')
          where msg' = "premature end of compressed stream"
        Stream.NeedDict adler -> do
          err <- setDictionary adler mdict
          case err of
            Just streamErr  -> finish streamErr
            Nothing         -> drainBuffers inChunks
        Stream.DataError    -> finish (StreamError DataError msg)
        _                   -> fail msg

  -- Note even if we end with an error we still try to flush the last chunk if
  -- there is one. The user just has to decide what they want to trust.
  finish end = do
    -- Note that there may be input bytes still available if the stream
    -- is embeded in some other data stream. Here we just silently discard
    -- any trailing data.
    outputBufferBytesAvailable <- Stream.outputBufferBytesAvailable
    if outputBufferBytesAvailable > 0
      then do (outFPtr, offset, length) <- Stream.popOutputBuffer
              Stream.finalise
              return (StreamChunk (S.PS outFPtr offset length) end)
      else do Stream.finalise
              return end

  setDictionary :: Stream.DictionaryHash -> Maybe S.ByteString
                -> Stream (Maybe DecompressStream)
  setDictionary _adler Nothing =
    return $ Just (StreamError DictionaryRequired "custom dictionary needed")
  setDictionary _adler (Just dict) = do
    status <- Stream.inflateSetDictionary dict
    case status of
      Stream.Ok -> return Nothing
      Stream.Error Stream.StreamError _ ->
        return $ Just (StreamError DictionaryRequired "provided dictionary not valid")
      Stream.Error Stream.DataError _   ->
        return $ Just (StreamError DictionaryRequired "given dictionary does not match the expected one")
      _ -> fail "error when setting inflate dictionary"
