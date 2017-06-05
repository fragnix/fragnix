{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Data/Streaming/Zlib.hs" #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | This is a middle-level wrapper around the zlib C API. It allows you to
-- work fully with bytestrings and not touch the FFI at all, but is still
-- low-level enough to allow you to implement high-level abstractions such as
-- enumerators. Significantly, it does not use lazy IO.
--
-- You'll probably need to reference the docs a bit to understand the
-- WindowBits parameters below, but a basic rule of thumb is 15 is for zlib
-- compression, and 31 for gzip compression.
--
-- A simple streaming compressor in pseudo-code would look like:
--
-- > def <- initDeflate ...
-- > popper <- feedDeflate def rawContent
-- > pullPopper popper
-- > ...
-- > finishDeflate def sendCompressedData
--
-- You can see a more complete example is available in the included
-- file-test.hs.
module Data.Streaming.Zlib
    ( -- * Inflate
      Inflate
    , initInflate
    , initInflateWithDictionary
    , feedInflate
    , finishInflate
    , flushInflate
    , getUnusedInflate
    , isCompleteInflate
      -- * Deflate
    , Deflate
    , initDeflate
    , initDeflateWithDictionary
    , feedDeflate
    , finishDeflate
    , flushDeflate
    , fullFlushDeflate
      -- * Data types
    , WindowBits (..)
    , defaultWindowBits
    , ZlibException (..)
    , Popper
    , PopperRes (..)
    ) where

import Data.Streaming.Zlib.Lowlevel
import Foreign.ForeignPtr
import Foreign.C.Types
import Data.ByteString.Unsafe
import Codec.Compression.Zlib (WindowBits(WindowBits), defaultWindowBits)
import qualified Data.ByteString as S
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Control.Monad (when)
import Data.IORef

type ZStreamPair = (ForeignPtr ZStreamStruct, ForeignPtr CChar)

-- | The state of an inflation (eg, decompression) process. All allocated
-- memory is automatically reclaimed by the garbage collector.
-- Also can contain the inflation dictionary that is used for decompression.
data Inflate = Inflate
    ZStreamPair
    (IORef S.ByteString) -- last ByteString fed in, needed for getUnusedInflate
    (IORef Bool)         -- set True when zlib indicates that inflation is complete
    (Maybe S.ByteString) -- dictionary

-- | The state of a deflation (eg, compression) process. All allocated memory
-- is automatically reclaimed by the garbage collector.
newtype Deflate = Deflate ZStreamPair

-- | Exception that can be thrown from the FFI code. The parameter is the
-- numerical error code from the zlib library. Quoting the zlib.h file
-- directly:
--
-- * #define Z_OK            0
--
-- * #define Z_STREAM_END    1
--
-- * #define Z_NEED_DICT     2
--
-- * #define Z_ERRNO        (-1)
--
-- * #define Z_STREAM_ERROR (-2)
--
-- * #define Z_DATA_ERROR   (-3)
--
-- * #define Z_MEM_ERROR    (-4)
--
-- * #define Z_BUF_ERROR    (-5)
--
-- * #define Z_VERSION_ERROR (-6)

data ZlibException = ZlibException Int
    deriving (Show, Typeable)
instance Exception ZlibException

-- | Some constants for the error codes, used internally
zStreamEnd :: CInt
zStreamEnd = 1

zNeedDict :: CInt
zNeedDict = 2

zBufError :: CInt
zBufError = -5

-- | Initialize an inflation process with the given 'WindowBits'. You will need
-- to call 'feedInflate' to feed compressed data to this and
-- 'finishInflate' to extract the final chunk of decompressed data.
initInflate :: WindowBits -> IO Inflate
initInflate w = do
    zstr <- zstreamNew
    inflateInit2 zstr w
    fzstr <- newForeignPtr c_free_z_stream_inflate zstr
    fbuff <- mallocForeignPtrBytes defaultChunkSize
    withForeignPtr fbuff $ \buff ->
        c_set_avail_out zstr buff $ fromIntegral defaultChunkSize
    lastBS <- newIORef S.empty
    complete <- newIORef False
    return $ Inflate (fzstr, fbuff) lastBS complete Nothing

-- | Initialize an inflation process with the given 'WindowBits'.
-- Unlike initInflate a dictionary for inflation is set which must
-- match the one set during compression.
initInflateWithDictionary :: WindowBits -> S.ByteString -> IO Inflate
initInflateWithDictionary w bs = do
    zstr <- zstreamNew
    inflateInit2 zstr w
    fzstr <- newForeignPtr c_free_z_stream_inflate zstr
    fbuff <- mallocForeignPtrBytes defaultChunkSize

    withForeignPtr fbuff $ \buff ->
        c_set_avail_out zstr buff $ fromIntegral defaultChunkSize
    lastBS <- newIORef S.empty
    complete <- newIORef False
    return $ Inflate (fzstr, fbuff) lastBS complete (Just bs)

-- | Initialize a deflation process with the given compression level and
-- 'WindowBits'. You will need to call 'feedDeflate' to feed uncompressed
-- data to this and 'finishDeflate' to extract the final chunks of compressed
-- data.
initDeflate :: Int -- ^ Compression level
            -> WindowBits -> IO Deflate
initDeflate level w = do
    zstr <- zstreamNew
    deflateInit2 zstr level w 8 StrategyDefault
    fzstr <- newForeignPtr c_free_z_stream_deflate zstr
    fbuff <- mallocForeignPtrBytes defaultChunkSize
    withForeignPtr fbuff $ \buff ->
        c_set_avail_out zstr buff $ fromIntegral defaultChunkSize
    return $ Deflate (fzstr, fbuff)

-- | Initialize an deflation process with the given compression level and
-- 'WindowBits'.
-- Unlike initDeflate a dictionary for deflation is set.
initDeflateWithDictionary :: Int -- ^ Compression level
                          -> S.ByteString -- ^ Deflate dictionary
                          -> WindowBits -> IO Deflate
initDeflateWithDictionary level bs w = do
    zstr <- zstreamNew
    deflateInit2 zstr level w 8 StrategyDefault
    fzstr <- newForeignPtr c_free_z_stream_deflate zstr
    fbuff <- mallocForeignPtrBytes defaultChunkSize

    unsafeUseAsCStringLen bs $ \(cstr, len) -> do
        c_call_deflate_set_dictionary zstr cstr $ fromIntegral len

    withForeignPtr fbuff $ \buff ->
        c_set_avail_out zstr buff $ fromIntegral defaultChunkSize
    return $ Deflate (fzstr, fbuff)

-- | Feed the given 'S.ByteString' to the inflater. Return a 'Popper',
-- an IO action that returns the decompressed data a chunk at a time.
-- The 'Popper' must be called to exhaustion before using the 'Inflate'
-- object again.
--
-- Note that this function automatically buffers the output to
-- 'defaultChunkSize', and therefore you won't get any data from the popper
-- until that much decompressed data is available. After you have fed all of
-- the compressed data to this function, you can extract your final chunk of
-- decompressed data using 'finishInflate'.
feedInflate
    :: Inflate
    -> S.ByteString
    -> IO Popper
feedInflate (Inflate (fzstr, fbuff) lastBS complete inflateDictionary) bs = do
    -- Write the BS to lastBS for use by getUnusedInflate. This is
    -- theoretically unnecessary, since we could just grab the pointer from the
    -- fzstr when needed. However, in that case, we wouldn't be holding onto a
    -- reference to the ForeignPtr, so the GC may decide to collect the
    -- ByteString in the interim.
    writeIORef lastBS bs

    withForeignPtr fzstr $ \zstr ->
        unsafeUseAsCStringLen bs $ \(cstr, len) ->
            c_set_avail_in zstr cstr $ fromIntegral len
    return $ drain fbuff fzstr (Just bs) inflate False
  where
    inflate zstr = do
        res <- c_call_inflate_noflush zstr
        res2 <- if (res == zNeedDict)
            then maybe (return zNeedDict)
                       (\dict -> (unsafeUseAsCStringLen dict $ \(cstr, len) -> do
                                    c_call_inflate_set_dictionary zstr cstr $ fromIntegral len
                                    c_call_inflate_noflush zstr))
                       inflateDictionary
            else return res
        when (res2 == zStreamEnd) (writeIORef complete True)
        return res2

-- | An IO action that returns the next chunk of data, returning 'Nothing' when
-- there is no more data to be popped.
type Popper = IO PopperRes

data PopperRes = PRDone
               | PRNext !S.ByteString
               | PRError !ZlibException
    deriving (Show, Typeable)

-- | Ensure that the given @ByteString@ is not deallocated.
keepAlive :: Maybe S.ByteString -> IO a -> IO a
keepAlive Nothing = id
keepAlive (Just bs) = unsafeUseAsCStringLen bs . const

drain :: ForeignPtr CChar
      -> ForeignPtr ZStreamStruct
      -> Maybe S.ByteString
      -> (ZStream' -> IO CInt)
      -> Bool
      -> Popper
drain fbuff fzstr mbs func isFinish = withForeignPtr fzstr $ \zstr -> keepAlive mbs $ do
    res <- func zstr
    if res < 0 && res /= zBufError
        then return $ PRError $ ZlibException $ fromIntegral res
        else do
            avail <- c_get_avail_out zstr
            let size = defaultChunkSize - fromIntegral avail
                toOutput = avail == 0 || (isFinish && size /= 0)
            if toOutput
                then withForeignPtr fbuff $ \buff -> do
                    bs <- S.packCStringLen (buff, size)
                    c_set_avail_out zstr buff
                        $ fromIntegral defaultChunkSize
                    return $ PRNext bs
                else return PRDone


-- | As explained in 'feedInflate', inflation buffers your decompressed
-- data. After you call 'feedInflate' with your last chunk of compressed
-- data, you will likely have some data still sitting in the buffer. This
-- function will return it to you.
finishInflate :: Inflate -> IO S.ByteString
finishInflate (Inflate (fzstr, fbuff) _ _ _) =
    withForeignPtr fzstr $ \zstr ->
        withForeignPtr fbuff $ \buff -> do
            avail <- c_get_avail_out zstr
            let size = defaultChunkSize - fromIntegral avail
            bs <- S.packCStringLen (buff, size)
            c_set_avail_out zstr buff $ fromIntegral defaultChunkSize
            return bs

-- | Flush the inflation buffer. Useful for interactive application.
--
-- This is actually a synonym for 'finishInflate'. It is provided for its more
-- semantic name.
--
-- Since 0.0.3
flushInflate :: Inflate -> IO S.ByteString
flushInflate = finishInflate

-- | Retrieve any data remaining after inflating. For more information on motivation, see:
--
-- <https://github.com/fpco/streaming-commons/issues/20>
--
-- Since 0.1.11
getUnusedInflate :: Inflate -> IO S.ByteString
getUnusedInflate (Inflate (fzstr, _) ref _ _) = do
    bs <- readIORef ref
    len <- withForeignPtr fzstr c_get_avail_in
    return $ S.drop (S.length bs - fromIntegral len) bs

-- | Returns True if the inflater has reached end-of-stream, or False if
-- it is still expecting more data.
--
-- Since 0.1.18
isCompleteInflate :: Inflate -> IO Bool
isCompleteInflate (Inflate _ _ complete _) = readIORef complete

-- | Feed the given 'S.ByteString' to the deflater. Return a 'Popper',
-- an IO action that returns the compressed data a chunk at a time.
-- The 'Popper' must be called to exhaustion before using the 'Deflate'
-- object again.
--
-- Note that this function automatically buffers the output to
-- 'defaultChunkSize', and therefore you won't get any data from the popper
-- until that much compressed data is available. After you have fed all of the
-- decompressed data to this function, you can extract your final chunks of
-- compressed data using 'finishDeflate'.
feedDeflate :: Deflate -> S.ByteString -> IO Popper
feedDeflate (Deflate (fzstr, fbuff)) bs = do
    withForeignPtr fzstr $ \zstr ->
        unsafeUseAsCStringLen bs $ \(cstr, len) -> do
            c_set_avail_in zstr cstr $ fromIntegral len
    return $ drain fbuff fzstr (Just bs) c_call_deflate_noflush False

-- | As explained in 'feedDeflate', deflation buffers your compressed
-- data. After you call 'feedDeflate' with your last chunk of uncompressed
-- data, use this to flush the rest of the data and signal end of input.
finishDeflate :: Deflate -> Popper
finishDeflate (Deflate (fzstr, fbuff)) =
    drain fbuff fzstr Nothing c_call_deflate_finish True

-- | Flush the deflation buffer. Useful for interactive application.
-- Internally this passes Z_SYNC_FLUSH to the zlib library.
--
-- Unlike 'finishDeflate', 'flushDeflate' does not signal end of input,
-- meaning you can feed more uncompressed data afterward.
--
-- Since 0.0.3
flushDeflate :: Deflate -> Popper
flushDeflate (Deflate (fzstr, fbuff)) =
    drain fbuff fzstr Nothing c_call_deflate_flush True

-- | Full flush the deflation buffer. Useful for interactive
-- applications where previously streamed data may not be
-- available. Using `fullFlushDeflate` too often can seriously degrade
-- compression. Internally this passes Z_FULL_FLUSH to the zlib
-- library.
--
-- Like 'flushDeflate', 'fullFlushDeflate' does not signal end of input,
-- meaning you can feed more uncompressed data afterward.
--
-- Since 0.1.5
fullFlushDeflate :: Deflate -> Popper
fullFlushDeflate (Deflate (fzstr, fbuff)) =
    drain fbuff fzstr Nothing c_call_deflate_full_flush True
