{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Blaze/ByteString/Builder.hs" #-}























































{-# LANGUAGE CPP, BangPatterns          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

------------------------------------------------------------------------------
-- |
-- Module:      Blaze.ByteString.Builder
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- "Blaze.ByteString.Builder" is the main module, which you should import as a user
-- of the @blaze-builder@ library.
--
-- > import Blaze.ByteString.Builder
--
-- It provides you with a type 'Builder' that allows to efficiently construct
-- lazy bytestrings with a large average chunk size.
--
-- Intuitively, a 'Builder' denotes the construction of a part of a lazy
-- bytestring. Builders can either be created using one of the primitive
-- combinators in "Blaze.ByteString.Builder.Write" or by using one of the predefined
-- combinators for standard Haskell values (see the exposed modules of this
-- package).  Concatenation of builders is done using 'mappend' from the
-- 'Monoid' typeclass.
--
-- Here is a small example that serializes a list of strings using the UTF-8
-- encoding.
--
-- @ import "Blaze.ByteString.Builder.Char.Utf8"@
--
-- > strings :: [String]
-- > strings = replicate 10000 "Hello there!"
--
-- The function @'fromString'@ creates a 'Builder' denoting the UTF-8 encoded
-- argument. Hence, UTF-8 encoding and concatenating all @strings@ can be done
-- follows.
--
-- > concatenation :: Builder
-- > concatenation = mconcat $ map fromString strings
--
-- The function 'toLazyByteString'  can be used to execute a 'Builder' and
-- obtain the resulting lazy bytestring.
--
-- > result :: L.ByteString
-- > result = toLazyByteString concatenation
--
-- The @result@ is a lazy bytestring containing 10000 repetitions of the string
-- @\"Hello there!\"@ encoded using UTF-8. The corresponding 120000 bytes are
-- distributed among three chunks of 32kb and a last chunk of 6kb.
--
-- /A note on history./ This serialization library was inspired by the
-- @Data.Binary.Builder@ module provided by the @binary@ package. It was
-- originally developed with the specific needs of the @blaze-html@ package in
-- mind. Since then it has been restructured to serve as a drop-in replacement
-- for @Data.Binary.Builder@, which it improves upon both in speed as well as
-- expressivity.
--
------------------------------------------------------------------------------

module Blaze.ByteString.Builder
    (
      -- * The 'Builder' type
      B.Builder

      -- * Creating builders
    , module Blaze.ByteString.Builder.Int
    , module Blaze.ByteString.Builder.Word
    , module Blaze.ByteString.Builder.ByteString
    , B.flush

      -- * Executing builders
    , B.toLazyByteString
    , toLazyByteStringWith
    , toByteString
    , toByteStringIO
    , toByteStringIOWith

    -- * 'Write's
    , W.Write
    , W.fromWrite
    , W.fromWriteSingleton
    , W.fromWriteList
    , writeToByteString

    -- ** Writing 'Storable's
    , W.writeStorable
    , W.fromStorable
    , W.fromStorables

    ) where

import Control.Monad(unless)

import Foreign
import qualified Foreign.ForeignPtr.Unsafe as Unsafe

import qualified Blaze.ByteString.Builder.Internal.Write as W
import           Blaze.ByteString.Builder.ByteString
import           Blaze.ByteString.Builder.Word
import           Blaze.ByteString.Builder.Int

import           Data.ByteString.Builder ( Builder )
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Builder.Extra as B

import qualified Data.ByteString               as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Internal as L

import System.IO.Unsafe (unsafeDupablePerformIO)



-- | Pack the chunks of a lazy bytestring into a single strict bytestring.
packChunks :: L.ByteString -> S.ByteString
packChunks lbs = do
    S.unsafeCreate (fromIntegral $ L.length lbs) (copyChunks lbs)
  where
    copyChunks !L.Empty                         !_pf = return ()
    copyChunks !(L.Chunk (S.PS fpbuf o l) lbs') !pf  = do
        withForeignPtr fpbuf $ \pbuf ->
            copyBytes pf (pbuf `plusPtr` o) l
        copyChunks lbs' (pf `plusPtr` l)

-- | Run the builder to construct a strict bytestring containing the sequence
-- of bytes denoted by the builder. This is done by first serializing to a lazy bytestring and then packing its
-- chunks to a appropriately sized strict bytestring.
--
-- > toByteString = packChunks . toLazyByteString
--
-- Note that @'toByteString'@ is a 'Monoid' homomorphism.
--
-- > toByteString mempty          == mempty
-- > toByteString (x `mappend` y) == toByteString x `mappend` toByteString y
--
-- However, in the second equation, the left-hand-side is generally faster to
-- execute.
--
toByteString :: Builder -> S.ByteString
toByteString = packChunks . B.toLazyByteString

-- | Default size (~32kb) for the buffer that becomes a chunk of the output
-- stream once it is filled.
--
defaultBufferSize :: Int
defaultBufferSize = 32 * 1024 - overhead -- Copied from Data.ByteString.Lazy.
    where overhead = 2 * sizeOf (undefined :: Int)


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
toByteStringIO :: (S.ByteString -> IO ()) -> Builder -> IO ()
toByteStringIO = toByteStringIOWith defaultBufferSize

toByteStringIOWith :: Int                      -- ^ Buffer size (upper bounds
                                               -- the number of bytes forced
                                               -- per call to the 'IO' action).
                   -> (S.ByteString -> IO ())  -- ^ 'IO' action to execute per
                                               -- full buffer, which is
                                               -- referenced by a strict
                                               -- 'S.ByteString'.
                   -> Builder                -- ^ 'Builder' to run.
                   -> IO ()                    -- ^ Resulting 'IO' action.
toByteStringIOWith !bufSize io builder = do
    S.mallocByteString bufSize >>= getBuffer (B.runBuilder builder) bufSize
  where
    getBuffer writer !size fp = do
      let !ptr = Unsafe.unsafeForeignPtrToPtr fp
      (bytes, next) <- writer ptr size
      case next of
        B.Done -> io $! S.PS fp 0 bytes
        B.More req writer' -> do
           io $! S.PS fp 0 bytes
           let !size' = max bufSize req
           S.mallocByteString size' >>= getBuffer writer' size'
        B.Chunk bs' writer' -> do
           if bytes > 0
             then do
               io $! S.PS fp 0 bytes
               unless (S.null bs') (io bs')
               S.mallocByteString bufSize >>= getBuffer writer' bufSize
             else do
               unless (S.null bs') (io bs')
               getBuffer writer' size fp


-- | Run a 'Builder' with the given buffer sizes.
--
-- Use this function for integrating the 'Builder' type with other libraries
-- that generate lazy bytestrings.
--
-- Note that the builders should guarantee that on average the desired chunk
-- size is attained. Builders may decide to start a new buffer and not
-- completely fill the existing buffer, if this is faster. However, they should
-- not spill too much of the buffer, if they cannot compensate for it.
--
-- FIXME: Note that the following paragraphs are not entirely correct as of
-- blaze-builder-0.4:
--
-- A call @toLazyByteStringWith bufSize minBufSize firstBufSize@ will generate
-- a lazy bytestring according to the following strategy. First, we allocate
-- a buffer of size @firstBufSize@ and start filling it. If it overflows, we
-- allocate a buffer of size @minBufSize@ and copy the first buffer to it in
-- order to avoid generating a too small chunk. Finally, every next buffer will
-- be of size @bufSize@. This, slow startup strategy is required to achieve
-- good speed for short (<200 bytes) resulting bytestrings, as for them the
-- allocation cost is of a large buffer cannot be compensated. Moreover, this
-- strategy also allows us to avoid spilling too much memory for short
-- resulting bytestrings.
--
-- Note that setting @firstBufSize >= minBufSize@ implies that the first buffer
-- is no longer copied but allocated and filled directly. Hence, setting
-- @firstBufSize = bufSize@ means that all chunks will use an underlying buffer
-- of size @bufSize@. This is recommended, if you know that you always output
-- more than @minBufSize@ bytes.
toLazyByteStringWith
    :: Int           -- ^ Buffer size (upper-bounds the resulting chunk size).
    -> Int           -- ^ This parameter is ignored as of blaze-builder-0.4
    -> Int           -- ^ Size of the first buffer to be used and copied for
                     -- larger resulting sequences
    -> Builder       -- ^ Builder to run.
    -> L.ByteString  -- ^ Lazy bytestring to output after the builder is
                     -- finished.
    -> L.ByteString  -- ^ Resulting lazy bytestring
toLazyByteStringWith bufSize _minBufSize firstBufSize builder k =
    B.toLazyByteStringWith (B.safeStrategy firstBufSize bufSize) k builder

-- | Run a 'Write' to produce a strict 'S.ByteString'.
-- This is equivalent to @('toByteString' . 'fromWrite')@, but is more
-- efficient because it uses just one appropriately-sized buffer.
writeToByteString :: W.Write -> S.ByteString
writeToByteString !w = unsafeDupablePerformIO $ do
    fptr <- S.mallocByteString (W.getBound w)
    len <- withForeignPtr fptr $ \ptr -> do
        end <- W.runWrite w ptr
        return $! end `minusPtr` ptr
    return $! S.fromForeignPtr fptr 0 len
{-# INLINE writeToByteString #-}
