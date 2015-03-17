{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Data/Binary/Builder/Base.hs" #-}















































{-# LANGUAGE BangPatterns, CPP, MagicHash #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Builder.Base
-- Copyright   : Lennart Kolmodin, Ross Paterson
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Lennart Kolmodin <kolmodin@gmail.com>
-- Stability   : experimental
-- Portability : portable to Hugs and GHC
--
-- A module exporting types and functions that are shared by
-- 'Data.Binary.Builder' and 'Data.Binary.Builder.Internal'.
--
-----------------------------------------------------------------------------














































































































































































































































































































































module Data.Binary.Builder.Base (
    -- * The Builder type
      Builder
    , toLazyByteString

    -- * Constructing Builders
    , empty
    , singleton
    , append
    , fromByteString        -- :: S.ByteString -> Builder
    , fromLazyByteString    -- :: L.ByteString -> Builder

    -- * Flushing the buffer state
    , flush

    -- * Derived Builders
    -- ** Big-endian writes
    , putWord16be           -- :: Word16 -> Builder
    , putWord32be           -- :: Word32 -> Builder
    , putWord64be           -- :: Word64 -> Builder

    -- ** Little-endian writes
    , putWord16le           -- :: Word16 -> Builder
    , putWord32le           -- :: Word32 -> Builder
    , putWord64le           -- :: Word64 -> Builder

    -- ** Host-endian, unaligned writes
    , putWordhost           -- :: Word -> Builder
    , putWord16host         -- :: Word16 -> Builder
    , putWord32host         -- :: Word32 -> Builder
    , putWord64host         -- :: Word64 -> Builder

      -- ** Unicode
    , putCharUtf8

      -- * Low-level construction of Builders
    , writeN
    , writeAtMost
    ) where

import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Data.Word
import Foreign

import System.IO.Unsafe as IO ( unsafePerformIO )

import Data.ByteString.Internal (inlinePerformIO)
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy.Internal as L

import GHC.Base (ord,Int(..),uncheckedShiftRL#)
import GHC.Word (Word32(..),Word16(..),Word64(..))

------------------------------------------------------------------------

-- | A 'Builder' is an efficient way to build lazy 'L.ByteString's.
-- There are several functions for constructing 'Builder's, but only one
-- to inspect them: to extract any data, you have to turn them into lazy
-- 'L.ByteString's using 'toLazyByteString'.
--
-- Internally, a 'Builder' constructs a lazy 'L.Bytestring' by filling byte
-- arrays piece by piece.  As each buffer is filled, it is \'popped\'
-- off, to become a new chunk of the resulting lazy 'L.ByteString'.
-- All this is hidden from the user of the 'Builder'.

newtype Builder = Builder {
        runBuilder :: (Buffer -> IO L.ByteString)
                   -> Buffer
                   -> IO L.ByteString
    }

instance Monoid Builder where
    mempty  = empty
    {-# INLINE mempty #-}
    mappend = append
    {-# INLINE mappend #-}
    mconcat = foldr mappend mempty
    {-# INLINE mconcat #-}

------------------------------------------------------------------------

-- | /O(1)./ The empty Builder, satisfying
--
--  * @'toLazyByteString' 'empty' = 'L.empty'@
--
empty :: Builder
empty = Builder (\ k b -> k b)
{-# INLINE empty #-}

-- | /O(1)./ A Builder taking a single byte, satisfying
--
--  * @'toLazyByteString' ('singleton' b) = 'L.singleton' b@
--
singleton :: Word8 -> Builder
singleton = writeN 1 . flip poke
{-# INLINE singleton #-}

------------------------------------------------------------------------

-- | /O(1)./ The concatenation of two Builders, an associative operation
-- with identity 'empty', satisfying
--
--  * @'toLazyByteString' ('append' x y) = 'L.append' ('toLazyByteString' x) ('toLazyByteString' y)@
--
append :: Builder -> Builder -> Builder
append (Builder f) (Builder g) = Builder (f . g)
{-# INLINE [0] append #-}

-- | /O(1)./ A Builder taking a 'S.ByteString', satisfying
--
--  * @'toLazyByteString' ('fromByteString' bs) = 'L.fromChunks' [bs]@
--
fromByteString :: S.ByteString -> Builder
fromByteString bs
  | S.null bs = empty
  | otherwise = flush `append` mapBuilder (L.Chunk bs)
{-# INLINE fromByteString #-}

-- | /O(1)./ A Builder taking a lazy 'L.ByteString', satisfying
--
--  * @'toLazyByteString' ('fromLazyByteString' bs) = bs@
--
fromLazyByteString :: L.ByteString -> Builder
fromLazyByteString bss = flush `append` mapBuilder (bss `L.append`)
{-# INLINE fromLazyByteString #-}

------------------------------------------------------------------------

-- Our internal buffer type
data Buffer = Buffer {-# UNPACK #-} !(ForeignPtr Word8)
                     {-# UNPACK #-} !Int                -- offset
                     {-# UNPACK #-} !Int                -- used bytes
                     {-# UNPACK #-} !Int                -- length left

------------------------------------------------------------------------

-- | /O(n)./ Extract a lazy 'L.ByteString' from a 'Builder'.
-- The construction work takes place if and when the relevant part of
-- the lazy 'L.ByteString' is demanded.
--
toLazyByteString :: Builder -> L.ByteString
toLazyByteString m = IO.unsafePerformIO $ do
    buf <- newBuffer defaultSize
    runBuilder (m `append` flush) (const (return L.Empty)) buf
{-# INLINE toLazyByteString #-}

-- | /O(1)./ Pop the 'S.ByteString' we have constructed so far, if any,
-- yielding a new chunk in the result lazy 'L.ByteString'.
flush :: Builder
flush = Builder $ \ k buf@(Buffer p o u l) ->
    if u == 0  -- Invariant (from Data.ByteString.Lazy)
      then k buf
      else let !b  = Buffer p (o+u) 0 l
               !bs = S.PS p o u
           in return $! L.Chunk bs (inlinePerformIO (k b))

------------------------------------------------------------------------

--
-- copied from Data.ByteString.Lazy
--
defaultSize :: Int
defaultSize = 32 * k - overhead
    where k = 1024
          overhead = 2 * sizeOf (undefined :: Int)

------------------------------------------------------------------------

-- | Sequence an IO operation on the buffer
withBuffer :: (Buffer -> IO Buffer) -> Builder
withBuffer f = Builder $ \ k buf -> f buf >>= k
{-# INLINE withBuffer #-}

-- | Get the size of the buffer
withSize :: (Int -> Builder) -> Builder
withSize f = Builder $ \ k buf@(Buffer _ _ _ l) ->
    runBuilder (f l) k buf

-- | Map the resulting list of bytestrings.
mapBuilder :: (L.ByteString -> L.ByteString) -> Builder
mapBuilder f = Builder (fmap f .)

------------------------------------------------------------------------

-- | Ensure that there are at least @n@ many bytes available.
ensureFree :: Int -> Builder
ensureFree n = n `seq` withSize $ \ l ->
    if n <= l then empty else
        flush `append` withBuffer (const (newBuffer (max n defaultSize)))
{-# INLINE [0] ensureFree #-}

-- | Ensure that @n@ bytes are available, and then use @f@ to write at
-- most @n@ bytes into memory.  @f@ must return the actual number of
-- bytes written.
writeAtMost :: Int -> (Ptr Word8 -> IO Int) -> Builder
writeAtMost n f = ensureFree n `append` withBuffer (writeBuffer f)
{-# INLINE [0] writeAtMost #-}

-- | Ensure that @n@ bytes are available, and then use @f@ to write
-- exactly @n@ bytes into memory.
writeN :: Int -> (Ptr Word8 -> IO ()) -> Builder
writeN n f = writeAtMost n (\ p -> f p >> return n)
{-# INLINE writeN #-}

writeBuffer :: (Ptr Word8 -> IO Int) -> Buffer -> IO Buffer
writeBuffer f (Buffer fp o u l) = do
    n <- withForeignPtr fp (\p -> f (p `plusPtr` (o+u)))
    return $! Buffer fp o (u+n) (l-n)
{-# INLINE writeBuffer #-}

newBuffer :: Int -> IO Buffer
newBuffer size = do
    fp <- S.mallocByteString size
    return $! Buffer fp 0 0 size
{-# INLINE newBuffer #-}

------------------------------------------------------------------------

--
-- We rely on the fromIntegral to do the right masking for us.
-- The inlining here is critical, and can be worth 4x performance
--

-- | Write a Word16 in big endian format
putWord16be :: Word16 -> Builder
putWord16be w = writeN 2 $ \p -> do
    poke p               (fromIntegral (shiftr_w16 w 8) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (w)              :: Word8)
{-# INLINE putWord16be #-}

-- | Write a Word16 in little endian format
putWord16le :: Word16 -> Builder
putWord16le w = writeN 2 $ \p -> do
    poke p               (fromIntegral (w)              :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w16 w 8) :: Word8)
{-# INLINE putWord16le #-}

-- putWord16le w16 = writeN 2 (\p -> poke (castPtr p) w16)

-- | Write a Word32 in big endian format
putWord32be :: Word32 -> Builder
putWord32be w = writeN 4 $ \p -> do
    poke p               (fromIntegral (shiftr_w32 w 24) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (w)               :: Word8)
{-# INLINE putWord32be #-}

--
-- a data type to tag Put/Check. writes construct these which are then
-- inlined and flattened. matching Checks will be more robust with rules.
--

-- | Write a Word32 in little endian format
putWord32le :: Word32 -> Builder
putWord32le w = writeN 4 $ \p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 w 24) :: Word8)
{-# INLINE putWord32le #-}

-- on a little endian machine:
-- putWord32le w32 = writeN 4 (\p -> poke (castPtr p) w32)

-- | Write a Word64 in big endian format
putWord64be :: Word64 -> Builder
putWord64be w = writeN 8 $ \p -> do
    poke p               (fromIntegral (shiftr_w64 w 56) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (w)               :: Word8)
{-# INLINE putWord64be #-}

-- | Write a Word64 in little endian format
putWord64le :: Word64 -> Builder

putWord64le w = writeN 8 $ \p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (shiftr_w64 w 56) :: Word8)
{-# INLINE putWord64le #-}

-- on a little endian machine:
-- putWord64le w64 = writeN 8 (\p -> poke (castPtr p) w64)

------------------------------------------------------------------------
-- Unaligned, word size ops

-- | /O(1)./ A Builder taking a single native machine word. The word is
-- written in host order, host endian form, for the machine you're on.
-- On a 64 bit machine the Word is an 8 byte value, on a 32 bit machine,
-- 4 bytes. Values written this way are not portable to
-- different endian or word sized machines, without conversion.
--
putWordhost :: Word -> Builder
putWordhost w =
    writeN (sizeOf (undefined :: Word)) (\p -> poke (castPtr p) w)
{-# INLINE putWordhost #-}

-- | Write a Word16 in native host order and host endianness.
-- 2 bytes will be written, unaligned.
putWord16host :: Word16 -> Builder
putWord16host w16 =
    writeN (sizeOf (undefined :: Word16)) (\p -> poke (castPtr p) w16)
{-# INLINE putWord16host #-}

-- | Write a Word32 in native host order and host endianness.
-- 4 bytes will be written, unaligned.
putWord32host :: Word32 -> Builder
putWord32host w32 =
    writeN (sizeOf (undefined :: Word32)) (\p -> poke (castPtr p) w32)
{-# INLINE putWord32host #-}

-- | Write a Word64 in native host order.
-- On a 32 bit machine we write two host order Word32s, in big endian form.
-- 8 bytes will be written, unaligned.
putWord64host :: Word64 -> Builder
putWord64host w =
    writeN (sizeOf (undefined :: Word64)) (\p -> poke (castPtr p) w)
{-# INLINE putWord64host #-}

------------------------------------------------------------------------
-- Unicode

-- Code lifted from the text package by Bryan O'Sullivan.

-- | Write a character using UTF-8 encoding.
putCharUtf8 :: Char -> Builder
putCharUtf8 x = writeAtMost 4 $ \ p -> case undefined of
    _ | n <= 0x7F   -> poke p c >> return 1
      | n <= 0x07FF -> do
          poke p a2
          poke (p `plusPtr` 1) b2
          return 2
      | n <= 0xFFFF -> do
          poke p a3
          poke (p `plusPtr` 1) b3
          poke (p `plusPtr` 2) c3
          return 3
      | otherwise   -> do
          poke p a4
          poke (p `plusPtr` 1) b4
          poke (p `plusPtr` 2) c4
          poke (p `plusPtr` 3) d4
          return 4
  where
      n = ord x
      c = fromIntegral n
      (a2,b2) = ord2 x
      (a3,b3,c3) = ord3 x
      (a4,b4,c4,d4) = ord4 x

ord2 :: Char -> (Word8,Word8)
ord2 c = (x1,x2)
  where
    n = ord c
    x1 = fromIntegral $ (n `shiftR` 6) + 0xC0
    x2 = fromIntegral $ (n .&. 0x3F) + 0x80

ord3 :: Char -> (Word8,Word8,Word8)
ord3 c = (x1,x2,x3)
  where
    n = ord c
    x1 = fromIntegral $ (n `shiftR` 12) + 0xE0
    x2 = fromIntegral $ ((n `shiftR` 6) .&. 0x3F) + 0x80
    x3 = fromIntegral $ (n .&. 0x3F) + 0x80

ord4 :: Char -> (Word8,Word8,Word8,Word8)
ord4 c = (x1,x2,x3,x4)
  where
    n = ord c
    x1 = fromIntegral $ (n `shiftR` 18) + 0xF0
    x2 = fromIntegral $ ((n `shiftR` 12) .&. 0x3F) + 0x80
    x3 = fromIntegral $ ((n `shiftR` 6) .&. 0x3F) + 0x80
    x4 = fromIntegral $ (n .&. 0x3F) + 0x80

------------------------------------------------------------------------
-- Unchecked shifts

{-# INLINE shiftr_w16 #-}
shiftr_w16 :: Word16 -> Int -> Word16
{-# INLINE shiftr_w32 #-}
shiftr_w32 :: Word32 -> Int -> Word32
{-# INLINE shiftr_w64 #-}
shiftr_w64 :: Word64 -> Int -> Word64

shiftr_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftRL#`   i)
shiftr_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftRL#`   i)

shiftr_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftRL#` i)


------------------------------------------------------------------------
-- Some nice rules for Builder

-- In versions of GHC prior to 7.0 these rules would make GHC believe
-- that 'writeN' and 'ensureFree' are recursive and the rules wouldn't
-- fire.
{-# RULES

"append/writeAtMost" forall a b (f::Ptr Word8 -> IO Int)
                                (g::Ptr Word8 -> IO Int) ws.
    append (writeAtMost a f) (append (writeAtMost b g) ws) =
        append (writeAtMost (a+b) (\p -> f p >>= \n ->
                                    g (p `plusPtr` n) >>= \m ->
                                    let s = n+m in s `seq` return s)) ws

"writeAtMost/writeAtMost" forall a b (f::Ptr Word8 -> IO Int)
                                     (g::Ptr Word8 -> IO Int).
    append (writeAtMost a f) (writeAtMost b g) =
        writeAtMost (a+b) (\p -> f p >>= \n ->
                            g (p `plusPtr` n) >>= \m ->
                            let s = n+m in s `seq` return s)

"ensureFree/ensureFree" forall a b .
    append (ensureFree a) (ensureFree b) = ensureFree (max a b)

"flush/flush"
    append flush flush = flush #-}
