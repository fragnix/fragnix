{-# LINE 1 "System/ByteOrder.hs" #-}
{-

Module : System.ByteOrder
Copyright : (c) Antoine Latter 2009
License : BSD3

Maintainer : Antoine Latter <aslatter@gmail.com>

-}

{-# OPTIONS_GHC -fno-cse #-}

module System.ByteOrder(byteOrder, ByteOrder(..)) where

import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (castPtr)
import Foreign.Storable (poke)
import Data.Word
import System.IO.Unsafe (unsafePerformIO)

-- |Indicates the byte-ordering for a 4-byte value, where '1'
-- indicates the most-significant byte and '4' indicates the
-- least significant byte.
--
-- In this format, big endian byte order would be represented as:
-- (1,2,3,4).
--
-- For convinience, the most common cases (BigEndian and LittleEndian)
-- are provided their own constructors.
data ByteOrder
    = BigEndian
    | LittleEndian
    | Mixed (Word8, Word8, Word8, Word8)
 deriving (Eq, Show, Read, Ord)

input :: Word32
input = 0x01020304

-- |Returns the native byte ordering of the system.
byteOrder :: ByteOrder
byteOrder = unsafePerformIO byteOrderIO
{-# NOINLINE byteOrder #-}

byteOrderIO :: IO ByteOrder
byteOrderIO = bytesToByteOrder `fmap` wordToBytes input

wordToBytes :: Word32 -> IO (Word8,Word8,Word8,Word8)
wordToBytes word = alloca $ \wordPtr -> do
         poke wordPtr word
         [x1,x2,x3,x4] <- peekArray 4 (castPtr wordPtr)
         return (x1,x2,x3,x4)

bytesToByteOrder :: (Word8,Word8,Word8,Word8) -> ByteOrder
bytesToByteOrder (1, 2, 3, 4)     = BigEndian
bytesToByteOrder (4, 3, 2, 1)     = LittleEndian
bytesToByteOrder (x1, x2, x3, x4) = Mixed (x1,x2,x3,x4)

