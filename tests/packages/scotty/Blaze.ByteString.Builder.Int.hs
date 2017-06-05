{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Blaze/ByteString/Builder/Int.hs" #-}
------------------------------------------------------------------------------
-- |
-- Module:      Blaze.ByteString.Builder.Int
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- 'Write's and 'Builder's for serializing integers.
--
-- See "Blaze.ByteString.Builder.Word" for information about how to best write several
-- integers at once.
--
------------------------------------------------------------------------------

module Blaze.ByteString.Builder.Int
    (
    -- * Writing integers to a buffer

      writeInt8

    -- ** Big-endian writes
    , writeInt16be           -- :: Int16 -> Write
    , writeInt32be           -- :: Int32 -> Write
    , writeInt64be           -- :: Int64 -> Write

    -- ** Little-endian writes
    , writeInt16le           -- :: Int16 -> Write
    , writeInt32le           -- :: Int32 -> Write
    , writeInt64le           -- :: Int64 -> Write

    -- ** Host-endian writes
    , writeInthost           -- :: Int -> Write
    , writeInt16host         -- :: Int16 -> Write
    , writeInt32host         -- :: Int32 -> Write
    , writeInt64host         -- :: Int64 -> Write

    -- * Creating builders from integers

    -- | We provide serialization functions both for singleton integers as well as
    -- for lists of integers. Using these list serialization functions is /much/ faster
    -- than using @mconcat . map fromInt/<n/>@, as the list serialization
    -- functions use a tighter inner loop.

    , fromInt8
    , fromInt8s

    -- ** Big-endian serialization
    , fromInt16be            -- :: Int16   -> Builder
    , fromInt32be            -- :: Int32   -> Builder
    , fromInt64be            -- :: Int64   -> Builder
    , fromInt32sbe           -- :: [Int32] -> Builder
    , fromInt16sbe           -- :: [Int16] -> Builder
    , fromInt64sbe           -- :: [Int64] -> Builder

    -- ** Little-endian serialization
    , fromInt16le            -- :: Int16   -> Builder
    , fromInt32le            -- :: Int32   -> Builder
    , fromInt64le            -- :: Int64   -> Builder
    , fromInt16sle           -- :: [Int16] -> Builder
    , fromInt32sle           -- :: [Int32] -> Builder
    , fromInt64sle           -- :: [Int64] -> Builder

    -- ** Host-endian serialization
    , fromInthost            -- :: Int     -> Builder
    , fromInt16host          -- :: Int16   -> Builder
    , fromInt32host          -- :: Int32   -> Builder
    , fromInt64host          -- :: Int64   -> Builder
    , fromIntshost           -- :: [Int]   -> Builder
    , fromInt16shost         -- :: [Int16] -> Builder
    , fromInt32shost         -- :: [Int32] -> Builder
    , fromInt64shost         -- :: [Int64] -> Builder

    ) where

import Data.Int
import Blaze.ByteString.Builder.Compat.Write ( Write, writePrimFixed )
import Data.ByteString.Builder ( Builder )
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Builder.Extra as B
import qualified Data.ByteString.Builder.Prim  as P

-- | Write a single signed byte.
--
writeInt8      :: Int8 -> Write
writeInt8 = writePrimFixed P.int8
{-# INLINE writeInt8 #-}

-- | Write an 'Int16' in big endian format.
writeInt16be   :: Int16 -> Write
writeInt16be = writePrimFixed P.int16BE
{-# INLINE writeInt16be #-}

-- | Write an 'Int32' in big endian format.
writeInt32be   :: Int32 -> Write
writeInt32be = writePrimFixed P.int32BE
{-# INLINE writeInt32be #-}

-- | Write an 'Int64' in big endian format.
writeInt64be   :: Int64 -> Write
writeInt64be = writePrimFixed P.int64BE
{-# INLINE writeInt64be #-}

-- | Write an 'Int16' in little endian format.
writeInt16le   :: Int16 -> Write
writeInt16le = writePrimFixed P.int16LE
{-# INLINE writeInt16le #-}

-- | Write an 'Int32' in little endian format.
writeInt32le   :: Int32 -> Write
writeInt32le = writePrimFixed P.int32LE
{-# INLINE writeInt32le #-}

-- | Write an 'Int64' in little endian format.
writeInt64le   :: Int64 -> Write
writeInt64le = writePrimFixed P.int64LE
{-# INLINE writeInt64le #-}

-- | Write a single native machine 'Int'. The 'Int' is written in host order,
-- host endian form, for the machine you're on. On a 64 bit machine the 'Int'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values written this way
-- are not portable to different endian or integer sized machines, without
-- conversion.
--
writeInthost   :: Int -> Write
writeInthost = writePrimFixed P.intHost
{-# INLINE writeInthost #-}

-- | Write an 'Int16' in native host order and host endianness.
writeInt16host :: Int16 -> Write
writeInt16host = writePrimFixed P.int16Host
{-# INLINE writeInt16host #-}

-- | Write an 'Int32' in native host order and host endianness.
writeInt32host :: Int32 -> Write
writeInt32host = writePrimFixed P.int32Host
{-# INLINE writeInt32host #-}

-- | Write an 'Int64' in native host order and host endianness.
writeInt64host :: Int64 -> Write
writeInt64host = writePrimFixed P.int64Host
{-# INLINE writeInt64host #-}

-- | Serialize a single byte.
fromInt8       :: Int8    -> Builder
fromInt8 = B.int8
{-# INLINE fromInt8 #-}

-- | Serialize a list of bytes.
fromInt8s      :: [Int8]  -> Builder
fromInt8s = P.primMapListFixed P.int8
{-# INLINE fromInt8s #-}

-- | Serialize an 'Int16' in big endian format.
fromInt16be    :: Int16   -> Builder
fromInt16be = B.int16BE
{-# INLINE fromInt16be #-}

-- | Serialize an 'Int32' in big endian format.
fromInt32be    :: Int32   -> Builder
fromInt32be = B.int32BE
{-# INLINE fromInt32be #-}

-- | Serialize an 'Int64' in big endian format.
fromInt64be    :: Int64   -> Builder
fromInt64be = B.int64BE
{-# INLINE fromInt64be #-}

-- | Serialize a list of 'Int32's in big endian format.
fromInt32sbe   :: [Int32] -> Builder
fromInt32sbe = P.primMapListFixed P.int32BE
{-# INLINE fromInt32sbe #-}

-- | Serialize a list of 'Int16's in big endian format.
fromInt16sbe   :: [Int16] -> Builder
fromInt16sbe = P.primMapListFixed P.int16BE
{-# INLINE fromInt16sbe #-}

-- | Serialize a list of 'Int64's in big endian format.
fromInt64sbe   :: [Int64] -> Builder
fromInt64sbe = P.primMapListFixed P.int64BE
{-# INLINE fromInt64sbe #-}

-- | Serialize an 'Int16' in little endian format.
fromInt16le    :: Int16   -> Builder
fromInt16le = B.int16LE
{-# INLINE fromInt16le #-}

-- | Serialize an 'Int32' in little endian format.
fromInt32le    :: Int32   -> Builder
fromInt32le = B.int32LE
{-# INLINE fromInt32le #-}

-- | Serialize an 'Int64' in little endian format.
fromInt64le    :: Int64   -> Builder
fromInt64le = B.int64LE
{-# INLINE fromInt64le #-}

-- | Serialize a list of 'Int16's in little endian format.
fromInt16sle   :: [Int16] -> Builder
fromInt16sle = P.primMapListFixed P.int16LE
{-# INLINE fromInt16sle #-}

-- | Serialize a list of 'Int32's in little endian format.
fromInt32sle   :: [Int32] -> Builder
fromInt32sle = P.primMapListFixed P.int32LE
{-# INLINE fromInt32sle #-}

-- | Serialize a list of 'Int64's in little endian format.
fromInt64sle   :: [Int64] -> Builder
fromInt64sle = P.primMapListFixed P.int64LE
{-# INLINE fromInt64sle #-}

-- | Serialize a single native machine 'Int'. The 'Int' is serialized in host
-- order, host endian form, for the machine you're on. On a 64 bit machine the
-- 'Int' is an 8 byte value, on a 32 bit machine, 4 bytes. Values written this
-- way are not portable to different endian or integer sized machines, without
-- conversion.
--
fromInthost    :: Int     -> Builder
fromInthost = B.intHost
{-# INLINE fromInthost #-}

-- | Write an 'Int16' in native host order and host endianness.
fromInt16host  :: Int16   -> Builder
fromInt16host = B.int16Host
{-# INLINE fromInt16host #-}

-- | Write an 'Int32' in native host order and host endianness.
fromInt32host  :: Int32   -> Builder
fromInt32host = B.int32Host
{-# INLINE fromInt32host #-}

-- | Write an 'Int64' in native host order and host endianness.
fromInt64host  :: Int64   -> Builder
fromInt64host = B.int64Host
{-# INLINE fromInt64host #-}

-- | Serialize a list of 'Int's.
-- See 'fromInthost' for usage considerations.
fromIntshost   :: [Int]   -> Builder
fromIntshost = P.primMapListFixed P.intHost
{-# INLINE fromIntshost #-}

-- | Write a list of 'Int16's in native host order and host endianness.
fromInt16shost :: [Int16] -> Builder
fromInt16shost = P.primMapListFixed P.int16Host
{-# INLINE fromInt16shost #-}

-- | Write a list of 'Int32's in native host order and host endianness.
fromInt32shost :: [Int32] -> Builder
fromInt32shost = P.primMapListFixed P.int32Host
{-# INLINE fromInt32shost #-}

-- | Write a list of 'Int64's in native host order and host endianness.
fromInt64shost :: [Int64] -> Builder
fromInt64shost = P.primMapListFixed P.int64Host
{-# INLINE fromInt64shost #-}
