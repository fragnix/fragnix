{-# LINE 1 "Data/Hashable/Class.hs" #-}
# 1 "Data/Hashable/Class.hs"
# 1 "<command-line>"
# 9 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 9 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1











































































































# 9 "<command-line>" 2
# 1 "Data/Hashable/Class.hs"
{-# LANGUAGE BangPatterns, CPP, ForeignFunctionInterface, MagicHash,
             ScopedTypeVariables, UnliftedFFITypes #-}

{-# LANGUAGE DefaultSignatures, FlexibleContexts #-}


------------------------------------------------------------------------
-- |
-- Module      :  Data.Hashable.Class
-- Copyright   :  (c) Milan Straka 2010
--                (c) Johan Tibell 2011
--                (c) Bryan O'Sullivan 2011, 2012
-- License     :  BSD-style
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module defines a class, 'Hashable', for types that can be
-- converted to a hash value.  This class exists for the benefit of
-- hashing-based data structures.  The module provides instances for
-- most standard types.

module Data.Hashable.Class
    (
      -- * Computing hash values
      Hashable(..)

      -- ** Support for generics
    , GHashable(..)


      -- * Creating new instances
    , hashUsing
    , hashPtr
    , hashPtrWithSalt
    , hashByteArray
    , hashByteArrayWithSalt
    ) where

import Control.Exception (assert)
import Data.Bits (bitSize, shiftL, shiftR, xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as B
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List (foldl')
import Data.Ratio (Ratio, denominator, numerator)
import qualified Data.Text as T
import qualified Data.Text.Array as TA
import qualified Data.Text.Internal as T
import qualified Data.Text.Lazy as TL
import Data.Typeable
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Foreign.C (CString)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (alignment, peek, sizeOf)
import GHC.Base (ByteArray#)
import GHC.Conc (ThreadId(..))
import GHC.Prim (ThreadId#)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName


import GHC.Generics



import Data.Typeable.Internal(TypeRep(..))
import GHC.Fingerprint.Type(Fingerprint(..))



import Foreign.C (CLong(..))
import Foreign.C.Types (CInt(..))










import qualified Data.ByteString.Short.Internal as BSI



import GHC.Exts (Int(..))
import GHC.Integer.GMP.Internals (Integer(..))













# 1 "/usr/local/lib/ghc-7.8.3/include/MachDeps.h" 1

# 15 "/usr/local/lib/ghc-7.8.3/include/MachDeps.h"






# 1 "/usr/local/lib/ghc-7.8.3/include/ghcautoconf.h" 1





































































































































































































































































































































































































































# 21 "/usr/local/lib/ghc-7.8.3/include/MachDeps.h" 2

































































# 99 "/usr/local/lib/ghc-7.8.3/include/MachDeps.h"

# 109 "/usr/local/lib/ghc-7.8.3/include/MachDeps.h"











# 106 "Data/Hashable/Class.hs" 2

infixl 0 `hashWithSalt`

------------------------------------------------------------------------
-- * Computing hash values

-- | A default salt used in the implementation of 'hash'.
defaultSalt :: Int

defaultSalt = -2578643520546668380  -- 0xdc36d1615b7400a4



{-# INLINE defaultSalt #-}

-- | The class of types that can be converted to a hash value.
--
-- Minimal implementation: 'hashWithSalt'.
class Hashable a where
    -- | Return a hash value for the argument, using the given salt.
    --
    -- The general contract of 'hashWithSalt' is:
    --
    --  * If two values are equal according to the '==' method, then
    --    applying the 'hashWithSalt' method on each of the two values
    --    /must/ produce the same integer result if the same salt is
    --    used in each case.
    --
    --  * It is /not/ required that if two values are unequal
    --    according to the '==' method, then applying the
    --    'hashWithSalt' method on each of the two values must produce
    --    distinct integer results. However, the programmer should be
    --    aware that producing distinct integer results for unequal
    --    values may improve the performance of hashing-based data
    --    structures.
    --
    --  * This method can be used to compute different hash values for
    --    the same input by providing a different salt in each
    --    application of the method. This implies that any instance
    --    that defines 'hashWithSalt' /must/ make use of the salt in
    --    its implementation.
    hashWithSalt :: Int -> a -> Int

    -- | Like 'hashWithSalt', but no salt is used. The default
    -- implementation uses 'hashWithSalt' with some default salt.
    -- Instances might want to implement this method to provide a more
    -- efficient implementation than the default implementation.
    hash :: a -> Int
    hash = hashWithSalt defaultSalt


    default hashWithSalt :: (Generic a, GHashable (Rep a)) => Int -> a -> Int
    hashWithSalt salt = ghashWithSalt salt . from

-- | The class of types that can be generically hashed.
class GHashable f where
    ghashWithSalt :: Int -> f a -> Int


-- Since we support a generic implementation of 'hashWithSalt' we
-- cannot also provide a default implementation for that method for
-- the non-generic instance use case. Instead we provide
-- 'defaultHashWith'.

defaultHashWithSalt :: Hashable a => Int -> a -> Int
defaultHashWithSalt salt x = salt `combine` hash x

-- | Transform a value into a 'Hashable' value, then hash the
-- transformed value using the given salt.
--
-- This is a useful shorthand in cases where a type can easily be
-- mapped to another type that is already an instance of 'Hashable'.
-- Example:
--
-- > data Foo = Foo | Bar
-- >          deriving (Enum)
-- >
-- > instance Hashable Foo where
-- >     hashWithSalt = hashUsing fromEnum
hashUsing :: (Hashable b) =>
             (a -> b)           -- ^ Transformation function.
          -> Int                -- ^ Salt.
          -> a                  -- ^ Value to transform.
          -> Int
hashUsing f salt x = hashWithSalt salt (f x)
{-# INLINE hashUsing #-}

instance Hashable Int where
    hash = id
    hashWithSalt = defaultHashWithSalt

instance Hashable Int8 where
    hash = fromIntegral
    hashWithSalt = defaultHashWithSalt

instance Hashable Int16 where
    hash = fromIntegral
    hashWithSalt = defaultHashWithSalt

instance Hashable Int32 where
    hash = fromIntegral
    hashWithSalt = defaultHashWithSalt

instance Hashable Int64 where
    hash n
        | bitSize (undefined :: Int) == 64 = fromIntegral n
        | otherwise = fromIntegral (fromIntegral n `xor`
                                   (fromIntegral n `shiftR` 32 :: Word64))
    hashWithSalt = defaultHashWithSalt

instance Hashable Word where
    hash = fromIntegral
    hashWithSalt = defaultHashWithSalt

instance Hashable Word8 where
    hash = fromIntegral
    hashWithSalt = defaultHashWithSalt

instance Hashable Word16 where
    hash = fromIntegral
    hashWithSalt = defaultHashWithSalt

instance Hashable Word32 where
    hash = fromIntegral
    hashWithSalt = defaultHashWithSalt

instance Hashable Word64 where
    hash n
        | bitSize (undefined :: Int) == 64 = fromIntegral n
        | otherwise = fromIntegral (n `xor` (n `shiftR` 32))
    hashWithSalt = defaultHashWithSalt

instance Hashable () where
    hash = fromEnum
    hashWithSalt = defaultHashWithSalt

instance Hashable Bool where
    hash = fromEnum
    hashWithSalt = defaultHashWithSalt

instance Hashable Ordering where
    hash = fromEnum
    hashWithSalt = defaultHashWithSalt

instance Hashable Char where
    hash = fromEnum
    hashWithSalt = defaultHashWithSalt

# 264 "Data/Hashable/Class.hs"

# 279 "Data/Hashable/Class.hs"

instance Hashable Integer where
# 291 "Data/Hashable/Class.hs"
    hash (S# int) = I# int
    hash n@(J# size# byteArray)
        | n >= minInt && n <= maxInt = fromInteger n :: Int
        | otherwise = let size = I# size#
                          numBytes = 8 * abs size
                      in hashByteArrayWithSalt byteArray 0 numBytes defaultSalt
                         `hashWithSalt` size
      where minInt = fromIntegral (minBound :: Int)
            maxInt = fromIntegral (maxBound :: Int)

    hashWithSalt salt (S# n) = hashWithSalt salt (I# n)
    hashWithSalt salt n@(J# size# byteArray)
        | n >= minInt && n <= maxInt = hashWithSalt salt (fromInteger n :: Int)
        | otherwise = let size = I# size#
                          numBytes = 8 * abs size
                      in hashByteArrayWithSalt byteArray 0 numBytes salt
                         `hashWithSalt` size
      where minInt = fromIntegral (minBound :: Int)
            maxInt = fromIntegral (maxBound :: Int)
# 319 "Data/Hashable/Class.hs"

instance (Integral a, Hashable a) => Hashable (Ratio a) where
    {-# SPECIALIZE instance Hashable (Ratio Integer) #-}
    hash a = hash (numerator a) `hashWithSalt` denominator a
    hashWithSalt s a = s `hashWithSalt` numerator a `hashWithSalt` denominator a

instance Hashable Float where
    hash x
        | isIEEE x =
            assert (sizeOf x >= sizeOf (0::Word32) &&
                    alignment x >= alignment (0::Word32)) $
            hash ((unsafePerformIO $ with x $ peek . castPtr) :: Word32)
        | otherwise = hash (show x)
    hashWithSalt = defaultHashWithSalt

instance Hashable Double where
    hash x
        | isIEEE x =
            assert (sizeOf x >= sizeOf (0::Word64) &&
                    alignment x >= alignment (0::Word64)) $
            hash ((unsafePerformIO $ with x $ peek . castPtr) :: Word64)
        | otherwise = hash (show x)
    hashWithSalt = defaultHashWithSalt

-- | A value with bit pattern (01)* (or 5* in hexa), for any size of Int.
-- It is used as data constructor distinguisher. GHC computes its value during
-- compilation.
distinguisher :: Int
distinguisher = fromIntegral $ (maxBound :: Word) `quot` 3
{-# INLINE distinguisher #-}

instance Hashable a => Hashable (Maybe a) where
    hash Nothing = 0
    hash (Just a) = distinguisher `hashWithSalt` a
    hashWithSalt s Nothing = s `combine` 0
    hashWithSalt s (Just a) = s `combine` distinguisher `hashWithSalt` a

instance (Hashable a, Hashable b) => Hashable (Either a b) where
    hash (Left a)  = 0 `hashWithSalt` a
    hash (Right b) = distinguisher `hashWithSalt` b
    hashWithSalt s (Left a)  = s `combine` 0 `hashWithSalt` a
    hashWithSalt s (Right b) = s `combine` distinguisher `hashWithSalt` b

instance (Hashable a1, Hashable a2) => Hashable (a1, a2) where
    hash (a1, a2) = hash a1 `hashWithSalt` a2
    hashWithSalt s (a1, a2) = s `hashWithSalt` a1 `hashWithSalt` a2

instance (Hashable a1, Hashable a2, Hashable a3) => Hashable (a1, a2, a3) where
    hash (a1, a2, a3) = hash a1 `hashWithSalt` a2 `hashWithSalt` a3
    hashWithSalt s (a1, a2, a3) = s `hashWithSalt` a1 `hashWithSalt` a2
                        `hashWithSalt` a3

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4) =>
         Hashable (a1, a2, a3, a4) where
    hash (a1, a2, a3, a4) = hash a1 `hashWithSalt` a2
                            `hashWithSalt` a3 `hashWithSalt` a4
    hashWithSalt s (a1, a2, a3, a4) = s `hashWithSalt` a1 `hashWithSalt` a2
                            `hashWithSalt` a3 `hashWithSalt` a4

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5)
      => Hashable (a1, a2, a3, a4, a5) where
    hash (a1, a2, a3, a4, a5) =
        hash a1 `hashWithSalt` a2 `hashWithSalt` a3
        `hashWithSalt` a4 `hashWithSalt` a5
    hashWithSalt s (a1, a2, a3, a4, a5) =
        s `hashWithSalt` a1 `hashWithSalt` a2 `hashWithSalt` a3
        `hashWithSalt` a4 `hashWithSalt` a5

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5,
          Hashable a6) => Hashable (a1, a2, a3, a4, a5, a6) where
    hash (a1, a2, a3, a4, a5, a6) =
        hash a1 `hashWithSalt` a2 `hashWithSalt` a3
        `hashWithSalt` a4 `hashWithSalt` a5 `hashWithSalt` a6
    hashWithSalt s (a1, a2, a3, a4, a5, a6) =
        s `hashWithSalt` a1 `hashWithSalt` a2 `hashWithSalt` a3
        `hashWithSalt` a4 `hashWithSalt` a5 `hashWithSalt` a6

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5,
          Hashable a6, Hashable a7) =>
         Hashable (a1, a2, a3, a4, a5, a6, a7) where
    hash (a1, a2, a3, a4, a5, a6, a7) =
        hash a1 `hashWithSalt` a2 `hashWithSalt` a3
        `hashWithSalt` a4 `hashWithSalt` a5 `hashWithSalt` a6 `hashWithSalt` a7
    hashWithSalt s (a1, a2, a3, a4, a5, a6, a7) =
        s `hashWithSalt` a1 `hashWithSalt` a2 `hashWithSalt` a3
        `hashWithSalt` a4 `hashWithSalt` a5 `hashWithSalt` a6 `hashWithSalt` a7

instance Hashable (StableName a) where
    hash = hashStableName
    hashWithSalt = defaultHashWithSalt

instance Hashable a => Hashable [a] where
    {-# SPECIALIZE instance Hashable [Char] #-}
    hashWithSalt = foldl' hashWithSalt

instance Hashable B.ByteString where
    hashWithSalt salt bs = B.inlinePerformIO $
                           B.unsafeUseAsCStringLen bs $ \(p, len) ->
                           hashPtrWithSalt p (fromIntegral len) salt

instance Hashable BL.ByteString where
    hashWithSalt = BL.foldlChunks hashWithSalt


instance Hashable BSI.ShortByteString where

    hashWithSalt salt sbs@(BSI.SBS ba) =



        hashByteArrayWithSalt ba 0 (BSI.length sbs) salt


instance Hashable T.Text where
    hashWithSalt salt (T.Text arr off len) =
        hashByteArrayWithSalt (TA.aBA arr) (off `shiftL` 1) (len `shiftL` 1)
        salt

instance Hashable TL.Text where
    hashWithSalt = TL.foldlChunks hashWithSalt

-- | Compute the hash of a ThreadId.
hashThreadId :: ThreadId -> Int
hashThreadId (ThreadId t) = hash (fromIntegral (getThreadId t) :: Int)

foreign import ccall unsafe "rts_getThreadId" getThreadId
    :: ThreadId# -> CInt

instance Hashable ThreadId where
    hash = hashThreadId
    hashWithSalt = defaultHashWithSalt

-- | Compute the hash of a TypeRep, in various GHC versions we can do this quickly.
hashTypeRep :: TypeRep -> Int
{-# INLINE hashTypeRep #-}

-- Fingerprint is just the MD5, so taking any Int from it is fine
hashTypeRep (TypeRep (Fingerprint x _) _ _) = fromIntegral x






instance Hashable TypeRep where
    hash = hashTypeRep
    hashWithSalt = defaultHashWithSalt
    {-# INLINE hash #-}






-- | Compute a hash value for the content of this pointer.
hashPtr :: Ptr a      -- ^ pointer to the data to hash
        -> Int        -- ^ length, in bytes
        -> IO Int     -- ^ hash value
hashPtr p len = hashPtrWithSalt p len defaultSalt

-- | Compute a hash value for the content of this pointer, using an
-- initial salt.
--
-- This function can for example be used to hash non-contiguous
-- segments of memory as if they were one contiguous segment, by using
-- the output of one hash as the salt for the next.
hashPtrWithSalt :: Ptr a   -- ^ pointer to the data to hash
                -> Int     -- ^ length, in bytes
                -> Int     -- ^ salt
                -> IO Int  -- ^ hash value
hashPtrWithSalt p len salt =
    fromIntegral `fmap` c_hashCString (castPtr p) (fromIntegral len)
    (fromIntegral salt)

foreign import ccall unsafe "hashable_fnv_hash" c_hashCString
    :: CString -> CLong -> CLong -> IO CLong

-- | Compute a hash value for the content of this 'ByteArray#',
-- beginning at the specified offset, using specified number of bytes.
hashByteArray :: ByteArray#  -- ^ data to hash
              -> Int         -- ^ offset, in bytes
              -> Int         -- ^ length, in bytes
              -> Int         -- ^ hash value
hashByteArray ba0 off len = hashByteArrayWithSalt ba0 off len defaultSalt
{-# INLINE hashByteArray #-}

-- | Compute a hash value for the content of this 'ByteArray#', using
-- an initial salt.
--
-- This function can for example be used to hash non-contiguous
-- segments of memory as if they were one contiguous segment, by using
-- the output of one hash as the salt for the next.
hashByteArrayWithSalt
    :: ByteArray#  -- ^ data to hash
    -> Int         -- ^ offset, in bytes
    -> Int         -- ^ length, in bytes
    -> Int         -- ^ salt
    -> Int         -- ^ hash value
hashByteArrayWithSalt ba !off !len !h =
    fromIntegral $ c_hashByteArray ba (fromIntegral off) (fromIntegral len)
    (fromIntegral h)

foreign import ccall unsafe "hashable_fnv_hash_offset" c_hashByteArray
    :: ByteArray# -> CLong -> CLong -> CLong -> CLong

-- | Combine two given hash values.  'combine' has zero as a left
-- identity.
combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2
