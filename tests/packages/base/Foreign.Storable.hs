{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Foreign/Storable.hs" #-}













































{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Storable
-- Copyright   :  (c) The FFI task force 2001
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The module "Foreign.Storable" provides most elementary support for
-- marshalling and is part of the language-independent portion of the
-- Foreign Function Interface (FFI), and will normally be imported via
-- the "Foreign" module.
--
-----------------------------------------------------------------------------

module Foreign.Storable
        ( Storable(
             sizeOf,
             alignment,
             peekElemOff,
             pokeElemOff,
             peekByteOff,
             pokeByteOff,
             peek,
             poke)
        ) where


import Control.Monad            ( liftM )





























































































































































































































































































































































































































































































































































































































































































































































































import GHC.Storable
import GHC.Stable       ( StablePtr )
import GHC.Num
import GHC.Int
import GHC.Word
import GHC.Ptr
import GHC.Base
import GHC.Fingerprint.Type
import Data.Bits
import GHC.Real

{- |
The member functions of this class facilitate writing values of
primitive types to raw memory (which may have been allocated with the
above mentioned routines) and reading values from blocks of raw
memory.  The class, furthermore, includes support for computing the
storage requirements and alignment restrictions of storable types.

Memory addresses are represented as values of type @'Ptr' a@, for some
@a@ which is an instance of class 'Storable'.  The type argument to
'Ptr' helps provide some valuable type safety in FFI code (you can\'t
mix pointers of different types without an explicit cast), while
helping the Haskell type system figure out which marshalling method is
needed for a given pointer.

All marshalling between Haskell and a foreign language ultimately
boils down to translating Haskell data structures into the binary
representation of a corresponding data structure of the foreign
language and vice versa.  To code this marshalling in Haskell, it is
necessary to manipulate primitive data types stored in unstructured
memory blocks.  The class 'Storable' facilitates this manipulation on
all types for which it is instantiated, which are the standard basic
types of Haskell, the fixed size @Int@ types ('Int8', 'Int16',
'Int32', 'Int64'), the fixed size @Word@ types ('Word8', 'Word16',
'Word32', 'Word64'), 'StablePtr', all types from "Foreign.C.Types",
as well as 'Ptr'.

Minimal complete definition: 'sizeOf', 'alignment', one of 'peek',
'peekElemOff' and 'peekByteOff', and one of 'poke', 'pokeElemOff' and
'pokeByteOff'.
-}

class Storable a where

   sizeOf      :: a -> Int
   -- ^ Computes the storage requirements (in bytes) of the argument.
   -- The value of the argument is not used.

   alignment   :: a -> Int
   -- ^ Computes the alignment constraint of the argument.  An
   -- alignment constraint @x@ is fulfilled by any address divisible
   -- by @x@.  The value of the argument is not used.

   peekElemOff :: Ptr a -> Int      -> IO a
   -- ^       Read a value from a memory area regarded as an array
   --         of values of the same kind.  The first argument specifies
   --         the start address of the array and the second the index into
   --         the array (the first element of the array has index
   --         @0@).  The following equality holds,
   -- 
   -- > peekElemOff addr idx = IOExts.fixIO $ \result ->
   -- >   peek (addr `plusPtr` (idx * sizeOf result))
   --
   --         Note that this is only a specification, not
   --         necessarily the concrete implementation of the
   --         function.

   pokeElemOff :: Ptr a -> Int -> a -> IO ()
   -- ^       Write a value to a memory area regarded as an array of
   --         values of the same kind.  The following equality holds:
   -- 
   -- > pokeElemOff addr idx x = 
   -- >   poke (addr `plusPtr` (idx * sizeOf x)) x

   peekByteOff :: Ptr b -> Int      -> IO a
   -- ^       Read a value from a memory location given by a base
   --         address and offset.  The following equality holds:
   --
   -- > peekByteOff addr off = peek (addr `plusPtr` off)

   pokeByteOff :: Ptr b -> Int -> a -> IO ()
   -- ^       Write a value to a memory location given by a base
   --         address and offset.  The following equality holds:
   --
   -- > pokeByteOff addr off x = poke (addr `plusPtr` off) x
  
   peek        :: Ptr a      -> IO a
   -- ^ Read a value from the given memory location.
   --
   --  Note that the peek and poke functions might require properly
   --  aligned addresses to function correctly.  This is architecture
   --  dependent; thus, portable code should ensure that when peeking or
   --  poking values of some type @a@, the alignment
   --  constraint for @a@, as given by the function
   --  'alignment' is fulfilled.

   poke        :: Ptr a -> a -> IO ()
   -- ^ Write the given value to the given memory location.  Alignment
   -- restrictions might apply; see 'peek'.
 
   -- circular default instances
   peekElemOff = peekElemOff_ undefined
      where peekElemOff_ :: a -> Ptr a -> Int -> IO a
            peekElemOff_ undef ptr off = peekByteOff ptr (off * sizeOf undef)
   pokeElemOff ptr off val = pokeByteOff ptr (off * sizeOf val) val

   peekByteOff ptr off = peek (ptr `plusPtr` off)
   pokeByteOff ptr off = poke (ptr `plusPtr` off)

   peek ptr = peekElemOff ptr 0
   poke ptr = pokeElemOff ptr 0

   {-# MINIMAL sizeOf, alignment,
               (peek | peekElemOff | peekByteOff),
               (poke | pokeElemOff | pokeByteOff) #-}

-- System-dependent, but rather obvious instances

instance Storable Bool where
   sizeOf _          = sizeOf (undefined::Int32)
   alignment _       = alignment (undefined::Int32)
   peekElemOff p i   = liftM (/= (0::Int32)) $ peekElemOff (castPtr p) i
   pokeElemOff p i x = pokeElemOff (castPtr p) i (if x then 1 else 0::Int32)


instance Storable (Char) where {                       sizeOf    _ = 4;                             alignment _ = 4;                            peekElemOff =           readWideCharOffPtr;                             pokeElemOff = writeWideCharOffPtr }

instance Storable (Int) where {                       sizeOf    _ = 8;                             alignment _ = 8;                            peekElemOff =           readIntOffPtr;                             pokeElemOff = writeIntOffPtr }

instance Storable (Word) where {                       sizeOf    _ = 8;                             alignment _ = 8;                            peekElemOff =           readWordOffPtr;                             pokeElemOff = writeWordOffPtr }

instance Storable ((Ptr a)) where {                       sizeOf    _ = 8;                             alignment _ = 8;                            peekElemOff =           readPtrOffPtr;                             pokeElemOff = writePtrOffPtr }

instance Storable ((FunPtr a)) where {                       sizeOf    _ = 8;                             alignment _ = 8;                            peekElemOff =           readFunPtrOffPtr;                             pokeElemOff = writeFunPtrOffPtr }

instance Storable ((StablePtr a)) where {                       sizeOf    _ = 8;                             alignment _ = 8;                            peekElemOff =           readStablePtrOffPtr;                             pokeElemOff = writeStablePtrOffPtr }

instance Storable (Float) where {                       sizeOf    _ = 4;                             alignment _ = 4;                            peekElemOff =           readFloatOffPtr;                             pokeElemOff = writeFloatOffPtr }

instance Storable (Double) where {                       sizeOf    _ = 8;                             alignment _ = 8;                            peekElemOff =           readDoubleOffPtr;                             pokeElemOff = writeDoubleOffPtr }

instance Storable (Word8) where {                       sizeOf    _ = 1;                             alignment _ = 1;                            peekElemOff =           readWord8OffPtr;                             pokeElemOff = writeWord8OffPtr }

instance Storable (Word16) where {                       sizeOf    _ = 2;                             alignment _ = 2;                            peekElemOff =           readWord16OffPtr;                             pokeElemOff = writeWord16OffPtr }

instance Storable (Word32) where {                       sizeOf    _ = 4;                             alignment _ = 4;                            peekElemOff =           readWord32OffPtr;                             pokeElemOff = writeWord32OffPtr }

instance Storable (Word64) where {                       sizeOf    _ = 8;                             alignment _ = 8;                            peekElemOff =           readWord64OffPtr;                             pokeElemOff = writeWord64OffPtr }

instance Storable (Int8) where {                       sizeOf    _ = 1;                             alignment _ = 1;                            peekElemOff =           readInt8OffPtr;                             pokeElemOff = writeInt8OffPtr }

instance Storable (Int16) where {                       sizeOf    _ = 2;                             alignment _ = 2;                            peekElemOff =           readInt16OffPtr;                             pokeElemOff = writeInt16OffPtr }

instance Storable (Int32) where {                       sizeOf    _ = 4;                             alignment _ = 4;                            peekElemOff =           readInt32OffPtr;                             pokeElemOff = writeInt32OffPtr }

instance Storable (Int64) where {                       sizeOf    _ = 8;                             alignment _ = 8;                            peekElemOff =           readInt64OffPtr;                             pokeElemOff = writeInt64OffPtr }

-- XXX: here to avoid orphan instance in GHC.Fingerprint
instance Storable Fingerprint where
  sizeOf _ = 16
  alignment _ = 8
  peek = peekFingerprint
  poke = pokeFingerprint

-- peek/poke in fixed BIG-endian 128-bit format
peekFingerprint :: Ptr Fingerprint -> IO Fingerprint
peekFingerprint p0 = do
      let peekW64 :: Ptr Word8 -> Int -> Word64 -> IO Word64
          peekW64 _  0  !i = return i
          peekW64 !p !n !i = do
                w8 <- peek p
                peekW64 (p `plusPtr` 1) (n-1) 
                    ((i `shiftL` 8) .|. fromIntegral w8)

      high <- peekW64 (castPtr p0) 8 0
      low  <- peekW64 (castPtr p0 `plusPtr` 8) 8 0
      return (Fingerprint high low)

pokeFingerprint :: Ptr Fingerprint -> Fingerprint -> IO ()
pokeFingerprint p0 (Fingerprint high low) = do
      let pokeW64 :: Ptr Word8 -> Int -> Word64 -> IO ()
          pokeW64 _ 0  _  = return ()
          pokeW64 p !n !i = do
                pokeElemOff p (n-1) (fromIntegral i)
                pokeW64 p (n-1) (i `shiftR` 8)

      pokeW64 (castPtr p0) 8 high
      pokeW64 (castPtr p0 `plusPtr` 8) 8 low
