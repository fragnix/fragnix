{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Foreign/Ptr.hs" #-}













































{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , MagicHash
           , GeneralizedNewtypeDeriving
  #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Ptr
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides typed pointers to foreign data.  It is part
-- of the Foreign Function Interface (FFI) and will normally be
-- imported via the "Foreign" module.
--
-----------------------------------------------------------------------------

module Foreign.Ptr (

    -- * Data pointers

    Ptr,
    nullPtr,
    castPtr,
    plusPtr,
    alignPtr,
    minusPtr,

    -- * Function pointers

    FunPtr,
    nullFunPtr,
    castFunPtr,
    castFunPtrToPtr,
    castPtrToFunPtr,

    freeHaskellFunPtr,
    -- Free the function pointer created by foreign export dynamic.

    -- * Integral types with lossless conversion to and from pointers
    IntPtr,
    ptrToIntPtr,
    intPtrToPtr,
    WordPtr,
    ptrToWordPtr,
    wordPtrToPtr
 ) where

import GHC.Ptr
import GHC.Base
import GHC.Num
import GHC.Read
import GHC.Real
import GHC.Show
import GHC.Enum

import Data.Bits
import Data.Typeable
import Foreign.Storable ( Storable(..) )

-- | Release the storage associated with the given 'FunPtr', which
-- must have been obtained from a wrapper stub.  This should be called
-- whenever the return value from a foreign import wrapper function is
-- no longer required; otherwise, the storage it uses will leak.
foreign import ccall unsafe "freeHaskellFunctionPtr"
    freeHaskellFunPtr :: FunPtr a -> IO ()
















































































































































































































































































































































































































































{- --------------------------------------------------------------------------
// Dirty CPP hackery for CTypes/CTypesISO
//
// (c) The FFI task force, 2000
// --------------------------------------------------------------------------
-}


{-
// As long as there is no automatic derivation of classes for newtypes we resort
// to extremely dirty cpp-hackery.   :-P   Some care has to be taken when the
// macros below are modified, otherwise the layout rule will bite you.
-}

--  // GHC can derive any class for a newtype, so we make use of that here...









-- | An unsigned integral type that can be losslessly converted to and from
-- @Ptr@. This type is also compatible with the C99 type @uintptr_t@, and
-- can be marshalled to and from that type safely.
newtype WordPtr = WordPtr Word deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Bounded,Integral,Bits,FiniteBits); instance Read WordPtr where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Word);    readList             = unsafeCoerce# (readList  :: ReadS [Word]); }; instance Show WordPtr where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Word -> ShowS);    show                 = unsafeCoerce# (show :: Word -> String);    showList             = unsafeCoerce# (showList :: [Word] -> ShowS); };
        -- Word and Int are guaranteed pointer-sized in GHC

-- | A signed integral type that can be losslessly converted to and from
-- @Ptr@.  This type is also compatible with the C99 type @intptr_t@, and
-- can be marshalled to and from that type safely.
newtype IntPtr = IntPtr Int deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Bounded,Integral,Bits,FiniteBits); instance Read IntPtr where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Int);    readList             = unsafeCoerce# (readList  :: ReadS [Int]); }; instance Show IntPtr where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Int -> ShowS);    show                 = unsafeCoerce# (show :: Int -> String);    showList             = unsafeCoerce# (showList :: [Int] -> ShowS); };
        -- Word and Int are guaranteed pointer-sized in GHC

-- | casts a @Ptr@ to a @WordPtr@
ptrToWordPtr :: Ptr a -> WordPtr
ptrToWordPtr (Ptr a#) = WordPtr (W# (int2Word# (addr2Int# a#)))

-- | casts a @WordPtr@ to a @Ptr@
wordPtrToPtr :: WordPtr -> Ptr a
wordPtrToPtr (WordPtr (W# w#)) = Ptr (int2Addr# (word2Int# w#))

-- | casts a @Ptr@ to an @IntPtr@
ptrToIntPtr :: Ptr a -> IntPtr
ptrToIntPtr (Ptr a#) = IntPtr (I# (addr2Int# a#))

-- | casts an @IntPtr@ to a @Ptr@
intPtrToPtr :: IntPtr -> Ptr a
intPtrToPtr (IntPtr (I# i#)) = Ptr (int2Addr# i#)
