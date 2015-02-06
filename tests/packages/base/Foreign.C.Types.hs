{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Foreign/C/Types.hs" #-}













































{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , MagicHash
           , GeneralizedNewtypeDeriving
  #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
-- XXX -fno-warn-unused-binds stops us warning about unused constructors,
-- but really we should just remove them if we don't want them

-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.C.Types
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Mapping of C types to corresponding Haskell types.
--
-----------------------------------------------------------------------------

module Foreign.C.Types
        ( -- * Representations of C types
          -- $ctypes

          -- ** Integral types
          -- | These types are are represented as @newtype@s of
          -- types in "Data.Int" and "Data.Word", and are instances of
          -- 'Prelude.Eq', 'Prelude.Ord', 'Prelude.Num', 'Prelude.Read',
          -- 'Prelude.Show', 'Prelude.Enum', 'Typeable', 'Storable',
          -- 'Prelude.Bounded', 'Prelude.Real', 'Prelude.Integral' and
          -- 'Bits'.
          CChar(..),    CSChar(..),   CUChar(..)
        , CShort(..),   CUShort(..),  CInt(..),      CUInt(..)
        , CLong(..),    CULong(..)
        , CPtrdiff(..), CSize(..),    CWchar(..),    CSigAtomic(..)
        , CLLong(..),   CULLong(..)
        , CIntPtr(..),  CUIntPtr(..), CIntMax(..),   CUIntMax(..)

          -- ** Numeric types
          -- | These types are represented as @newtype@s of basic
          -- foreign types, and are instances of
          -- 'Prelude.Eq', 'Prelude.Ord', 'Prelude.Num', 'Prelude.Read',
          -- 'Prelude.Show', 'Prelude.Enum', 'Typeable' and 'Storable'.
        , CClock(..),   CTime(..),    CUSeconds(..), CSUSeconds(..)

        -- extracted from CTime, because we don't want this comment in
        -- the Haskell language reports:

        -- | To convert 'CTime' to 'Data.Time.UTCTime', use the following:
        --
        -- > \t -> posixSecondsToUTCTime (realToFrac t :: POSIXTime)
        --

          -- ** Floating types
          -- | These types are are represented as @newtype@s of
          -- 'Prelude.Float' and 'Prelude.Double', and are instances of
          -- 'Prelude.Eq', 'Prelude.Ord', 'Prelude.Num', 'Prelude.Read',
          -- 'Prelude.Show', 'Prelude.Enum', 'Typeable', 'Storable',
          -- 'Prelude.Real', 'Prelude.Fractional', 'Prelude.Floating',
          -- 'Prelude.RealFrac' and 'Prelude.RealFloat'.
        , CFloat(..),   CDouble(..)
        -- XXX GHC doesn't support CLDouble yet
        -- , CLDouble(..)

          -- ** Other types

          -- Instances of: Eq and Storable
        , CFile,        CFpos,     CJmpBuf
        ) where

import Foreign.Storable
import Data.Bits        ( Bits(..), FiniteBits(..) )
import Data.Int         ( Int8,  Int16,  Int32,  Int64  )
import Data.Word        ( Word8, Word16, Word32, Word64 )
import Data.Typeable

import GHC.Base
import GHC.Float
import GHC.Enum
import GHC.Real
import GHC.Show
import GHC.Read
import GHC.Num
















































































































































































































































































































































































































































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









-- | Haskell type representing the C @char@ type.
newtype CChar = CChar Int8 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Bounded,Integral,Bits,FiniteBits); instance Read CChar where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Int8);    readList             = unsafeCoerce# (readList  :: ReadS [Int8]); }; instance Show CChar where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Int8 -> ShowS);    show                 = unsafeCoerce# (show :: Int8 -> String);    showList             = unsafeCoerce# (showList :: [Int8] -> ShowS); };
-- | Haskell type representing the C @signed char@ type.
newtype CSChar = CSChar Int8 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Bounded,Integral,Bits,FiniteBits); instance Read CSChar where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Int8);    readList             = unsafeCoerce# (readList  :: ReadS [Int8]); }; instance Show CSChar where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Int8 -> ShowS);    show                 = unsafeCoerce# (show :: Int8 -> String);    showList             = unsafeCoerce# (showList :: [Int8] -> ShowS); };
-- | Haskell type representing the C @unsigned char@ type.
newtype CUChar = CUChar Word8 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Bounded,Integral,Bits,FiniteBits); instance Read CUChar where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Word8);    readList             = unsafeCoerce# (readList  :: ReadS [Word8]); }; instance Show CUChar where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Word8 -> ShowS);    show                 = unsafeCoerce# (show :: Word8 -> String);    showList             = unsafeCoerce# (showList :: [Word8] -> ShowS); };

-- | Haskell type representing the C @short@ type.
newtype CShort = CShort Int16 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Bounded,Integral,Bits,FiniteBits); instance Read CShort where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Int16);    readList             = unsafeCoerce# (readList  :: ReadS [Int16]); }; instance Show CShort where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Int16 -> ShowS);    show                 = unsafeCoerce# (show :: Int16 -> String);    showList             = unsafeCoerce# (showList :: [Int16] -> ShowS); };
-- | Haskell type representing the C @unsigned short@ type.
newtype CUShort = CUShort Word16 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Bounded,Integral,Bits,FiniteBits); instance Read CUShort where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Word16);    readList             = unsafeCoerce# (readList  :: ReadS [Word16]); }; instance Show CUShort where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Word16 -> ShowS);    show                 = unsafeCoerce# (show :: Word16 -> String);    showList             = unsafeCoerce# (showList :: [Word16] -> ShowS); };

-- | Haskell type representing the C @int@ type.
newtype CInt = CInt Int32 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Bounded,Integral,Bits,FiniteBits); instance Read CInt where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Int32);    readList             = unsafeCoerce# (readList  :: ReadS [Int32]); }; instance Show CInt where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Int32 -> ShowS);    show                 = unsafeCoerce# (show :: Int32 -> String);    showList             = unsafeCoerce# (showList :: [Int32] -> ShowS); };
-- | Haskell type representing the C @unsigned int@ type.
newtype CUInt = CUInt Word32 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Bounded,Integral,Bits,FiniteBits); instance Read CUInt where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Word32);    readList             = unsafeCoerce# (readList  :: ReadS [Word32]); }; instance Show CUInt where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Word32 -> ShowS);    show                 = unsafeCoerce# (show :: Word32 -> String);    showList             = unsafeCoerce# (showList :: [Word32] -> ShowS); };

-- | Haskell type representing the C @long@ type.
newtype CLong = CLong Int64 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Bounded,Integral,Bits,FiniteBits); instance Read CLong where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Int64);    readList             = unsafeCoerce# (readList  :: ReadS [Int64]); }; instance Show CLong where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Int64 -> ShowS);    show                 = unsafeCoerce# (show :: Int64 -> String);    showList             = unsafeCoerce# (showList :: [Int64] -> ShowS); };
-- | Haskell type representing the C @unsigned long@ type.
newtype CULong = CULong Word64 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Bounded,Integral,Bits,FiniteBits); instance Read CULong where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Word64);    readList             = unsafeCoerce# (readList  :: ReadS [Word64]); }; instance Show CULong where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Word64 -> ShowS);    show                 = unsafeCoerce# (show :: Word64 -> String);    showList             = unsafeCoerce# (showList :: [Word64] -> ShowS); };

-- | Haskell type representing the C @long long@ type.
newtype CLLong = CLLong Int64 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Bounded,Integral,Bits,FiniteBits); instance Read CLLong where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Int64);    readList             = unsafeCoerce# (readList  :: ReadS [Int64]); }; instance Show CLLong where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Int64 -> ShowS);    show                 = unsafeCoerce# (show :: Int64 -> String);    showList             = unsafeCoerce# (showList :: [Int64] -> ShowS); };
-- | Haskell type representing the C @unsigned long long@ type.
newtype CULLong = CULLong Word64 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Bounded,Integral,Bits,FiniteBits); instance Read CULLong where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Word64);    readList             = unsafeCoerce# (readList  :: ReadS [Word64]); }; instance Show CULLong where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Word64 -> ShowS);    show                 = unsafeCoerce# (show :: Word64 -> String);    showList             = unsafeCoerce# (showList :: [Word64] -> ShowS); };

{-# RULES
"fromIntegral/a->CChar"   fromIntegral = \x -> CChar   (fromIntegral x)
"fromIntegral/a->CSChar"  fromIntegral = \x -> CSChar  (fromIntegral x)
"fromIntegral/a->CUChar"  fromIntegral = \x -> CUChar  (fromIntegral x)
"fromIntegral/a->CShort"  fromIntegral = \x -> CShort  (fromIntegral x)
"fromIntegral/a->CUShort" fromIntegral = \x -> CUShort (fromIntegral x)
"fromIntegral/a->CInt"    fromIntegral = \x -> CInt    (fromIntegral x)
"fromIntegral/a->CUInt"   fromIntegral = \x -> CUInt   (fromIntegral x)
"fromIntegral/a->CLong"   fromIntegral = \x -> CLong   (fromIntegral x)
"fromIntegral/a->CULong"  fromIntegral = \x -> CULong  (fromIntegral x)
"fromIntegral/a->CLLong"  fromIntegral = \x -> CLLong  (fromIntegral x)
"fromIntegral/a->CULLong" fromIntegral = \x -> CULLong (fromIntegral x)

"fromIntegral/CChar->a"   fromIntegral = \(CChar   x) -> fromIntegral x
"fromIntegral/CSChar->a"  fromIntegral = \(CSChar  x) -> fromIntegral x
"fromIntegral/CUChar->a"  fromIntegral = \(CUChar  x) -> fromIntegral x
"fromIntegral/CShort->a"  fromIntegral = \(CShort  x) -> fromIntegral x
"fromIntegral/CUShort->a" fromIntegral = \(CUShort x) -> fromIntegral x
"fromIntegral/CInt->a"    fromIntegral = \(CInt    x) -> fromIntegral x
"fromIntegral/CUInt->a"   fromIntegral = \(CUInt   x) -> fromIntegral x
"fromIntegral/CLong->a"   fromIntegral = \(CLong   x) -> fromIntegral x
"fromIntegral/CULong->a"  fromIntegral = \(CULong  x) -> fromIntegral x
"fromIntegral/CLLong->a"  fromIntegral = \(CLLong  x) -> fromIntegral x
"fromIntegral/CULLong->a" fromIntegral = \(CULLong x) -> fromIntegral x
 #-}

-- | Haskell type representing the C @float@ type.
newtype CFloat = CFloat Float deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Fractional,Floating,RealFrac,RealFloat); instance Read CFloat where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Float);    readList             = unsafeCoerce# (readList  :: ReadS [Float]); }; instance Show CFloat where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Float -> ShowS);    show                 = unsafeCoerce# (show :: Float -> String);    showList             = unsafeCoerce# (showList :: [Float] -> ShowS); };
-- | Haskell type representing the C @double@ type.
newtype CDouble = CDouble Double deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Fractional,Floating,RealFrac,RealFloat); instance Read CDouble where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Double);    readList             = unsafeCoerce# (readList  :: ReadS [Double]); }; instance Show CDouble where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Double -> ShowS);    show                 = unsafeCoerce# (show :: Double -> String);    showList             = unsafeCoerce# (showList :: [Double] -> ShowS); };
-- XXX GHC doesn't support CLDouble yet

{-# RULES
"realToFrac/a->CFloat"    realToFrac = \x -> CFloat   (realToFrac x)
"realToFrac/a->CDouble"   realToFrac = \x -> CDouble  (realToFrac x)

"realToFrac/CFloat->a"    realToFrac = \(CFloat   x) -> realToFrac x
"realToFrac/CDouble->a"   realToFrac = \(CDouble  x) -> realToFrac x
 #-}

-- GHC doesn't support CLDouble yet
-- "realToFrac/a->CLDouble"  realToFrac = \x -> CLDouble (realToFrac x)
-- "realToFrac/CLDouble->a"  realToFrac = \(CLDouble x) -> realToFrac x

-- | Haskell type representing the C @ptrdiff_t@ type.
newtype CPtrdiff = CPtrdiff Int64 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Bounded,Integral,Bits,FiniteBits); instance Read CPtrdiff where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Int64);    readList             = unsafeCoerce# (readList  :: ReadS [Int64]); }; instance Show CPtrdiff where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Int64 -> ShowS);    show                 = unsafeCoerce# (show :: Int64 -> String);    showList             = unsafeCoerce# (showList :: [Int64] -> ShowS); };
-- | Haskell type representing the C @size_t@ type.
newtype CSize = CSize Word64 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Bounded,Integral,Bits,FiniteBits); instance Read CSize where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Word64);    readList             = unsafeCoerce# (readList  :: ReadS [Word64]); }; instance Show CSize where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Word64 -> ShowS);    show                 = unsafeCoerce# (show :: Word64 -> String);    showList             = unsafeCoerce# (showList :: [Word64] -> ShowS); };
-- | Haskell type representing the C @wchar_t@ type.
newtype CWchar = CWchar Int32 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Bounded,Integral,Bits,FiniteBits); instance Read CWchar where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Int32);    readList             = unsafeCoerce# (readList  :: ReadS [Int32]); }; instance Show CWchar where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Int32 -> ShowS);    show                 = unsafeCoerce# (show :: Int32 -> String);    showList             = unsafeCoerce# (showList :: [Int32] -> ShowS); };
-- | Haskell type representing the C @sig_atomic_t@ type.
newtype CSigAtomic = CSigAtomic Int32 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Bounded,Integral,Bits,FiniteBits); instance Read CSigAtomic where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Int32);    readList             = unsafeCoerce# (readList  :: ReadS [Int32]); }; instance Show CSigAtomic where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Int32 -> ShowS);    show                 = unsafeCoerce# (show :: Int32 -> String);    showList             = unsafeCoerce# (showList :: [Int32] -> ShowS); };

{-# RULES
"fromIntegral/a->CPtrdiff"   fromIntegral = \x -> CPtrdiff   (fromIntegral x)
"fromIntegral/a->CSize"      fromIntegral = \x -> CSize      (fromIntegral x)
"fromIntegral/a->CWchar"     fromIntegral = \x -> CWchar     (fromIntegral x)
"fromIntegral/a->CSigAtomic" fromIntegral = \x -> CSigAtomic (fromIntegral x)

"fromIntegral/CPtrdiff->a"   fromIntegral = \(CPtrdiff   x) -> fromIntegral x
"fromIntegral/CSize->a"      fromIntegral = \(CSize      x) -> fromIntegral x
"fromIntegral/CWchar->a"     fromIntegral = \(CWchar     x) -> fromIntegral x
"fromIntegral/CSigAtomic->a" fromIntegral = \(CSigAtomic x) -> fromIntegral x
 #-}

-- | Haskell type representing the C @clock_t@ type.
newtype CClock = CClock Int64 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable); instance Read CClock where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Int64);    readList             = unsafeCoerce# (readList  :: ReadS [Int64]); }; instance Show CClock where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Int64 -> ShowS);    show                 = unsafeCoerce# (show :: Int64 -> String);    showList             = unsafeCoerce# (showList :: [Int64] -> ShowS); };
-- | Haskell type representing the C @time_t@ type.
newtype CTime = CTime Int64 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable); instance Read CTime where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Int64);    readList             = unsafeCoerce# (readList  :: ReadS [Int64]); }; instance Show CTime where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Int64 -> ShowS);    show                 = unsafeCoerce# (show :: Int64 -> String);    showList             = unsafeCoerce# (showList :: [Int64] -> ShowS); };
-- | Haskell type representing the C @useconds_t@ type.
--
-- /Since: 4.4.0.0/
newtype CUSeconds = CUSeconds Word32 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable); instance Read CUSeconds where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Word32);    readList             = unsafeCoerce# (readList  :: ReadS [Word32]); }; instance Show CUSeconds where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Word32 -> ShowS);    show                 = unsafeCoerce# (show :: Word32 -> String);    showList             = unsafeCoerce# (showList :: [Word32] -> ShowS); };
-- | Haskell type representing the C @suseconds_t@ type.
--
-- /Since: 4.4.0.0/
newtype CSUSeconds = CSUSeconds Int64 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable); instance Read CSUSeconds where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Int64);    readList             = unsafeCoerce# (readList  :: ReadS [Int64]); }; instance Show CSUSeconds where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Int64 -> ShowS);    show                 = unsafeCoerce# (show :: Int64 -> String);    showList             = unsafeCoerce# (showList :: [Int64] -> ShowS); };

-- FIXME: Implement and provide instances for Eq and Storable
-- | Haskell type representing the C @FILE@ type.
data CFile = CFile
-- | Haskell type representing the C @fpos_t@ type.
data CFpos = CFpos
-- | Haskell type representing the C @jmp_buf@ type.
data CJmpBuf = CJmpBuf

newtype CIntPtr = CIntPtr Int64 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Bounded,Integral,Bits,FiniteBits); instance Read CIntPtr where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Int64);    readList             = unsafeCoerce# (readList  :: ReadS [Int64]); }; instance Show CIntPtr where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Int64 -> ShowS);    show                 = unsafeCoerce# (show :: Int64 -> String);    showList             = unsafeCoerce# (showList :: [Int64] -> ShowS); };
newtype CUIntPtr = CUIntPtr Word64 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Bounded,Integral,Bits,FiniteBits); instance Read CUIntPtr where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Word64);    readList             = unsafeCoerce# (readList  :: ReadS [Word64]); }; instance Show CUIntPtr where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Word64 -> ShowS);    show                 = unsafeCoerce# (show :: Word64 -> String);    showList             = unsafeCoerce# (showList :: [Word64] -> ShowS); };
newtype CIntMax = CIntMax Int64 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Bounded,Integral,Bits,FiniteBits); instance Read CIntMax where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Int64);    readList             = unsafeCoerce# (readList  :: ReadS [Int64]); }; instance Show CIntMax where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Int64 -> ShowS);    show                 = unsafeCoerce# (show :: Int64 -> String);    showList             = unsafeCoerce# (showList :: [Int64] -> ShowS); };
newtype CUIntMax = CUIntMax Word64 deriving (Eq,Ord,Num,Enum,Storable,Real,Typeable, Bounded,Integral,Bits,FiniteBits); instance Read CUIntMax where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Word64);    readList             = unsafeCoerce# (readList  :: ReadS [Word64]); }; instance Show CUIntMax where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Word64 -> ShowS);    show                 = unsafeCoerce# (show :: Word64 -> String);    showList             = unsafeCoerce# (showList :: [Word64] -> ShowS); };

{-# RULES
"fromIntegral/a->CIntPtr"  fromIntegral = \x -> CIntPtr  (fromIntegral x)
"fromIntegral/a->CUIntPtr" fromIntegral = \x -> CUIntPtr (fromIntegral x)
"fromIntegral/a->CIntMax"  fromIntegral = \x -> CIntMax  (fromIntegral x)
"fromIntegral/a->CUIntMax" fromIntegral = \x -> CUIntMax (fromIntegral x)
 #-}

-- C99 types which are still missing include:
-- wint_t, wctrans_t, wctype_t

{- $ctypes

These types are needed to accurately represent C function prototypes,
in order to access C library interfaces in Haskell.  The Haskell system
is not required to represent those types exactly as C does, but the
following guarantees are provided concerning a Haskell type @CT@
representing a C type @t@:

* If a C function prototype has @t@ as an argument or result type, the
  use of @CT@ in the corresponding position in a foreign declaration
  permits the Haskell program to access the full range of values encoded
  by the C type; and conversely, any Haskell value for @CT@ has a valid
  representation in C.

* @'sizeOf' ('Prelude.undefined' :: CT)@ will yield the same value as
  @sizeof (t)@ in C.

* @'alignment' ('Prelude.undefined' :: CT)@ matches the alignment
  constraint enforced by the C implementation for @t@.

* The members 'peek' and 'poke' of the 'Storable' class map all values
  of @CT@ to the corresponding value of @t@ and vice versa.

* When an instance of 'Prelude.Bounded' is defined for @CT@, the values
  of 'Prelude.minBound' and 'Prelude.maxBound' coincide with @t_MIN@
  and @t_MAX@ in C.

* When an instance of 'Prelude.Eq' or 'Prelude.Ord' is defined for @CT@,
  the predicates defined by the type class implement the same relation
  as the corresponding predicate in C on @t@.

* When an instance of 'Prelude.Num', 'Prelude.Read', 'Prelude.Integral',
  'Prelude.Fractional', 'Prelude.Floating', 'Prelude.RealFrac', or
  'Prelude.RealFloat' is defined for @CT@, the arithmetic operations
  defined by the type class implement the same function as the
  corresponding arithmetic operations (if available) in C on @t@.

* When an instance of 'Bits' is defined for @CT@, the bitwise operation
  defined by the type class implement the same function as the
  corresponding bitwise operation in C on @t@.

-}

