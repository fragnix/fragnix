{-# LINE 1 "System.Posix.Types.hs" #-}










































{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , MagicHash
           , GeneralizedNewtypeDeriving
  #-}
{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Types
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX data types: Haskell equivalents of the types defined by the
-- @\<sys\/types.h>@ C header on a POSIX system.
--
-----------------------------------------------------------------------------
































































































































































































































































































































































































































































module System.Posix.Types (

  -- * POSIX data types

  -- ** Platform differences
  -- | This module contains platform specific information about types.
  --   __/As such the types presented on this page reflect the platform
  --   on which the documentation was generated and may not coincide with
  --   the types on your platform./__
  CDev(..),
  CIno(..),
  CMode(..),
  COff(..),
  CPid(..),
  CSsize(..),

  CGid(..),
  CNlink(..),
  CUid(..),
  CCc(..),
  CSpeed(..),
  CTcflag(..),
  CRLim(..),

  Fd(..),

  LinkCount,
  UserID,
  GroupID,

  ByteCount,
  ClockTick,
  EpochTime,
  FileOffset,
  ProcessID,
  ProcessGroupID,
  DeviceID,
  FileID,
  FileMode,
  Limit
 ) where

import Foreign
import Foreign.C
-- import Data.Bits

import GHC.Base
import GHC.Enum
import GHC.Num
import GHC.Real
-- import GHC.Prim
import GHC.Read
import GHC.Show

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









newtype CDev = CDev Word64 deriving (Eq,Ord,Num,Enum,Storable,Real, Bounded,Integral,Bits,FiniteBits); instance Read CDev where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Word64);    readList             = unsafeCoerce# (readList  :: ReadS [Word64]); }; instance Show CDev where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Word64 -> ShowS);    show                 = unsafeCoerce# (show :: Word64 -> String);    showList             = unsafeCoerce# (showList :: [Word64] -> ShowS); };
newtype CIno = CIno Word64 deriving (Eq,Ord,Num,Enum,Storable,Real, Bounded,Integral,Bits,FiniteBits); instance Read CIno where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Word64);    readList             = unsafeCoerce# (readList  :: ReadS [Word64]); }; instance Show CIno where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Word64 -> ShowS);    show                 = unsafeCoerce# (show :: Word64 -> String);    showList             = unsafeCoerce# (showList :: [Word64] -> ShowS); };
newtype {-# CTYPE "mode_t" #-} CMode = CMode Word32 deriving (Eq,Ord,Num,Enum,Storable,Real, Bounded,Integral,Bits,FiniteBits); instance Read CMode where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Word32);    readList             = unsafeCoerce# (readList  :: ReadS [Word32]); }; instance Show CMode where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Word32 -> ShowS);    show                 = unsafeCoerce# (show :: Word32 -> String);    showList             = unsafeCoerce# (showList :: [Word32] -> ShowS); };
newtype COff = COff Int64 deriving (Eq,Ord,Num,Enum,Storable,Real, Bounded,Integral,Bits,FiniteBits); instance Read COff where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Int64);    readList             = unsafeCoerce# (readList  :: ReadS [Int64]); }; instance Show COff where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Int64 -> ShowS);    show                 = unsafeCoerce# (show :: Int64 -> String);    showList             = unsafeCoerce# (showList :: [Int64] -> ShowS); };
newtype CPid = CPid Int32 deriving (Eq,Ord,Num,Enum,Storable,Real, Bounded,Integral,Bits,FiniteBits); instance Read CPid where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Int32);    readList             = unsafeCoerce# (readList  :: ReadS [Int32]); }; instance Show CPid where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Int32 -> ShowS);    show                 = unsafeCoerce# (show :: Int32 -> String);    showList             = unsafeCoerce# (showList :: [Int32] -> ShowS); };

newtype CSsize = CSsize Int64 deriving (Eq,Ord,Num,Enum,Storable,Real, Bounded,Integral,Bits,FiniteBits); instance Read CSsize where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Int64);    readList             = unsafeCoerce# (readList  :: ReadS [Int64]); }; instance Show CSsize where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Int64 -> ShowS);    show                 = unsafeCoerce# (show :: Int64 -> String);    showList             = unsafeCoerce# (showList :: [Int64] -> ShowS); };

newtype CGid = CGid Word32 deriving (Eq,Ord,Num,Enum,Storable,Real, Bounded,Integral,Bits,FiniteBits); instance Read CGid where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Word32);    readList             = unsafeCoerce# (readList  :: ReadS [Word32]); }; instance Show CGid where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Word32 -> ShowS);    show                 = unsafeCoerce# (show :: Word32 -> String);    showList             = unsafeCoerce# (showList :: [Word32] -> ShowS); };
newtype CNlink = CNlink Word64 deriving (Eq,Ord,Num,Enum,Storable,Real, Bounded,Integral,Bits,FiniteBits); instance Read CNlink where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Word64);    readList             = unsafeCoerce# (readList  :: ReadS [Word64]); }; instance Show CNlink where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Word64 -> ShowS);    show                 = unsafeCoerce# (show :: Word64 -> String);    showList             = unsafeCoerce# (showList :: [Word64] -> ShowS); };

newtype CUid = CUid Word32 deriving (Eq,Ord,Num,Enum,Storable,Real, Bounded,Integral,Bits,FiniteBits); instance Read CUid where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Word32);    readList             = unsafeCoerce# (readList  :: ReadS [Word32]); }; instance Show CUid where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Word32 -> ShowS);    show                 = unsafeCoerce# (show :: Word32 -> String);    showList             = unsafeCoerce# (showList :: [Word32] -> ShowS); };
newtype CCc = CCc Word8 deriving (Eq,Ord,Num,Enum,Storable,Real); instance Read CCc where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Word8);    readList             = unsafeCoerce# (readList  :: ReadS [Word8]); }; instance Show CCc where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Word8 -> ShowS);    show                 = unsafeCoerce# (show :: Word8 -> String);    showList             = unsafeCoerce# (showList :: [Word8] -> ShowS); };
newtype CSpeed = CSpeed Word32 deriving (Eq,Ord,Num,Enum,Storable,Real); instance Read CSpeed where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Word32);    readList             = unsafeCoerce# (readList  :: ReadS [Word32]); }; instance Show CSpeed where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Word32 -> ShowS);    show                 = unsafeCoerce# (show :: Word32 -> String);    showList             = unsafeCoerce# (showList :: [Word32] -> ShowS); };
newtype CTcflag = CTcflag Word32 deriving (Eq,Ord,Num,Enum,Storable,Real, Bounded,Integral,Bits,FiniteBits); instance Read CTcflag where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Word32);    readList             = unsafeCoerce# (readList  :: ReadS [Word32]); }; instance Show CTcflag where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Word32 -> ShowS);    show                 = unsafeCoerce# (show :: Word32 -> String);    showList             = unsafeCoerce# (showList :: [Word32] -> ShowS); };
newtype CRLim = CRLim Word64 deriving (Eq,Ord,Num,Enum,Storable,Real, Bounded,Integral,Bits,FiniteBits); instance Read CRLim where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS Word64);    readList             = unsafeCoerce# (readList  :: ReadS [Word64]); }; instance Show CRLim where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> Word64 -> ShowS);    show                 = unsafeCoerce# (show :: Word64 -> String);    showList             = unsafeCoerce# (showList :: [Word64] -> ShowS); };

-- ToDo: blksize_t, clockid_t, blkcnt_t, fsblkcnt_t, fsfilcnt_t, id_t, key_t
-- suseconds_t, timer_t, useconds_t

-- Make an Fd type rather than using CInt everywhere
newtype Fd = Fd CInt deriving (Eq,Ord,Num,Enum,Storable,Real, Bounded,Integral,Bits,FiniteBits); instance Read Fd where {    readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS CInt);    readList             = unsafeCoerce# (readList  :: ReadS [CInt]); }; instance Show Fd where {    showsPrec            = unsafeCoerce# (showsPrec :: Int -> CInt -> ShowS);    show                 = unsafeCoerce# (show :: CInt -> String);    showList             = unsafeCoerce# (showList :: [CInt] -> ShowS); };

-- nicer names, and backwards compatibility with POSIX library:
type LinkCount      = CNlink
type UserID         = CUid
type GroupID        = CGid

type ByteCount      = CSize
type ClockTick      = CClock
type EpochTime      = CTime
type DeviceID       = CDev
type FileID         = CIno
type FileMode       = CMode
type ProcessID      = CPid
type FileOffset     = COff
type ProcessGroupID = CPid
type Limit          = CLong

