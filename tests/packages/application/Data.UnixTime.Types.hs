{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-261cd265/build/Data/UnixTime/Types.hs" #-}
{-# LINE 1 "Data/UnixTime/Types.hsc" #-}
module Data.UnixTime.Types where
{-# LINE 2 "Data/UnixTime/Types.hsc" #-}

import Control.Applicative ((<$>), (<*>))
import Data.ByteString
import Data.ByteString.Char8 ()
import Data.Int
import Foreign.C.Types
import Foreign.Storable

{-# LINE 10 "Data/UnixTime/Types.hsc" #-}
import Data.Binary

{-# LINE 12 "Data/UnixTime/Types.hsc" #-}


{-# LINE 14 "Data/UnixTime/Types.hsc" #-}

-- |
-- Data structure for Unix time.
--
-- Please note that this uses GHC-derived 'Eq' and 'Ord' instances.
-- Notably
--
-- >>> UnixTime 1 0 > UnixTime 0 999999999
-- True
--
-- You should instead use 'UnixDiffTime' along with its helpers such
-- as 'Data.UnixTime.microSecondsToUnixDiffTime' which will ensure
-- that such unusual values are never created.
data UnixTime = UnixTime {
    -- | Seconds from 1st Jan 1970
    utSeconds :: {-# UNPACK #-} !CTime
    -- | Micro seconds (i.e. 10^(-6))
  , utMicroSeconds :: {-# UNPACK #-} !Int32
  } deriving (Eq,Ord,Show)

instance Storable UnixTime where
    sizeOf _    = ((16))
{-# LINE 36 "Data/UnixTime/Types.hsc" #-}
    alignment _ = (8)
{-# LINE 37 "Data/UnixTime/Types.hsc" #-}
    peek ptr    = UnixTime
            <$> ((\hsc_ptr -> peekByteOff hsc_ptr 0))  ptr
{-# LINE 39 "Data/UnixTime/Types.hsc" #-}
            <*> ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 40 "Data/UnixTime/Types.hsc" #-}
    poke ptr ut = do
            ((\hsc_ptr -> pokeByteOff hsc_ptr 0))  ptr (utSeconds ut)
{-# LINE 42 "Data/UnixTime/Types.hsc" #-}
            ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr (utMicroSeconds ut)
{-# LINE 43 "Data/UnixTime/Types.hsc" #-}


{-# LINE 45 "Data/UnixTime/Types.hsc" #-}
instance Binary UnixTime where
        put (UnixTime (CTime sec) msec) = do
            put sec
            put msec
        get = UnixTime <$> (CTime `fmap` get) <*> get

{-# LINE 51 "Data/UnixTime/Types.hsc" #-}

-- |
-- Format of the strptime()/strftime() style.
type Format = ByteString

-- |
-- Data structure for UnixTime diff.
--
-- It is up to the user to ensure that @'udtMicroSeconds' < 1000000@.
-- Helpers such as 'Data.UnixTime.microSecondsToUnixDiffTime' can help
-- you to create valid values. For example, it's a mistake to use
-- 'Data.Text.addUnixDiffTime' with a value @UnixDiffTime 0 9999999@
-- as it will produce an incorrect value back. You should instead use
-- functions such as 'Data.UnixTime.microSecondsToUnixDiffTime' to
-- create values that are in-range. This avoids any gotchas when then
-- doing comparisons.
data UnixDiffTime = UnixDiffTime {
    -- | Seconds from 1st Jan 1970
    udtSeconds :: {-# UNPACK #-} !CTime
    -- | Micro seconds (i.e. 10^(-6))
  , udtMicroSeconds :: {-# UNPACK #-} !Int32
  } deriving (Eq,Ord,Show)
