{-# LANGUAGE Haskell2010, CPP, Rank2Types, DeriveDataTypeable, StandaloneDeriving #-}
{-# LINE 1 "dist/dist-sandbox-261cd265/build/Data/Time/Clock/CTimespec.hs" #-}

















































{-# LINE 1 "lib/Data/Time/Clock/CTimespec.hsc" #-}
-- #hide
{-# LINE 2 "lib/Data/Time/Clock/CTimespec.hsc" #-}
module Data.Time.Clock.CTimespec where


{-# LINE 5 "lib/Data/Time/Clock/CTimespec.hsc" #-}


{-# LINE 7 "lib/Data/Time/Clock/CTimespec.hsc" #-}


{-# LINE 9 "lib/Data/Time/Clock/CTimespec.hsc" #-}
import Foreign

{-# LINE 13 "lib/Data/Time/Clock/CTimespec.hsc" #-}
import Foreign.C


{-# LINE 16 "lib/Data/Time/Clock/CTimespec.hsc" #-}

data CTimespec = MkCTimespec CTime CLong

instance Storable CTimespec where
    sizeOf _ = (16)
{-# LINE 21 "lib/Data/Time/Clock/CTimespec.hsc" #-}
    alignment _ = alignment (undefined :: CLong)
    peek p = do
        s  <- (\hsc_ptr -> peekByteOff hsc_ptr 0) p
{-# LINE 24 "lib/Data/Time/Clock/CTimespec.hsc" #-}
        ns <- (\hsc_ptr -> peekByteOff hsc_ptr 8) p
{-# LINE 25 "lib/Data/Time/Clock/CTimespec.hsc" #-}
        return (MkCTimespec s ns)
    poke p (MkCTimespec s ns) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p s
{-# LINE 28 "lib/Data/Time/Clock/CTimespec.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p ns
{-# LINE 29 "lib/Data/Time/Clock/CTimespec.hsc" #-}

foreign import ccall unsafe "time.h clock_gettime"
    clock_gettime :: Int32 -> Ptr CTimespec -> IO CInt
{-# LINE 32 "lib/Data/Time/Clock/CTimespec.hsc" #-}

-- | Get the current POSIX time from the system clock.
getCTimespec :: IO CTimespec
getCTimespec = alloca (\ptspec -> do
    throwErrnoIfMinus1_ "clock_gettime" $
        clock_gettime 0 ptspec
{-# LINE 38 "lib/Data/Time/Clock/CTimespec.hsc" #-}
    peek ptspec
    )


{-# LINE 42 "lib/Data/Time/Clock/CTimespec.hsc" #-}
