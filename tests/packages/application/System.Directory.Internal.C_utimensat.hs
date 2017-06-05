{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-261cd265/build/System/Directory/Internal/C_utimensat.hs" #-}
{-# LINE 1 "System/Directory/Internal/C_utimensat.hsc" #-}
module System.Directory.Internal.C_utimensat where
{-# LINE 2 "System/Directory/Internal/C_utimensat.hsc" #-}

{-# LINE 3 "System/Directory/Internal/C_utimensat.hsc" #-}

{-# LINE 4 "System/Directory/Internal/C_utimensat.hsc" #-}

{-# LINE 5 "System/Directory/Internal/C_utimensat.hsc" #-}

{-# LINE 6 "System/Directory/Internal/C_utimensat.hsc" #-}

{-# LINE 7 "System/Directory/Internal/C_utimensat.hsc" #-}

{-# LINE 8 "System/Directory/Internal/C_utimensat.hsc" #-}

{-# LINE 9 "System/Directory/Internal/C_utimensat.hsc" #-}

{-# LINE 10 "System/Directory/Internal/C_utimensat.hsc" #-}

{-# LINE 11 "System/Directory/Internal/C_utimensat.hsc" #-}

{-# LINE 12 "System/Directory/Internal/C_utimensat.hsc" #-}

{-# LINE 13 "System/Directory/Internal/C_utimensat.hsc" #-}

{-# LINE 14 "System/Directory/Internal/C_utimensat.hsc" #-}
import Prelude ()
import System.Directory.Internal.Prelude
import Data.Time.Clock.POSIX (POSIXTime)

data CTimeSpec = CTimeSpec EpochTime CLong

instance Storable CTimeSpec where
    sizeOf    _ = (16)
{-# LINE 22 "System/Directory/Internal/C_utimensat.hsc" #-}
    -- workaround (hsc2hs for GHC < 8.0 doesn't support #{alignment ...})
    alignment _ = (8)
{-# LINE 24 "System/Directory/Internal/C_utimensat.hsc" #-}
    poke p (CTimeSpec sec nsec) = do
      ((\hsc_ptr -> pokeByteOff hsc_ptr 0))  p sec
{-# LINE 26 "System/Directory/Internal/C_utimensat.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p nsec
{-# LINE 27 "System/Directory/Internal/C_utimensat.hsc" #-}
    peek p = do
      sec  <- (\hsc_ptr -> peekByteOff hsc_ptr 0) p
{-# LINE 29 "System/Directory/Internal/C_utimensat.hsc" #-}
      nsec <- (\hsc_ptr -> peekByteOff hsc_ptr 8) p
{-# LINE 30 "System/Directory/Internal/C_utimensat.hsc" #-}
      return (CTimeSpec sec nsec)

c_AT_FDCWD :: CInt
c_AT_FDCWD = (-100)
{-# LINE 34 "System/Directory/Internal/C_utimensat.hsc" #-}

utimeOmit :: CTimeSpec
utimeOmit = CTimeSpec (CTime 0) (1073741822)
{-# LINE 37 "System/Directory/Internal/C_utimensat.hsc" #-}

toCTimeSpec :: POSIXTime -> CTimeSpec
toCTimeSpec t = CTimeSpec (CTime sec) (truncate $ 10 ^ (9 :: Int) * frac)
  where
    (sec,  frac)  = if frac' < 0 then (sec' - 1, frac' + 1) else (sec', frac')
    (sec', frac') = properFraction (toRational t)

foreign import ccall "utimensat" c_utimensat
  :: CInt -> CString -> Ptr CTimeSpec -> CInt -> IO CInt


{-# LINE 48 "System/Directory/Internal/C_utimensat.hsc" #-}
