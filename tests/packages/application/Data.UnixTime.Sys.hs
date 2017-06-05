{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-261cd265/build/Data/UnixTime/Sys.hs" #-}
{-# LINE 1 "Data/UnixTime/Sys.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LINE 2 "Data/UnixTime/Sys.hsc" #-}

module Data.UnixTime.Sys (getUnixTime) where

import Data.UnixTime.Types
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

-- from System.Time


{-# LINE 15 "Data/UnixTime/Sys.hsc" #-}

{-# LINE 16 "Data/UnixTime/Sys.hsc" #-}

type CTimeVal = ()
type CTimeZone = ()

foreign import ccall unsafe "gettimeofday"
    c_gettimeofday :: Ptr CTimeVal -> Ptr CTimeZone -> IO CInt

-- |
-- Getting 'UnixTime' from OS.

getUnixTime :: IO UnixTime
getUnixTime = allocaBytes (16) $ \ p_timeval -> do
{-# LINE 28 "Data/UnixTime/Sys.hsc" #-}
    throwErrnoIfMinus1_ "getClockTime" $ c_gettimeofday p_timeval nullPtr
    sec  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))  p_timeval
{-# LINE 30 "Data/UnixTime/Sys.hsc" #-}
    usec <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p_timeval
{-# LINE 31 "Data/UnixTime/Sys.hsc" #-}
    return $ UnixTime sec usec
