{-# LINE 1 "System.CPUTime.hsc" #-}
{-# LANGUAGE Trustworthy #-}
{-# LINE 2 "System.CPUTime.hsc" #-}
{-# LANGUAGE CPP, NondecreasingIndentation, CApiFFI #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.CPUTime
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The standard CPUTime library.
--
-----------------------------------------------------------------------------


{-# LINE 19 "System.CPUTime.hsc" #-}

{-# LINE 20 "System.CPUTime.hsc" #-}

module System.CPUTime
        (
         getCPUTime,       -- :: IO Integer
         cpuTimePrecision  -- :: Integer
        ) where

import Data.Ratio

import Foreign
import Foreign.C

-- For struct rusage

{-# LINE 34 "System.CPUTime.hsc" #-}

{-# LINE 35 "System.CPUTime.hsc" #-}

{-# LINE 36 "System.CPUTime.hsc" #-}

{-# LINE 37 "System.CPUTime.hsc" #-}

{-# LINE 38 "System.CPUTime.hsc" #-}

-- For FILETIME etc. on Windows

{-# LINE 43 "System.CPUTime.hsc" #-}

-- for struct tms

{-# LINE 46 "System.CPUTime.hsc" #-}

{-# LINE 47 "System.CPUTime.hsc" #-}

{-# LINE 48 "System.CPUTime.hsc" #-}


{-# LINE 61 "System.CPUTime.hsc" #-}
realToInteger :: Real a => a -> Integer
realToInteger ct = round (realToFrac ct :: Double)
  -- CTime, CClock, CUShort etc are in Real but not Fractional,
  -- so we must convert to Double before we can round it

{-# LINE 66 "System.CPUTime.hsc" #-}

-- -----------------------------------------------------------------------------
-- |Computation 'getCPUTime' returns the number of picoseconds CPU time
-- used by the current program.  The precision of this result is
-- implementation-dependent.

getCPUTime :: IO Integer
getCPUTime = do


{-# LINE 76 "System.CPUTime.hsc" #-}
-- getrusage() is right royal pain to deal with when targetting multiple
-- versions of Solaris, since some versions supply it in libc (2.3 and 2.5),
-- while 2.4 has got it in libucb (I wouldn't be too surprised if it was back
-- again in libucb in 2.6..)
--
-- Avoid the problem by resorting to times() instead.
--

{-# LINE 84 "System.CPUTime.hsc" #-}
    allocaBytes (144) $ \ p_rusage -> do
{-# LINE 85 "System.CPUTime.hsc" #-}
    throwErrnoIfMinus1_ "getrusage" $ getrusage (0) p_rusage
{-# LINE 86 "System.CPUTime.hsc" #-}

    let ru_utime = ((\hsc_ptr -> hsc_ptr `plusPtr` 0)) p_rusage
{-# LINE 88 "System.CPUTime.hsc" #-}
    let ru_stime = ((\hsc_ptr -> hsc_ptr `plusPtr` 16)) p_rusage
{-# LINE 89 "System.CPUTime.hsc" #-}
    u_sec  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))  ru_utime :: IO CTime
{-# LINE 90 "System.CPUTime.hsc" #-}
    u_usec <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ru_utime :: IO CSUSeconds
{-# LINE 91 "System.CPUTime.hsc" #-}
    s_sec  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))  ru_stime :: IO CTime
{-# LINE 92 "System.CPUTime.hsc" #-}
    s_usec <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ru_stime :: IO CSUSeconds
{-# LINE 93 "System.CPUTime.hsc" #-}
    return ((realToInteger u_sec * 1000000 + realToInteger u_usec +
             realToInteger s_sec * 1000000 + realToInteger s_usec)
                * 1000000)

type CRUsage = ()
foreign import capi unsafe "HsBase.h getrusage" getrusage :: CInt -> Ptr CRUsage -> IO CInt

{-# LINE 115 "System.CPUTime.hsc" #-}


{-# LINE 149 "System.CPUTime.hsc" #-}


-- |The 'cpuTimePrecision' constant is the smallest measurable difference
-- in CPU time that the implementation can record, and is given as an
-- integral number of picoseconds.

cpuTimePrecision :: Integer
cpuTimePrecision = round ((1000000000000::Integer) % fromIntegral (clockTicks))

foreign import ccall unsafe clk_tck :: CLong

clockTicks :: Int
clockTicks = fromIntegral clk_tck
