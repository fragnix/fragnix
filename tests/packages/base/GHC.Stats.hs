{-# LINE 1 "GHC.Stats.hsc" #-}
{-# LANGUAGE Trustworthy #-}
{-# LINE 2 "GHC.Stats.hsc" #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- | This module provides access to internal garbage collection and
-- memory usage statistics.  These statistics are not available unless
-- a program is run with the @-T@ RTS flag.
--
-- This module is GHC-only and should not be considered portable.
--
-- @since 4.5.0.0
-----------------------------------------------------------------------------
module GHC.Stats
    ( GCStats(..)
    , getGCStats
    , getGCStatsEnabled
) where

import Control.Monad
import Data.Int
import GHC.Base
import GHC.Read ( Read )
import GHC.Show ( Show )
import GHC.IO.Exception
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr


{-# LINE 32 "GHC.Stats.hsc" #-}

foreign import ccall "getGCStats"        getGCStats_       :: Ptr () -> IO ()

-- | Returns whether GC stats have been enabled (with @+RTS -T@, for example).
--
-- @since 4.6.0.0
foreign import ccall "getGCStatsEnabled" getGCStatsEnabled :: IO Bool

-- I'm probably violating a bucket of constraints here... oops.

-- | Statistics about memory usage and the garbage collector. Apart from
-- 'currentBytesUsed' and 'currentBytesSlop' all are cumulative values since
-- the program started.
--
-- @since 4.5.0.0
data GCStats = GCStats
    { -- | Total number of bytes allocated
    bytesAllocated :: !Int64
    -- | Number of garbage collections performed (any generation, major and
    -- minor)
    , numGcs :: !Int64
    -- | Maximum number of live bytes seen so far
    , maxBytesUsed :: !Int64
    -- | Number of byte usage samples taken, or equivalently
    -- the number of major GCs performed.
    , numByteUsageSamples :: !Int64
    -- | Sum of all byte usage samples, can be used with
    -- 'numByteUsageSamples' to calculate averages with
    -- arbitrary weighting (if you are sampling this record multiple
    -- times).
    , cumulativeBytesUsed :: !Int64
    -- | Number of bytes copied during GC
    , bytesCopied :: !Int64
    -- | Number of live bytes at the end of the last major GC
    , currentBytesUsed :: !Int64
    -- | Current number of bytes lost to slop
    , currentBytesSlop :: !Int64
    -- | Maximum number of bytes lost to slop at any one time so far
    , maxBytesSlop :: !Int64
    -- | Maximum number of megabytes allocated
    , peakMegabytesAllocated :: !Int64
    -- | CPU time spent running mutator threads.  This does not include
    -- any profiling overhead or initialization.
    , mutatorCpuSeconds :: !Double

    -- | Wall clock time spent running mutator threads.  This does not
    -- include initialization.
    , mutatorWallSeconds :: !Double
    -- | CPU time spent running GC
    , gcCpuSeconds :: !Double
    -- | Wall clock time spent running GC
    , gcWallSeconds :: !Double
    -- | Total CPU time elapsed since program start
    , cpuSeconds :: !Double
    -- | Total wall clock time elapsed since start
    , wallSeconds :: !Double
    -- | Number of bytes copied during GC, minus space held by mutable
    -- lists held by the capabilities.  Can be used with
    -- 'parMaxBytesCopied' to determine how well parallel GC utilized
    -- all cores.
    , parTotBytesCopied :: !Int64

    -- | Sum of number of bytes copied each GC by the most active GC
    -- thread each GC.  The ratio of 'parTotBytesCopied' divided by
    -- 'parMaxBytesCopied' approaches 1 for a maximally sequential
    -- run and approaches the number of threads (set by the RTS flag
    -- @-N@) for a maximally parallel run.
    , parMaxBytesCopied :: !Int64
    } deriving (Show, Read)

    {-
    , initCpuSeconds :: !Double
    , initWallSeconds :: !Double
    -}

-- | Retrieves garbage collection and memory statistics as of the last
-- garbage collection.  If you would like your statistics as recent as
-- possible, first run a 'System.Mem.performGC'.
--
-- @since 4.5.0.0
getGCStats :: IO GCStats
getGCStats = do
  statsEnabled <- getGCStatsEnabled
  unless statsEnabled .  ioError $ IOError
    Nothing
    UnsupportedOperation
    ""
    "getGCStats: GC stats not enabled. Use `+RTS -T -RTS' to enable them."
    Nothing
    Nothing
  allocaBytes ((144)) $ \p -> do
{-# LINE 123 "GHC.Stats.hsc" #-}
    getGCStats_ p
    bytesAllocated <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 125 "GHC.Stats.hsc" #-}
    numGcs <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 126 "GHC.Stats.hsc" #-}
    numByteUsageSamples <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) p
{-# LINE 127 "GHC.Stats.hsc" #-}
    maxBytesUsed <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) p
{-# LINE 128 "GHC.Stats.hsc" #-}
    cumulativeBytesUsed <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) p
{-# LINE 129 "GHC.Stats.hsc" #-}
    bytesCopied <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) p
{-# LINE 130 "GHC.Stats.hsc" #-}
    currentBytesUsed <- ((\hsc_ptr -> peekByteOff hsc_ptr 48)) p
{-# LINE 131 "GHC.Stats.hsc" #-}
    currentBytesSlop <- ((\hsc_ptr -> peekByteOff hsc_ptr 56)) p
{-# LINE 132 "GHC.Stats.hsc" #-}
    maxBytesSlop <- ((\hsc_ptr -> peekByteOff hsc_ptr 64)) p
{-# LINE 133 "GHC.Stats.hsc" #-}
    peakMegabytesAllocated <- ((\hsc_ptr -> peekByteOff hsc_ptr 72)) p
{-# LINE 134 "GHC.Stats.hsc" #-}
    {-
    initCpuSeconds <- (# peek GCStats, init_cpu_seconds) p
    initWallSeconds <- (# peek GCStats, init_wall_seconds) p
    -}
    mutatorCpuSeconds <- ((\hsc_ptr -> peekByteOff hsc_ptr 96)) p
{-# LINE 139 "GHC.Stats.hsc" #-}
    mutatorWallSeconds <- ((\hsc_ptr -> peekByteOff hsc_ptr 104)) p
{-# LINE 140 "GHC.Stats.hsc" #-}
    gcCpuSeconds <- ((\hsc_ptr -> peekByteOff hsc_ptr 112)) p
{-# LINE 141 "GHC.Stats.hsc" #-}
    gcWallSeconds <- ((\hsc_ptr -> peekByteOff hsc_ptr 120)) p
{-# LINE 142 "GHC.Stats.hsc" #-}
    cpuSeconds <- ((\hsc_ptr -> peekByteOff hsc_ptr 128)) p
{-# LINE 143 "GHC.Stats.hsc" #-}
    wallSeconds <- ((\hsc_ptr -> peekByteOff hsc_ptr 136)) p
{-# LINE 144 "GHC.Stats.hsc" #-}
    parTotBytesCopied <- ((\hsc_ptr -> peekByteOff hsc_ptr 80)) p
{-# LINE 145 "GHC.Stats.hsc" #-}
    parMaxBytesCopied <- ((\hsc_ptr -> peekByteOff hsc_ptr 88)) p
{-# LINE 146 "GHC.Stats.hsc" #-}
    return GCStats { .. }

{-

-- Nontrivial to implement: TaskStats needs arbitrarily large
-- amounts of memory, spark stats wants to use SparkCounters
-- but that needs a new rts/ header.

data TaskStats = TaskStats
    { taskMutCpuSeconds :: Int64
    , taskMutWallSeconds :: Int64
    , taskGcCpuSeconds :: Int64
    , taskGcWallSeconds :: Int64
    } deriving (Show, Read)

data SparkStats = SparkStats
    { sparksCreated :: Int64
    , sparksDud :: Int64
    , sparksOverflowed :: Int64
    , sparksConverted :: Int64
    , sparksGcd :: Int64
    , sparksFizzled :: Int64
    } deriving (Show, Read)

-- We also could get per-generation stats, which requires a
-- non-constant but at runtime known about of memory.

-}
