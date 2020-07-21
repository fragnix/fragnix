{-# LINE 1 "GHC.RTS.Flags.hsc" #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LINE 2 "GHC.RTS.Flags.hsc" #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Accessors to GHC RTS flags.
-- Descriptions of flags can be seen in
-- <https://www.haskell.org/ghc/docs/latest/html/users_guide/runtime-control.html GHC User's Guide>,
-- or by running RTS help message using @+RTS --help@.
--
-- @since 4.8.0.0
--
module GHC.RTS.Flags
  ( RtsTime
  , RtsNat
  , RTSFlags (..)
  , GiveGCStats (..)
  , GCFlags (..)
  , ConcFlags (..)
  , MiscFlags (..)
  , DebugFlags (..)
  , DoCostCentres (..)
  , CCFlags (..)
  , DoHeapProfile (..)
  , ProfFlags (..)
  , DoTrace (..)
  , TraceFlags (..)
  , TickyFlags (..)
  , getRTSFlags
  , getGCFlags
  , getConcFlags
  , getMiscFlags
  , getDebugFlags
  , getCCFlags
  , getProfFlags
  , getTraceFlags
  , getTickyFlags
  ) where


{-# LINE 39 "GHC.RTS.Flags.hsc" #-}

{-# LINE 40 "GHC.RTS.Flags.hsc" #-}

import Control.Applicative
import Control.Monad

import Foreign.C.String    (peekCString)
import Foreign.C.Types     (CChar, CInt)
import Foreign.Ptr         (Ptr, nullPtr)
import Foreign.Storable    (peekByteOff)

import GHC.Base
import GHC.Enum
import GHC.IO
import GHC.Real
import GHC.Show
import GHC.Word

-- | @'Time'@ is defined as a @'StgWord64'@ in @stg/Types.h@
--
-- @since 4.8.2.0
type RtsTime = Word64

-- | @'nat'@ defined in @rts/Types.h@
--
-- @since 4.8.2.0
type RtsNat = Word32
{-# LINE 65 "GHC.RTS.Flags.hsc" #-}

-- | Should we produce a summary of the garbage collector statistics after the
-- program has exited?
--
-- @since 4.8.2.0
data GiveGCStats
    = NoGCStats
    | CollectGCStats
    | OneLineGCStats
    | SummaryGCStats
    | VerboseGCStats
    deriving (Show)

instance Enum GiveGCStats where
    fromEnum NoGCStats      = 0
{-# LINE 80 "GHC.RTS.Flags.hsc" #-}
    fromEnum CollectGCStats = 1
{-# LINE 81 "GHC.RTS.Flags.hsc" #-}
    fromEnum OneLineGCStats = 2
{-# LINE 82 "GHC.RTS.Flags.hsc" #-}
    fromEnum SummaryGCStats = 3
{-# LINE 83 "GHC.RTS.Flags.hsc" #-}
    fromEnum VerboseGCStats = 4
{-# LINE 84 "GHC.RTS.Flags.hsc" #-}

    toEnum 0      = NoGCStats
{-# LINE 86 "GHC.RTS.Flags.hsc" #-}
    toEnum 1 = CollectGCStats
{-# LINE 87 "GHC.RTS.Flags.hsc" #-}
    toEnum 2 = OneLineGCStats
{-# LINE 88 "GHC.RTS.Flags.hsc" #-}
    toEnum 3 = SummaryGCStats
{-# LINE 89 "GHC.RTS.Flags.hsc" #-}
    toEnum 4 = VerboseGCStats
{-# LINE 90 "GHC.RTS.Flags.hsc" #-}
    toEnum e = errorWithoutStackTrace ("invalid enum for GiveGCStats: " ++ show e)

-- | Parameters of the garbage collector.
--
-- @since 4.8.0.0
data GCFlags = GCFlags
    { statsFile             :: Maybe FilePath
    , giveStats             :: GiveGCStats
    , maxStkSize            :: RtsNat
    , initialStkSize        :: RtsNat
    , stkChunkSize          :: RtsNat
    , stkChunkBufferSize    :: RtsNat
    , maxHeapSize           :: RtsNat
    , minAllocAreaSize      :: RtsNat
    , minOldGenSize         :: RtsNat
    , heapSizeSuggestion    :: RtsNat
    , heapSizeSuggestionAuto :: Bool
    , oldGenFactor          :: Double
    , pcFreeHeap            :: Double
    , generations           :: RtsNat
    , steps                 :: RtsNat
    , squeezeUpdFrames      :: Bool
    , compact               :: Bool -- ^ True <=> "compact all the time"
    , compactThreshold      :: Double
    , sweep                 :: Bool
      -- ^ use "mostly mark-sweep" instead of copying for the oldest generation
    , ringBell              :: Bool
    , frontpanel            :: Bool
    , idleGCDelayTime       :: RtsTime
    , doIdleGC              :: Bool
    , heapBase              :: Word -- ^ address to ask the OS for memory
    , allocLimitGrace       :: Word
    } deriving (Show)

-- | Parameters concerning context switching
--
-- @since 4.8.0.0
data ConcFlags = ConcFlags
    { ctxtSwitchTime  :: RtsTime
    , ctxtSwitchTicks :: Int
    } deriving (Show)

-- | Miscellaneous parameters
--
-- @since 4.8.0.0
data MiscFlags = MiscFlags
    { tickInterval          :: RtsTime
    , installSignalHandlers :: Bool
    , machineReadable       :: Bool
    , linkerMemBase         :: Word
      -- ^ address to ask the OS for memory for the linker, 0 ==> off
    } deriving (Show)

-- | Flags to control debugging output & extra checking in various
-- subsystems.
--
-- @since 4.8.0.0
data DebugFlags = DebugFlags
    { scheduler   :: Bool -- ^ 's'
    , interpreter :: Bool -- ^ 'i'
    , weak        :: Bool -- ^ 'w'
    , gccafs      :: Bool -- ^ 'G'
    , gc          :: Bool -- ^ 'g'
    , block_alloc :: Bool -- ^ 'b'
    , sanity      :: Bool -- ^ 'S'
    , stable      :: Bool -- ^ 't'
    , prof        :: Bool -- ^ 'p'
    , linker      :: Bool -- ^ 'l' the object linker
    , apply       :: Bool -- ^ 'a'
    , stm         :: Bool -- ^ 'm'
    , squeeze     :: Bool -- ^ 'z' stack squeezing & lazy blackholing
    , hpc         :: Bool -- ^ 'c' coverage
    , sparks      :: Bool -- ^ 'r'
    } deriving (Show)

-- | Should the RTS produce a cost-center summary?
--
-- @since 4.8.2.0
data DoCostCentres
    = CostCentresNone
    | CostCentresSummary
    | CostCentresVerbose
    | CostCentresAll
    | CostCentresXML
    deriving (Show)

instance Enum DoCostCentres where
    fromEnum CostCentresNone    = 0
{-# LINE 178 "GHC.RTS.Flags.hsc" #-}
    fromEnum CostCentresSummary = 1
{-# LINE 179 "GHC.RTS.Flags.hsc" #-}
    fromEnum CostCentresVerbose = 2
{-# LINE 180 "GHC.RTS.Flags.hsc" #-}
    fromEnum CostCentresAll     = 3
{-# LINE 181 "GHC.RTS.Flags.hsc" #-}
    fromEnum CostCentresXML     = 4
{-# LINE 182 "GHC.RTS.Flags.hsc" #-}

    toEnum 0    = CostCentresNone
{-# LINE 184 "GHC.RTS.Flags.hsc" #-}
    toEnum 1 = CostCentresSummary
{-# LINE 185 "GHC.RTS.Flags.hsc" #-}
    toEnum 2 = CostCentresVerbose
{-# LINE 186 "GHC.RTS.Flags.hsc" #-}
    toEnum 3     = CostCentresAll
{-# LINE 187 "GHC.RTS.Flags.hsc" #-}
    toEnum 4     = CostCentresXML
{-# LINE 188 "GHC.RTS.Flags.hsc" #-}
    toEnum e = errorWithoutStackTrace ("invalid enum for DoCostCentres: " ++ show e)

-- | Parameters pertaining to the cost-center profiler.
--
-- @since 4.8.0.0
data CCFlags = CCFlags
    { doCostCentres :: DoCostCentres
    , profilerTicks :: Int
    , msecsPerTick  :: Int
    } deriving (Show)

-- | What sort of heap profile are we collecting?
--
-- @since 4.8.2.0
data DoHeapProfile
    = NoHeapProfiling
    | HeapByCCS
    | HeapByMod
    | HeapByDescr
    | HeapByType
    | HeapByRetainer
    | HeapByLDV
    | HeapByClosureType
    deriving (Show)

instance Enum DoHeapProfile where
    fromEnum NoHeapProfiling   = 0
{-# LINE 215 "GHC.RTS.Flags.hsc" #-}
    fromEnum HeapByCCS         = 1
{-# LINE 216 "GHC.RTS.Flags.hsc" #-}
    fromEnum HeapByMod         = 2
{-# LINE 217 "GHC.RTS.Flags.hsc" #-}
    fromEnum HeapByDescr       = 4
{-# LINE 218 "GHC.RTS.Flags.hsc" #-}
    fromEnum HeapByType        = 5
{-# LINE 219 "GHC.RTS.Flags.hsc" #-}
    fromEnum HeapByRetainer    = 6
{-# LINE 220 "GHC.RTS.Flags.hsc" #-}
    fromEnum HeapByLDV         = 7
{-# LINE 221 "GHC.RTS.Flags.hsc" #-}
    fromEnum HeapByClosureType = 8
{-# LINE 222 "GHC.RTS.Flags.hsc" #-}

    toEnum 0    = NoHeapProfiling
{-# LINE 224 "GHC.RTS.Flags.hsc" #-}
    toEnum 1          = HeapByCCS
{-# LINE 225 "GHC.RTS.Flags.hsc" #-}
    toEnum 2          = HeapByMod
{-# LINE 226 "GHC.RTS.Flags.hsc" #-}
    toEnum 4        = HeapByDescr
{-# LINE 227 "GHC.RTS.Flags.hsc" #-}
    toEnum 5         = HeapByType
{-# LINE 228 "GHC.RTS.Flags.hsc" #-}
    toEnum 6     = HeapByRetainer
{-# LINE 229 "GHC.RTS.Flags.hsc" #-}
    toEnum 7          = HeapByLDV
{-# LINE 230 "GHC.RTS.Flags.hsc" #-}
    toEnum 8 = HeapByClosureType
{-# LINE 231 "GHC.RTS.Flags.hsc" #-}
    toEnum e = errorWithoutStackTrace ("invalid enum for DoHeapProfile: " ++ show e)

-- | Parameters of the cost-center profiler
--
-- @since 4.8.0.0
data ProfFlags = ProfFlags
    { doHeapProfile            :: DoHeapProfile
    , heapProfileInterval      :: RtsTime -- ^ time between samples
    , heapProfileIntervalTicks :: Word    -- ^ ticks between samples (derived)
    , includeTSOs              :: Bool
    , showCCSOnException       :: Bool
    , maxRetainerSetSize       :: Word
    , ccsLength                :: Word
    , modSelector              :: Maybe String
    , descrSelector            :: Maybe String
    , typeSelector             :: Maybe String
    , ccSelector               :: Maybe String
    , ccsSelector              :: Maybe String
    , retainerSelector         :: Maybe String
    , bioSelector              :: Maybe String
    } deriving (Show)

-- | Is event tracing enabled?
--
-- @since 4.8.2.0
data DoTrace
    = TraceNone      -- ^ no tracing
    | TraceEventLog  -- ^ send tracing events to the event log
    | TraceStderr    -- ^ send tracing events to @stderr@
    deriving (Show)

instance Enum DoTrace where
    fromEnum TraceNone     = 0
{-# LINE 264 "GHC.RTS.Flags.hsc" #-}
    fromEnum TraceEventLog = 1
{-# LINE 265 "GHC.RTS.Flags.hsc" #-}
    fromEnum TraceStderr   = 2
{-# LINE 266 "GHC.RTS.Flags.hsc" #-}

    toEnum 0     = TraceNone
{-# LINE 268 "GHC.RTS.Flags.hsc" #-}
    toEnum 1 = TraceEventLog
{-# LINE 269 "GHC.RTS.Flags.hsc" #-}
    toEnum 2   = TraceStderr
{-# LINE 270 "GHC.RTS.Flags.hsc" #-}
    toEnum e = errorWithoutStackTrace ("invalid enum for DoTrace: " ++ show e)

-- | Parameters pertaining to event tracing
--
-- @since 4.8.0.0
data TraceFlags = TraceFlags
    { tracing        :: DoTrace
    , timestamp      :: Bool -- ^ show timestamp in stderr output
    , traceScheduler :: Bool -- ^ trace scheduler events
    , traceGc        :: Bool -- ^ trace GC events
    , sparksSampled  :: Bool -- ^ trace spark events by a sampled method
    , sparksFull     :: Bool -- ^ trace spark events 100% accurately
    , user           :: Bool -- ^ trace user events (emitted from Haskell code)
    } deriving (Show)

-- | Parameters pertaining to ticky-ticky profiler
--
-- @since 4.8.0.0
data TickyFlags = TickyFlags
    { showTickyStats :: Bool
    , tickyFile      :: Maybe FilePath
    } deriving (Show)

-- | Parameters of the runtime system
--
-- @since 4.8.0.0
data RTSFlags = RTSFlags
    { gcFlags         :: GCFlags
    , concurrentFlags :: ConcFlags
    , miscFlags       :: MiscFlags
    , debugFlags      :: DebugFlags
    , costCentreFlags :: CCFlags
    , profilingFlags  :: ProfFlags
    , traceFlags      :: TraceFlags
    , tickyFlags      :: TickyFlags
    } deriving (Show)

foreign import ccall safe "getGcFlags"
  getGcFlagsPtr :: IO (Ptr ())

foreign import ccall safe "getConcFlags"
  getConcFlagsPtr :: IO (Ptr ())

foreign import ccall safe "getMiscFlags"
  getMiscFlagsPtr :: IO (Ptr ())

foreign import ccall safe "getDebugFlags"
  getDebugFlagsPtr :: IO (Ptr ())

foreign import ccall safe "getCcFlags"
  getCcFlagsPtr :: IO (Ptr ())

foreign import ccall safe "getProfFlags" getProfFlagsPtr :: IO (Ptr ())

foreign import ccall safe "getTraceFlags"
  getTraceFlagsPtr :: IO (Ptr ())

foreign import ccall safe "getTickyFlags"
  getTickyFlagsPtr :: IO (Ptr ())

getRTSFlags :: IO RTSFlags
getRTSFlags = do
  RTSFlags <$> getGCFlags
           <*> getConcFlags
           <*> getMiscFlags
           <*> getDebugFlags
           <*> getCCFlags
           <*> getProfFlags
           <*> getTraceFlags
           <*> getTickyFlags

peekFilePath :: Ptr () -> IO (Maybe FilePath)
peekFilePath ptr
  | ptr == nullPtr = return Nothing
  | otherwise      = return (Just "<filepath>")

-- | Read a NUL terminated string. Return Nothing in case of a NULL pointer.
peekCStringOpt :: Ptr CChar -> IO (Maybe String)
peekCStringOpt ptr
  | ptr == nullPtr = return Nothing
  | otherwise      = Just <$> peekCString ptr

getGCFlags :: IO GCFlags
getGCFlags = do
  ptr <- getGcFlagsPtr
  GCFlags <$> (peekFilePath =<< (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr)
{-# LINE 356 "GHC.RTS.Flags.hsc" #-}
          <*> (toEnum . fromIntegral <$>
                ((\hsc_ptr -> peekByteOff hsc_ptr 8) ptr :: IO RtsNat))
{-# LINE 358 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 12) ptr
{-# LINE 359 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 360 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 361 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 362 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 28) ptr
{-# LINE 363 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 364 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 365 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 44) ptr
{-# LINE 366 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 367 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
{-# LINE 368 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
{-# LINE 369 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 72) ptr
{-# LINE 370 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 76) ptr
{-# LINE 371 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 80) ptr
{-# LINE 372 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 84) ptr
{-# LINE 373 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 88) ptr
{-# LINE 374 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 96) ptr
{-# LINE 375 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 100) ptr
{-# LINE 376 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 104) ptr
{-# LINE 377 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 112) ptr
{-# LINE 378 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 120) ptr
{-# LINE 379 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 128) ptr
{-# LINE 380 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 136) ptr
{-# LINE 381 "GHC.RTS.Flags.hsc" #-}

getConcFlags :: IO ConcFlags
getConcFlags = do
  ptr <- getConcFlagsPtr
  ConcFlags <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 386 "GHC.RTS.Flags.hsc" #-}
            <*> (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 387 "GHC.RTS.Flags.hsc" #-}

getMiscFlags :: IO MiscFlags
getMiscFlags = do
  ptr <- getMiscFlagsPtr
  MiscFlags <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 392 "GHC.RTS.Flags.hsc" #-}
            <*> (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 393 "GHC.RTS.Flags.hsc" #-}
            <*> (\hsc_ptr -> peekByteOff hsc_ptr 12) ptr
{-# LINE 394 "GHC.RTS.Flags.hsc" #-}
            <*> (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 395 "GHC.RTS.Flags.hsc" #-}

getDebugFlags :: IO DebugFlags
getDebugFlags = do
  ptr <- getDebugFlagsPtr
  DebugFlags <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 400 "GHC.RTS.Flags.hsc" #-}
             <*> (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 401 "GHC.RTS.Flags.hsc" #-}
             <*> (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 402 "GHC.RTS.Flags.hsc" #-}
             <*> (\hsc_ptr -> peekByteOff hsc_ptr 12) ptr
{-# LINE 403 "GHC.RTS.Flags.hsc" #-}
             <*> (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 404 "GHC.RTS.Flags.hsc" #-}
             <*> (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 405 "GHC.RTS.Flags.hsc" #-}
             <*> (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 406 "GHC.RTS.Flags.hsc" #-}
             <*> (\hsc_ptr -> peekByteOff hsc_ptr 28) ptr
{-# LINE 407 "GHC.RTS.Flags.hsc" #-}
             <*> (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 408 "GHC.RTS.Flags.hsc" #-}
             <*> (\hsc_ptr -> peekByteOff hsc_ptr 36) ptr
{-# LINE 409 "GHC.RTS.Flags.hsc" #-}
             <*> (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 410 "GHC.RTS.Flags.hsc" #-}
             <*> (\hsc_ptr -> peekByteOff hsc_ptr 44) ptr
{-# LINE 411 "GHC.RTS.Flags.hsc" #-}
             <*> (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 412 "GHC.RTS.Flags.hsc" #-}
             <*> (\hsc_ptr -> peekByteOff hsc_ptr 52) ptr
{-# LINE 413 "GHC.RTS.Flags.hsc" #-}
             <*> (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
{-# LINE 414 "GHC.RTS.Flags.hsc" #-}

getCCFlags :: IO CCFlags
getCCFlags = do
  ptr <- getCcFlagsPtr
  CCFlags <$> (toEnum . fromIntegral
                <$> ((\hsc_ptr -> peekByteOff hsc_ptr 0) ptr :: IO RtsNat))
{-# LINE 420 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 421 "GHC.RTS.Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 422 "GHC.RTS.Flags.hsc" #-}

getProfFlags :: IO ProfFlags
getProfFlags = do
  ptr <- getProfFlagsPtr
  ProfFlags <$> (toEnum <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr)
{-# LINE 427 "GHC.RTS.Flags.hsc" #-}
            <*> (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 428 "GHC.RTS.Flags.hsc" #-}
            <*> (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 429 "GHC.RTS.Flags.hsc" #-}
            <*> (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 430 "GHC.RTS.Flags.hsc" #-}
            <*> (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 431 "GHC.RTS.Flags.hsc" #-}
            <*> (\hsc_ptr -> peekByteOff hsc_ptr 28) ptr
{-# LINE 432 "GHC.RTS.Flags.hsc" #-}
            <*> (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 433 "GHC.RTS.Flags.hsc" #-}
            <*> (peekCStringOpt =<< (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr)
{-# LINE 434 "GHC.RTS.Flags.hsc" #-}
            <*> (peekCStringOpt =<< (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr)
{-# LINE 435 "GHC.RTS.Flags.hsc" #-}
            <*> (peekCStringOpt =<< (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr)
{-# LINE 436 "GHC.RTS.Flags.hsc" #-}
            <*> (peekCStringOpt =<< (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr)
{-# LINE 437 "GHC.RTS.Flags.hsc" #-}
            <*> (peekCStringOpt =<< (\hsc_ptr -> peekByteOff hsc_ptr 72) ptr)
{-# LINE 438 "GHC.RTS.Flags.hsc" #-}
            <*> (peekCStringOpt =<< (\hsc_ptr -> peekByteOff hsc_ptr 80) ptr)
{-# LINE 439 "GHC.RTS.Flags.hsc" #-}
            <*> (peekCStringOpt =<< (\hsc_ptr -> peekByteOff hsc_ptr 88) ptr)
{-# LINE 440 "GHC.RTS.Flags.hsc" #-}

getTraceFlags :: IO TraceFlags
getTraceFlags = do
  ptr <- getTraceFlagsPtr
  TraceFlags <$> (toEnum . fromIntegral
                   <$> ((\hsc_ptr -> peekByteOff hsc_ptr 0) ptr :: IO CInt))
{-# LINE 446 "GHC.RTS.Flags.hsc" #-}
             <*> (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 447 "GHC.RTS.Flags.hsc" #-}
             <*> (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 448 "GHC.RTS.Flags.hsc" #-}
             <*> (\hsc_ptr -> peekByteOff hsc_ptr 12) ptr
{-# LINE 449 "GHC.RTS.Flags.hsc" #-}
             <*> (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 450 "GHC.RTS.Flags.hsc" #-}
             <*> (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 451 "GHC.RTS.Flags.hsc" #-}
             <*> (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 452 "GHC.RTS.Flags.hsc" #-}

getTickyFlags :: IO TickyFlags
getTickyFlags = do
  ptr <- getTickyFlagsPtr
  TickyFlags <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 457 "GHC.RTS.Flags.hsc" #-}
             <*> (peekFilePath =<< (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr)
{-# LINE 458 "GHC.RTS.Flags.hsc" #-}
