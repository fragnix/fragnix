{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-d76e0d17/build/System/Posix/Unistd.hs" #-}
{-# LINE 1 "System/Posix/Unistd.hsc" #-}
{-# LANGUAGE CApiFFI #-}
{-# LINE 2 "System/Posix/Unistd.hsc" #-}
{-# LANGUAGE NondecreasingIndentation #-}

{-# LINE 6 "System/Posix/Unistd.hsc" #-}
{-# LANGUAGE Trustworthy #-}

{-# LINE 8 "System/Posix/Unistd.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Unistd
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX miscellaneous stuff, mostly from unistd.h
--
-----------------------------------------------------------------------------

module System.Posix.Unistd (
    -- * System environment
    SystemID(..),
    getSystemID,

    SysVar(..),
    getSysVar,

    -- * Sleeping
    sleep, usleep, nanosleep,

    -- * File synchronisation
    fileSynchronise,
    fileSynchroniseDataOnly,

  {-
    ToDo from unistd.h:
      confstr,
      lots of sysconf variables

    -- use Network.BSD
    gethostid, gethostname

    -- should be in System.Posix.Files?
    pathconf, fpathconf,

    -- System.Posix.Signals
    ualarm,

    -- System.Posix.IO
    read, write,

    -- should be in System.Posix.User?
    getEffectiveUserName,
-}
  ) where


{-# LINE 60 "System/Posix/Unistd.hsc" #-}

import Foreign.C.Error
import Foreign.C.String ( peekCString )
import Foreign.C.Types
import Foreign
import System.Posix.Types
import System.Posix.Internals


{-# LINE 72 "System/Posix/Unistd.hsc" #-}

-- -----------------------------------------------------------------------------
-- System environment (uname())

data SystemID =
  SystemID { systemName :: String
           , nodeName   :: String
           , release    :: String
           , version    :: String
           , machine    :: String
           }

getSystemID :: IO SystemID
getSystemID = do
  allocaBytes (390) $ \p_sid -> do
{-# LINE 87 "System/Posix/Unistd.hsc" #-}
    throwErrnoIfMinus1_ "getSystemID" (c_uname p_sid)
    sysN <- peekCString (((\hsc_ptr -> hsc_ptr `plusPtr` 0)) p_sid)
{-# LINE 89 "System/Posix/Unistd.hsc" #-}
    node <- peekCString (((\hsc_ptr -> hsc_ptr `plusPtr` 65)) p_sid)
{-# LINE 90 "System/Posix/Unistd.hsc" #-}
    rel  <- peekCString (((\hsc_ptr -> hsc_ptr `plusPtr` 130)) p_sid)
{-# LINE 91 "System/Posix/Unistd.hsc" #-}
    ver  <- peekCString (((\hsc_ptr -> hsc_ptr `plusPtr` 195)) p_sid)
{-# LINE 92 "System/Posix/Unistd.hsc" #-}
    mach <- peekCString (((\hsc_ptr -> hsc_ptr `plusPtr` 260)) p_sid)
{-# LINE 93 "System/Posix/Unistd.hsc" #-}
    return (SystemID { systemName = sysN,
                       nodeName   = node,
                       release    = rel,
                       version    = ver,
                       machine    = mach
                     })

foreign import ccall unsafe "uname"
   c_uname :: Ptr CUtsname -> IO CInt

-- -----------------------------------------------------------------------------
-- sleeping

-- | Sleep for the specified duration (in seconds).  Returns the time remaining
-- (if the sleep was interrupted by a signal, for example).
--
-- /GHC Note/: 'Control.Concurrent.threadDelay' is a better choice.  Since GHC
-- uses signals for its internal clock, a call to 'sleep' will usually be
-- interrupted immediately.  That makes 'sleep' unusable in a program compiled
-- with GHC, unless the RTS timer is disabled (with @+RTS -V0@).  Furthermore,
-- without the @-threaded@ option, 'sleep' will block all other user threads.
-- Even with the @-threaded@ option, 'sleep' requires a full OS thread to
-- itself.  'Control.Concurrent.threadDelay' has none of these shortcomings.
--
sleep :: Int -> IO Int
sleep 0 = return 0
sleep secs = do r <- c_sleep (fromIntegral secs); return (fromIntegral r)


{-# LINE 122 "System/Posix/Unistd.hsc" #-}
{-# WARNING sleep "This function has several shortcomings (see documentation). Please consider using Control.Concurrent.threadDelay instead." #-}

{-# LINE 124 "System/Posix/Unistd.hsc" #-}

foreign import ccall safe "sleep"
  c_sleep :: CUInt -> IO CUInt

-- | Sleep for the specified duration (in microseconds).
--
-- /GHC Note/: 'Control.Concurrent.threadDelay' is a better choice.
-- Without the @-threaded@ option, 'usleep' will block all other user
-- threads.  Even with the @-threaded@ option, 'usleep' requires a
-- full OS thread to itself.  'Control.Concurrent.threadDelay' has
-- neither of these shortcomings.
--
usleep :: Int -> IO ()

{-# LINE 138 "System/Posix/Unistd.hsc" #-}
usleep usecs = nanosleep (fromIntegral usecs * 1000)

{-# LINE 155 "System/Posix/Unistd.hsc" #-}

-- | Sleep for the specified duration (in nanoseconds)
--
-- /GHC Note/: the comment for 'usleep' also applies here.
nanosleep :: Integer -> IO ()

{-# LINE 163 "System/Posix/Unistd.hsc" #-}
nanosleep 0 = return ()
nanosleep nsecs = do
  allocaBytes (16) $ \pts1 -> do
{-# LINE 166 "System/Posix/Unistd.hsc" #-}
  allocaBytes (16) $ \pts2 -> do
{-# LINE 167 "System/Posix/Unistd.hsc" #-}
     let (tv_sec0, tv_nsec0) = nsecs `divMod` 1000000000
     let
       loop tv_sec tv_nsec = do
         ((\hsc_ptr -> pokeByteOff hsc_ptr 0))  pts1 tv_sec
{-# LINE 171 "System/Posix/Unistd.hsc" #-}
         ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) pts1 tv_nsec
{-# LINE 172 "System/Posix/Unistd.hsc" #-}
         res <- c_nanosleep pts1 pts2
         if res == 0
            then return ()
            else do errno <- getErrno
                    if errno == eINTR
                       then do
                           tv_sec'  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))  pts2
{-# LINE 179 "System/Posix/Unistd.hsc" #-}
                           tv_nsec' <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) pts2
{-# LINE 180 "System/Posix/Unistd.hsc" #-}
                           loop tv_sec' tv_nsec'
                       else throwErrno "nanosleep"
     loop (fromIntegral tv_sec0 :: CTime) (fromIntegral tv_nsec0 :: CTime)

data CTimeSpec

foreign import ccall safe "__hsunix_nanosleep"
  c_nanosleep :: Ptr CTimeSpec -> Ptr CTimeSpec -> IO CInt

{-# LINE 189 "System/Posix/Unistd.hsc" #-}

-- -----------------------------------------------------------------------------
-- System variables

data SysVar = ArgumentLimit
            | ChildLimit
            | ClockTick
            | GroupLimit
            | OpenFileLimit
            | PosixVersion
            | HasSavedIDs
            | HasJobControl
        -- ToDo: lots more

getSysVar :: SysVar -> IO Integer
getSysVar v =
    case v of
      ArgumentLimit -> sysconf (0)
{-# LINE 207 "System/Posix/Unistd.hsc" #-}
      ChildLimit    -> sysconf (1)
{-# LINE 208 "System/Posix/Unistd.hsc" #-}
      ClockTick     -> sysconf (2)
{-# LINE 209 "System/Posix/Unistd.hsc" #-}
      GroupLimit    -> sysconf (3)
{-# LINE 210 "System/Posix/Unistd.hsc" #-}
      OpenFileLimit -> sysconf (4)
{-# LINE 211 "System/Posix/Unistd.hsc" #-}
      PosixVersion  -> sysconf (29)
{-# LINE 212 "System/Posix/Unistd.hsc" #-}
      HasSavedIDs   -> sysconf (8)
{-# LINE 213 "System/Posix/Unistd.hsc" #-}
      HasJobControl -> sysconf (7)
{-# LINE 214 "System/Posix/Unistd.hsc" #-}

sysconf :: CInt -> IO Integer
sysconf n = do
  r <- throwErrnoIfMinus1 "getSysVar" (c_sysconf n)
  return (fromIntegral r)

foreign import ccall unsafe "sysconf"
  c_sysconf :: CInt -> IO CLong

-- -----------------------------------------------------------------------------
-- File synchronization

-- | Performs @fsync(2)@ operation on file-descriptor.
--
-- Throws 'IOError' (\"unsupported operation\") if platform does not
-- provide @fsync(2)@ (use @#if HAVE_FSYNC@ CPP guard to
-- detect availability).
--
-- /Since: 2.7.1.0/
fileSynchronise :: Fd -> IO ()

{-# LINE 235 "System/Posix/Unistd.hsc" #-}
fileSynchronise fd = do
  throwErrnoIfMinus1_ "fileSynchronise" (c_fsync fd)

foreign import capi safe "unistd.h fsync"
  c_fsync :: Fd -> IO CInt

{-# LINE 246 "System/Posix/Unistd.hsc" #-}

-- | Performs @fdatasync(2)@ operation on file-descriptor.
--
-- Throws 'IOError' (\"unsupported operation\") if platform does not
-- provide @fdatasync(2)@ (use @#if HAVE_FDATASYNC@ CPP guard to
-- detect availability).
--
-- /Since: 2.7.1.0/
fileSynchroniseDataOnly :: Fd -> IO ()

{-# LINE 256 "System/Posix/Unistd.hsc" #-}
fileSynchroniseDataOnly fd = do
  throwErrnoIfMinus1_ "fileSynchroniseDataOnly" (c_fdatasync fd)

foreign import capi safe "unistd.h fdatasync"
  c_fdatasync :: Fd -> IO CInt

{-# LINE 267 "System/Posix/Unistd.hsc" #-}
