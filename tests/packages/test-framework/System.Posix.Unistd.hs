{-# LINE 1 "dist/dist-sandbox-235ea54e/build/System/Posix/Unistd.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                                 






                      






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "dist/dist-sandbox-235ea54e/build/System/Posix/Unistd.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/System/Posix/Unistd.hs" #-}
{-# LINE 1 "System/Posix/Unistd.hsc" #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LINE 2 "System/Posix/Unistd.hsc" #-}

{-# LINE 5 "System/Posix/Unistd.hsc" #-}
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


{-# LINE 53 "System/Posix/Unistd.hsc" #-}

import Foreign.C.Error
import Foreign.C.String ( peekCString )
import Foreign.C.Types
import Foreign
import System.Posix.Internals

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
{-# LINE 74 "System/Posix/Unistd.hsc" #-}
    throwErrnoIfMinus1_ "getSystemID" (c_uname p_sid)
    sysN <- peekCString (((\hsc_ptr -> hsc_ptr `plusPtr` 0)) p_sid)
{-# LINE 76 "System/Posix/Unistd.hsc" #-}
    node <- peekCString (((\hsc_ptr -> hsc_ptr `plusPtr` 65)) p_sid)
{-# LINE 77 "System/Posix/Unistd.hsc" #-}
    rel  <- peekCString (((\hsc_ptr -> hsc_ptr `plusPtr` 130)) p_sid)
{-# LINE 78 "System/Posix/Unistd.hsc" #-}
    ver  <- peekCString (((\hsc_ptr -> hsc_ptr `plusPtr` 195)) p_sid)
{-# LINE 79 "System/Posix/Unistd.hsc" #-}
    mach <- peekCString (((\hsc_ptr -> hsc_ptr `plusPtr` 260)) p_sid)
{-# LINE 80 "System/Posix/Unistd.hsc" #-}
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


{-# LINE 111 "System/Posix/Unistd.hsc" #-}

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

{-# LINE 125 "System/Posix/Unistd.hsc" #-}
usleep usecs = nanosleep (fromIntegral usecs * 1000)

{-# LINE 142 "System/Posix/Unistd.hsc" #-}

-- | Sleep for the specified duration (in nanoseconds)
--
-- /GHC Note/: the comment for 'usleep' also applies here.
nanosleep :: Integer -> IO ()

{-# LINE 150 "System/Posix/Unistd.hsc" #-}
nanosleep 0 = return ()
nanosleep nsecs = do
  allocaBytes (16) $ \pts1 -> do
{-# LINE 153 "System/Posix/Unistd.hsc" #-}
  allocaBytes (16) $ \pts2 -> do
{-# LINE 154 "System/Posix/Unistd.hsc" #-}
     let (tv_sec0, tv_nsec0) = nsecs `divMod` 1000000000
     let 
       loop tv_sec tv_nsec = do
         ((\hsc_ptr -> pokeByteOff hsc_ptr 0))  pts1 tv_sec
{-# LINE 158 "System/Posix/Unistd.hsc" #-}
         ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) pts1 tv_nsec
{-# LINE 159 "System/Posix/Unistd.hsc" #-}
         res <- c_nanosleep pts1 pts2
         if res == 0
            then return ()
            else do errno <- getErrno
                    if errno == eINTR
                       then do
                           tv_sec'  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))  pts2
{-# LINE 166 "System/Posix/Unistd.hsc" #-}
                           tv_nsec' <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) pts2
{-# LINE 167 "System/Posix/Unistd.hsc" #-}
                           loop tv_sec' tv_nsec'
                       else throwErrno "nanosleep"
     loop (fromIntegral tv_sec0 :: CTime) (fromIntegral tv_nsec0 :: CTime)

data CTimeSpec

foreign import ccall safe "__hsunix_nanosleep" 
  c_nanosleep :: Ptr CTimeSpec -> Ptr CTimeSpec -> IO CInt

{-# LINE 176 "System/Posix/Unistd.hsc" #-}

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
{-# LINE 194 "System/Posix/Unistd.hsc" #-}
      ChildLimit    -> sysconf (1)
{-# LINE 195 "System/Posix/Unistd.hsc" #-}
      ClockTick	    -> sysconf (2)
{-# LINE 196 "System/Posix/Unistd.hsc" #-}
      GroupLimit    -> sysconf (3)
{-# LINE 197 "System/Posix/Unistd.hsc" #-}
      OpenFileLimit -> sysconf (4)
{-# LINE 198 "System/Posix/Unistd.hsc" #-}
      PosixVersion  -> sysconf (29)
{-# LINE 199 "System/Posix/Unistd.hsc" #-}
      HasSavedIDs   -> sysconf (8)
{-# LINE 200 "System/Posix/Unistd.hsc" #-}
      HasJobControl -> sysconf (7)
{-# LINE 201 "System/Posix/Unistd.hsc" #-}

sysconf :: CInt -> IO Integer
sysconf n = do 
  r <- throwErrnoIfMinus1 "getSysVar" (c_sysconf n)
  return (fromIntegral r)

foreign import ccall unsafe "sysconf"
  c_sysconf :: CInt -> IO CLong
