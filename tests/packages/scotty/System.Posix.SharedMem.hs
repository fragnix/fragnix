{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-d76e0d17/build/System/Posix/SharedMem.hs" #-}
{-# LINE 1 "System/Posix/SharedMem.hsc" #-}

{-# LINE 4 "System/Posix/SharedMem.hsc" #-}
{-# LANGUAGE Trustworthy #-}

{-# LINE 6 "System/Posix/SharedMem.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.SharedMem
-- Copyright   :  (c) Daniel Franke 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires POSIX)
--
-- POSIX shared memory support.
--
-----------------------------------------------------------------------------

module System.Posix.SharedMem
    (ShmOpenFlags(..), shmOpen, shmUnlink)
    where


{-# LINE 25 "System/Posix/SharedMem.hsc" #-}


{-# LINE 27 "System/Posix/SharedMem.hsc" #-}

{-# LINE 28 "System/Posix/SharedMem.hsc" #-}

{-# LINE 29 "System/Posix/SharedMem.hsc" #-}

import System.Posix.Types

{-# LINE 32 "System/Posix/SharedMem.hsc" #-}
import Foreign.C

{-# LINE 34 "System/Posix/SharedMem.hsc" #-}

{-# LINE 35 "System/Posix/SharedMem.hsc" #-}
import Data.Bits

{-# LINE 37 "System/Posix/SharedMem.hsc" #-}

data ShmOpenFlags = ShmOpenFlags 
    { shmReadWrite :: Bool,
      -- ^ If true, open the shm object read-write rather than read-only. 
      shmCreate :: Bool,
      -- ^ If true, create the shm object if it does not exist. 
      shmExclusive :: Bool,
      -- ^ If true, throw an exception if the shm object already exists.
      shmTrunc :: Bool
      -- ^ If true, wipe the contents of the shm object after opening it.
    }

-- | Open a shared memory object with the given name, flags, and mode.
shmOpen :: String -> ShmOpenFlags -> FileMode -> IO Fd

{-# LINE 52 "System/Posix/SharedMem.hsc" #-}
shmOpen name flags mode =
    do cflags0 <- return 0
       cflags1 <- return $ cflags0 .|. (if shmReadWrite flags
                                        then 2
{-# LINE 56 "System/Posix/SharedMem.hsc" #-}
                                        else 0)
{-# LINE 57 "System/Posix/SharedMem.hsc" #-}
       cflags2 <- return $ cflags1 .|. (if shmCreate flags then 64 
{-# LINE 58 "System/Posix/SharedMem.hsc" #-}
                                        else 0)
       cflags3 <- return $ cflags2 .|. (if shmExclusive flags 
                                        then 128 
{-# LINE 61 "System/Posix/SharedMem.hsc" #-}
                                        else 0)
       cflags4 <- return $ cflags3 .|. (if shmTrunc flags then 512 
{-# LINE 63 "System/Posix/SharedMem.hsc" #-}
                                        else 0)
       withCAString name (shmOpen' cflags4)
    where shmOpen' cflags cname =
              do fd <- throwErrnoIfMinus1 "shmOpen" $ 
                       shm_open cname cflags mode
                 return $ Fd fd

{-# LINE 72 "System/Posix/SharedMem.hsc" #-}

-- | Delete the shared memory object with the given name.
shmUnlink :: String -> IO ()

{-# LINE 76 "System/Posix/SharedMem.hsc" #-}
shmUnlink name = withCAString name shmUnlink'
    where shmUnlink' cname =
              throwErrnoIfMinus1_ "shmUnlink" $ shm_unlink cname

{-# LINE 82 "System/Posix/SharedMem.hsc" #-}


{-# LINE 84 "System/Posix/SharedMem.hsc" #-}
foreign import ccall unsafe "shm_open"
        shm_open :: CString -> CInt -> CMode -> IO CInt

{-# LINE 87 "System/Posix/SharedMem.hsc" #-}


{-# LINE 89 "System/Posix/SharedMem.hsc" #-}
foreign import ccall unsafe "shm_unlink"
        shm_unlink :: CString -> IO CInt

{-# LINE 92 "System/Posix/SharedMem.hsc" #-}
