{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-261cd265/build/System/Posix/Directory/Common.hs" #-}
{-# LINE 1 "System/Posix/Directory/Common.hsc" #-}

{-# LINE 2 "System/Posix/Directory/Common.hsc" #-}
{-# LANGUAGE Safe #-}

{-# LINE 6 "System/Posix/Directory/Common.hsc" #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Directory.Common
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX directory support
--
-----------------------------------------------------------------------------


{-# LINE 22 "System/Posix/Directory/Common.hsc" #-}

module System.Posix.Directory.Common (
       DirStream(..), CDir, CDirent, DirStreamOffset(..),
       rewindDirStream,
       closeDirStream,

{-# LINE 28 "System/Posix/Directory/Common.hsc" #-}
       seekDirStream,

{-# LINE 30 "System/Posix/Directory/Common.hsc" #-}

{-# LINE 31 "System/Posix/Directory/Common.hsc" #-}
       tellDirStream,

{-# LINE 33 "System/Posix/Directory/Common.hsc" #-}
       changeWorkingDirectoryFd,
  ) where

import System.Posix.Types
import Foreign
import Foreign.C

newtype DirStream = DirStream (Ptr CDir)

data {-# CTYPE "DIR" #-} CDir
data {-# CTYPE "struct dirent" #-} CDirent

-- | @rewindDirStream dp@ calls @rewinddir@ to reposition
--   the directory stream @dp@ at the beginning of the directory.
rewindDirStream :: DirStream -> IO ()
rewindDirStream (DirStream dirp) = c_rewinddir dirp

foreign import ccall unsafe "rewinddir"
   c_rewinddir :: Ptr CDir -> IO ()

-- | @closeDirStream dp@ calls @closedir@ to close
--   the directory stream @dp@.
closeDirStream :: DirStream -> IO ()
closeDirStream (DirStream dirp) = do
  throwErrnoIfMinus1Retry_ "closeDirStream" (c_closedir dirp)

foreign import ccall unsafe "closedir"
   c_closedir :: Ptr CDir -> IO CInt

newtype DirStreamOffset = DirStreamOffset COff


{-# LINE 65 "System/Posix/Directory/Common.hsc" #-}
seekDirStream :: DirStream -> DirStreamOffset -> IO ()
seekDirStream (DirStream dirp) (DirStreamOffset off) =
  c_seekdir dirp (fromIntegral off) -- TODO: check for CLong/COff overflow

foreign import ccall unsafe "seekdir"
  c_seekdir :: Ptr CDir -> CLong -> IO ()

{-# LINE 72 "System/Posix/Directory/Common.hsc" #-}


{-# LINE 74 "System/Posix/Directory/Common.hsc" #-}
tellDirStream :: DirStream -> IO DirStreamOffset
tellDirStream (DirStream dirp) = do
  off <- c_telldir dirp
  return (DirStreamOffset (fromIntegral off)) -- TODO: check for overflow

foreign import ccall unsafe "telldir"
  c_telldir :: Ptr CDir -> IO CLong

{-# LINE 82 "System/Posix/Directory/Common.hsc" #-}

changeWorkingDirectoryFd :: Fd -> IO ()
changeWorkingDirectoryFd (Fd fd) =
  throwErrnoIfMinus1Retry_ "changeWorkingDirectoryFd" (c_fchdir fd)

foreign import ccall unsafe "fchdir"
  c_fchdir :: CInt -> IO CInt
