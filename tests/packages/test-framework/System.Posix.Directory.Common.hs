{-# LINE 1 "dist/dist-sandbox-235ea54e/build/System/Posix/Directory/Common.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                                 






                      






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "dist/dist-sandbox-235ea54e/build/System/Posix/Directory/Common.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/System/Posix/Directory/Common.hs" #-}
{-# LINE 1 "System/Posix/Directory/Common.hsc" #-}

{-# LINE 4 "System/Posix/Directory/Common.hsc" #-}

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


{-# LINE 20 "System/Posix/Directory/Common.hsc" #-}

module System.Posix.Directory.Common (
       DirStream(..), CDir, CDirent, DirStreamOffset(..),
       rewindDirStream,
       closeDirStream,

{-# LINE 26 "System/Posix/Directory/Common.hsc" #-}
       seekDirStream,

{-# LINE 28 "System/Posix/Directory/Common.hsc" #-}

{-# LINE 29 "System/Posix/Directory/Common.hsc" #-}
       tellDirStream,

{-# LINE 31 "System/Posix/Directory/Common.hsc" #-}
       changeWorkingDirectoryFd,
  ) where

import System.Posix.Types
import Foreign
import Foreign.C

newtype DirStream = DirStream (Ptr CDir)

type CDir       = ()
type CDirent    = ()

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


{-# LINE 63 "System/Posix/Directory/Common.hsc" #-}
seekDirStream :: DirStream -> DirStreamOffset -> IO ()
seekDirStream (DirStream dirp) (DirStreamOffset off) =
  c_seekdir dirp off

foreign import ccall unsafe "seekdir"
  c_seekdir :: Ptr CDir -> COff -> IO ()

{-# LINE 70 "System/Posix/Directory/Common.hsc" #-}


{-# LINE 72 "System/Posix/Directory/Common.hsc" #-}
tellDirStream :: DirStream -> IO DirStreamOffset
tellDirStream (DirStream dirp) = do
  off <- c_telldir dirp
  return (DirStreamOffset off)

foreign import ccall unsafe "telldir"
  c_telldir :: Ptr CDir -> IO COff

{-# LINE 80 "System/Posix/Directory/Common.hsc" #-}

changeWorkingDirectoryFd :: Fd -> IO ()
changeWorkingDirectoryFd (Fd fd) = 
  throwErrnoIfMinus1Retry_ "changeWorkingDirectoryFd" (c_fchdir fd)

foreign import ccall unsafe "fchdir"
  c_fchdir :: CInt -> IO CInt
