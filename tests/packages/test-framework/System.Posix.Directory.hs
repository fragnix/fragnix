{-# LINE 1 "dist/dist-sandbox-235ea54e/build/System/Posix/Directory.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                                 






                      






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "dist/dist-sandbox-235ea54e/build/System/Posix/Directory.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/System/Posix/Directory.hs" #-}
{-# LINE 1 "System/Posix/Directory.hsc" #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LINE 2 "System/Posix/Directory.hsc" #-}

{-# LINE 5 "System/Posix/Directory.hsc" #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Directory
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- String-based POSIX directory support
--
-----------------------------------------------------------------------------


{-# LINE 21 "System/Posix/Directory.hsc" #-}

module System.Posix.Directory (
   -- * Creating and removing directories
   createDirectory, removeDirectory,

   -- * Reading directories
   DirStream,
   openDirStream,
   readDirStream,
   rewindDirStream,   
   closeDirStream,
   DirStreamOffset,

{-# LINE 34 "System/Posix/Directory.hsc" #-}
   tellDirStream,

{-# LINE 36 "System/Posix/Directory.hsc" #-}

{-# LINE 37 "System/Posix/Directory.hsc" #-}
   seekDirStream,

{-# LINE 39 "System/Posix/Directory.hsc" #-}

   -- * The working dirctory
   getWorkingDirectory,
   changeWorkingDirectory,
   changeWorkingDirectoryFd,
  ) where

import System.IO.Error
import System.Posix.Error
import System.Posix.Types
import Foreign
import Foreign.C

import System.Posix.Directory.Common
import System.Posix.Internals (withFilePath, peekFilePath)

-- | @createDirectory dir mode@ calls @mkdir@ to 
--   create a new directory, @dir@, with permissions based on
--  @mode@.
createDirectory :: FilePath -> FileMode -> IO ()
createDirectory name mode =
  withFilePath name $ \s -> 
    throwErrnoPathIfMinus1Retry_ "createDirectory" name (c_mkdir s mode)  
    -- POSIX doesn't allow mkdir() to return EINTR, but it does on
    -- OS X (#5184), so we need the Retry variant here.

foreign import ccall unsafe "mkdir"
  c_mkdir :: CString -> CMode -> IO CInt

-- | @openDirStream dir@ calls @opendir@ to obtain a
--   directory stream for @dir@.
openDirStream :: FilePath -> IO DirStream
openDirStream name =
  withFilePath name $ \s -> do
    dirp <- throwErrnoPathIfNullRetry "openDirStream" name $ c_opendir s
    return (DirStream dirp)

foreign import ccall unsafe "__hsunix_opendir"
   c_opendir :: CString  -> IO (Ptr CDir)

-- | @readDirStream dp@ calls @readdir@ to obtain the
--   next directory entry (@struct dirent@) for the open directory
--   stream @dp@, and returns the @d_name@ member of that
--  structure.
readDirStream :: DirStream -> IO FilePath
readDirStream (DirStream dirp) =
  alloca $ \ptr_dEnt  -> loop ptr_dEnt
 where
  loop ptr_dEnt = do
    resetErrno
    r <- c_readdir dirp ptr_dEnt
    if (r == 0)
	 then do dEnt <- peek ptr_dEnt
		 if (dEnt == nullPtr)
		    then return []
		    else do
	 	     entry <- (d_name dEnt >>= peekFilePath)
		     c_freeDirEnt dEnt
		     return entry
	 else do errno <- getErrno
		 if (errno == eINTR) then loop ptr_dEnt else do
		 let (Errno eo) = errno
		 if (eo == 0)
		    then return []
		    else throwErrno "readDirStream"

-- traversing directories
foreign import ccall unsafe "__hscore_readdir"
  c_readdir  :: Ptr CDir -> Ptr (Ptr CDirent) -> IO CInt

foreign import ccall unsafe "__hscore_free_dirent"
  c_freeDirEnt  :: Ptr CDirent -> IO ()

foreign import ccall unsafe "__hscore_d_name"
  d_name :: Ptr CDirent -> IO CString


-- | @getWorkingDirectory@ calls @getcwd@ to obtain the name
--   of the current working directory.
getWorkingDirectory :: IO FilePath
getWorkingDirectory = do
  p <- mallocBytes long_path_size
  go p long_path_size
  where go p bytes = do
    	  p' <- c_getcwd p (fromIntegral bytes)
	  if p' /= nullPtr 
	     then do s <- peekFilePath p'
		     free p'
		     return s
	     else do errno <- getErrno
		     if errno == eRANGE
		        then do let bytes' = bytes * 2
			        p'' <- reallocBytes p bytes'
			        go p'' bytes'
		        else throwErrno "getCurrentDirectory"

foreign import ccall unsafe "getcwd"
   c_getcwd   :: Ptr CChar -> CSize -> IO (Ptr CChar)

foreign import ccall unsafe "__hsunix_long_path_size"
  long_path_size :: Int

-- | @changeWorkingDirectory dir@ calls @chdir@ to change
--   the current working directory to @dir@.
changeWorkingDirectory :: FilePath -> IO ()
changeWorkingDirectory path =
  modifyIOError (`ioeSetFileName` path) $
    withFilePath path $ \s -> 
       throwErrnoIfMinus1Retry_ "changeWorkingDirectory" (c_chdir s)

foreign import ccall unsafe "chdir"
   c_chdir :: CString -> IO CInt

removeDirectory :: FilePath -> IO ()
removeDirectory path =
  modifyIOError (`ioeSetFileName` path) $
    withFilePath path $ \s ->
       throwErrnoIfMinus1Retry_ "removeDirectory" (c_rmdir s)

foreign import ccall unsafe "rmdir"
   c_rmdir :: CString -> IO CInt
