{-# LINE 1 "System.Environment.ExecutablePath.hsc" #-}
{-# LANGUAGE Safe #-}
{-# LINE 2 "System.Environment.ExecutablePath.hsc" #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Environment.ExecutablePath
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Function to retrieve the absolute filepath of the current executable.
--
-- @since 4.6.0.0
-----------------------------------------------------------------------------

module System.Environment.ExecutablePath ( getExecutablePath ) where

-- The imports are purposely kept completely disjoint to prevent edits
-- to one OS implementation from breaking another.


{-# LINE 42 "System.Environment.ExecutablePath.hsc" #-}
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.Posix.Internals

{-# LINE 48 "System.Environment.ExecutablePath.hsc" #-}

-- The exported function is defined outside any if-guard to make sure
-- every OS implements it with the same type.

-- | Returns the absolute pathname of the current executable.
--
-- Note that for scripts and interactive sessions, this is the path to
-- the interpreter (e.g. ghci.)
--
-- @since 4.6.0.0
getExecutablePath :: IO FilePath

--------------------------------------------------------------------------------
-- Mac OS X


{-# LINE 156 "System.Environment.ExecutablePath.hsc" #-}

foreign import ccall unsafe "getFullProgArgv"
    c_getFullProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

getExecutablePath =
    alloca $ \ p_argc ->
    alloca $ \ p_argv -> do
        c_getFullProgArgv p_argc p_argv
        argc <- peek p_argc
        if argc > 0
            -- If argc > 0 then argv[0] is guaranteed by the standard
            -- to be a pointer to a null-terminated string.
            then peek p_argv >>= peek >>= peekFilePath
            else errorWithoutStackTrace $ "getExecutablePath: " ++ msg
  where msg = "no OS specific implementation and program name couldn't be " ++
              "found in argv"

--------------------------------------------------------------------------------


{-# LINE 176 "System.Environment.ExecutablePath.hsc" #-}
