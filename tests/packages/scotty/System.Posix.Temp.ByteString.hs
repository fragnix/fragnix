{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-d76e0d17/build/System/Posix/Temp/ByteString.hs" #-}
{-# LINE 1 "System/Posix/Temp/ByteString.hsc" #-}

{-# LINE 4 "System/Posix/Temp/ByteString.hsc" #-}
{-# LANGUAGE Trustworthy #-}

{-# LINE 6 "System/Posix/Temp/ByteString.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Temp.ByteString
-- Copyright   :  (c) Volker Stolz <vs@foldr.org>
--                    Deian Stefan <deian@cs.stanford.edu>
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org, vs@foldr.org, deian@cs.stanford.edu
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX temporary file and directory creation functions.
--
-----------------------------------------------------------------------------

module System.Posix.Temp.ByteString (
        mkstemp, mkstemps, mkdtemp
    ) where


{-# LINE 26 "System/Posix/Temp/ByteString.hsc" #-}

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Foreign.C

import System.IO
import System.Posix.ByteString.FilePath

{-# LINE 38 "System/Posix/Temp/ByteString.hsc" #-}
import System.Posix.IO
import System.Posix.Types


{-# LINE 42 "System/Posix/Temp/ByteString.hsc" #-}
foreign import ccall unsafe "HsUnix.h __hscore_mkstemp"
  c_mkstemp :: CString -> IO CInt

{-# LINE 45 "System/Posix/Temp/ByteString.hsc" #-}

-- | Make a unique filename and open it for reading\/writing. The returned
-- 'RawFilePath' is the (possibly relative) path of the created file, which is
-- padded with 6 random characters. The argument is the desired prefix of the
-- filepath of the temporary file to be created.
--
-- If you aren't using GHC or Hugs then this function simply wraps mktemp and
-- so shouldn't be considered safe.
mkstemp :: ByteString -> IO (RawFilePath, Handle)
mkstemp template' = do
  let template = template' `B.append` (BC.pack "XXXXXX")

{-# LINE 57 "System/Posix/Temp/ByteString.hsc" #-}
  withFilePath template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemp" (c_mkstemp ptr)
    name <- peekFilePath ptr
    h <- fdToHandle (Fd fd)
    return (name, h)

{-# LINE 67 "System/Posix/Temp/ByteString.hsc" #-}


{-# LINE 69 "System/Posix/Temp/ByteString.hsc" #-}
foreign import ccall unsafe "HsUnix.h __hscore_mkstemps"
  c_mkstemps :: CString -> CInt -> IO CInt

{-# LINE 72 "System/Posix/Temp/ByteString.hsc" #-}

-- |'mkstemps' - make a unique filename with a given prefix and suffix 
-- and open it for reading\/writing (only safe on GHC & Hugs).
-- The returned 'RawFilePath' is the (possibly relative) path of
-- the created file, which contains  6 random characters in between
-- the prefix and suffix.
mkstemps :: ByteString -> ByteString -> IO (RawFilePath, Handle)

{-# LINE 80 "System/Posix/Temp/ByteString.hsc" #-}
mkstemps prefix suffix = do
  let template = prefix `B.append` (BC.pack "XXXXXX") `B.append` suffix
      lenOfsuf = (fromIntegral $ B.length suffix) :: CInt
  withFilePath template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemps" (c_mkstemps ptr lenOfsuf)
    name <- peekFilePath ptr
    h <- fdToHandle (Fd fd)
    return (name, h)

{-# LINE 91 "System/Posix/Temp/ByteString.hsc" #-}


{-# LINE 93 "System/Posix/Temp/ByteString.hsc" #-}
foreign import ccall unsafe "HsUnix.h __hscore_mkdtemp"
  c_mkdtemp :: CString -> IO CString

{-# LINE 96 "System/Posix/Temp/ByteString.hsc" #-}

-- | Make a unique directory. The returned 'RawFilePath' is the path of the
-- created directory, which is padded with 6 random characters. The argument is
-- the desired prefix of the filepath of the temporary directory to be created.
--
-- If you aren't using GHC or Hugs then this function simply wraps mktemp and
-- so shouldn't be considered safe.
mkdtemp :: ByteString -> IO RawFilePath
mkdtemp template' = do
  let template = template' `B.append` (BC.pack "XXXXXX")

{-# LINE 107 "System/Posix/Temp/ByteString.hsc" #-}
  withFilePath template $ \ ptr -> do
    _ <- throwErrnoIfNull "mkdtemp" (c_mkdtemp ptr)
    name <- peekFilePath ptr
    return name

{-# LINE 116 "System/Posix/Temp/ByteString.hsc" #-}


{-# LINE 131 "System/Posix/Temp/ByteString.hsc" #-}

