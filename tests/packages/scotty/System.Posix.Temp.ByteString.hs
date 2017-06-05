{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-261cd265/build/System/Posix/Temp/ByteString.hs" #-}
{-# LINE 1 "System/Posix/Temp/ByteString.hsc" #-}
{-# LANGUAGE CApiFFI #-}
{-# LINE 2 "System/Posix/Temp/ByteString.hsc" #-}

{-# LINE 3 "System/Posix/Temp/ByteString.hsc" #-}
{-# LANGUAGE Safe #-}

{-# LINE 7 "System/Posix/Temp/ByteString.hsc" #-}
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


{-# LINE 27 "System/Posix/Temp/ByteString.hsc" #-}

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Foreign.C

import System.IO
import System.Posix.ByteString.FilePath

{-# LINE 39 "System/Posix/Temp/ByteString.hsc" #-}
import System.Posix.IO
import System.Posix.Types

foreign import capi unsafe "HsUnix.h mkstemp"
  c_mkstemp :: CString -> IO CInt

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
  withFilePath template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemp" (c_mkstemp ptr)
    name <- peekFilePath ptr
    h <- fdToHandle (Fd fd)
    return (name, h)


{-# LINE 62 "System/Posix/Temp/ByteString.hsc" #-}
foreign import capi unsafe "HsUnix.h mkstemps"
  c_mkstemps :: CString -> CInt -> IO CInt

{-# LINE 65 "System/Posix/Temp/ByteString.hsc" #-}

-- |'mkstemps' - make a unique filename with a given prefix and suffix
-- and open it for reading\/writing (only safe on GHC & Hugs).
-- The returned 'RawFilePath' is the (possibly relative) path of
-- the created file, which contains  6 random characters in between
-- the prefix and suffix.
mkstemps :: ByteString -> ByteString -> IO (RawFilePath, Handle)

{-# LINE 73 "System/Posix/Temp/ByteString.hsc" #-}
mkstemps prefix suffix = do
  let template = prefix `B.append` (BC.pack "XXXXXX") `B.append` suffix
      lenOfsuf = (fromIntegral $ B.length suffix) :: CInt
  withFilePath template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemps" (c_mkstemps ptr lenOfsuf)
    name <- peekFilePath ptr
    h <- fdToHandle (Fd fd)
    return (name, h)

{-# LINE 84 "System/Posix/Temp/ByteString.hsc" #-}


{-# LINE 86 "System/Posix/Temp/ByteString.hsc" #-}
foreign import capi unsafe "HsUnix.h mkdtemp"
  c_mkdtemp :: CString -> IO CString

{-# LINE 89 "System/Posix/Temp/ByteString.hsc" #-}

-- | Make a unique directory. The returned 'RawFilePath' is the path of the
-- created directory, which is padded with 6 random characters. The argument is
-- the desired prefix of the filepath of the temporary directory to be created.
--
-- If you aren't using GHC or Hugs then this function simply wraps mktemp and
-- so shouldn't be considered safe.
mkdtemp :: ByteString -> IO RawFilePath
mkdtemp template' = do
  let template = template' `B.append` (BC.pack "XXXXXX")

{-# LINE 100 "System/Posix/Temp/ByteString.hsc" #-}
  withFilePath template $ \ ptr -> do
    _ <- throwErrnoIfNull "mkdtemp" (c_mkdtemp ptr)
    name <- peekFilePath ptr
    return name

{-# LINE 109 "System/Posix/Temp/ByteString.hsc" #-}


{-# LINE 124 "System/Posix/Temp/ByteString.hsc" #-}

