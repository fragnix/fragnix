{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-261cd265/build/System/Posix/Fcntl.hs" #-}
{-# LINE 1 "System/Posix/Fcntl.hsc" #-}
{-# LANGUAGE CApiFFI #-}
{-# LINE 2 "System/Posix/Fcntl.hsc" #-}

{-# LINE 3 "System/Posix/Fcntl.hsc" #-}
{-# LANGUAGE Safe #-}

{-# LINE 7 "System/Posix/Fcntl.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Fcntl
-- Copyright   :  (c) The University of Glasgow 2014
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX file control support
--
-- @since 2.7.1.0
-----------------------------------------------------------------------------


{-# LINE 23 "System/Posix/Fcntl.hsc" #-}

module System.Posix.Fcntl (
    -- * File allocation
    Advice(..), fileAdvise,
    fileAllocate,
  ) where


{-# LINE 31 "System/Posix/Fcntl.hsc" #-}
import Foreign.C

{-# LINE 33 "System/Posix/Fcntl.hsc" #-}
import System.Posix.Types


{-# LINE 39 "System/Posix/Fcntl.hsc" #-}

-- -----------------------------------------------------------------------------
-- File control

-- | Advice parameter for 'fileAdvise' operation.
--
-- For more details, see documentation of @posix_fadvise(2)@.
--
-- @since 2.7.1.0
data Advice
  = AdviceNormal
  | AdviceRandom
  | AdviceSequential
  | AdviceWillNeed
  | AdviceDontNeed
  | AdviceNoReuse
  deriving Eq

-- | Performs @posix_fadvise(2)@ operation on file-descriptor.
--
-- If platform does not provide @posix_fadvise(2)@ 'fileAdvise'
-- becomes a no-op.
--
-- (use @#if HAVE_POSIX_FADVISE@ CPP guard to detect availability)
--
-- @since 2.7.1.0
fileAdvise :: Fd -> FileOffset -> FileOffset -> Advice -> IO ()

{-# LINE 67 "System/Posix/Fcntl.hsc" #-}
fileAdvise fd off len adv = do
  throwErrnoIfMinus1_ "fileAdvise" (c_posix_fadvise (fromIntegral fd) (fromIntegral off) (fromIntegral len) (packAdvice adv))

foreign import capi safe "fcntl.h posix_fadvise"
  c_posix_fadvise :: CInt -> COff -> COff -> CInt -> IO CInt

packAdvice :: Advice -> CInt
packAdvice AdviceNormal     = (0)
{-# LINE 75 "System/Posix/Fcntl.hsc" #-}
packAdvice AdviceRandom     = (1)
{-# LINE 76 "System/Posix/Fcntl.hsc" #-}
packAdvice AdviceSequential = (2)
{-# LINE 77 "System/Posix/Fcntl.hsc" #-}
packAdvice AdviceWillNeed   = (3)
{-# LINE 78 "System/Posix/Fcntl.hsc" #-}
packAdvice AdviceDontNeed   = (4)
{-# LINE 79 "System/Posix/Fcntl.hsc" #-}
packAdvice AdviceNoReuse    = (5)
{-# LINE 80 "System/Posix/Fcntl.hsc" #-}

{-# LINE 83 "System/Posix/Fcntl.hsc" #-}

-- | Performs @posix_fallocate(2)@ operation on file-descriptor.
--
-- Throws 'IOError' (\"unsupported operation\") if platform does not
-- provide @posix_fallocate(2)@.
--
-- (use @#if HAVE_POSIX_FALLOCATE@ CPP guard to detect availability).
--
-- @since 2.7.1.0
fileAllocate :: Fd -> FileOffset -> FileOffset -> IO ()

{-# LINE 94 "System/Posix/Fcntl.hsc" #-}
fileAllocate fd off len = do
  throwErrnoIfMinus1_ "fileAllocate" (c_posix_fallocate (fromIntegral fd) (fromIntegral off) (fromIntegral len))

foreign import capi safe "fcntl.h posix_fallocate"
  c_posix_fallocate :: CInt -> COff -> COff -> IO CInt

{-# LINE 105 "System/Posix/Fcntl.hsc" #-}
