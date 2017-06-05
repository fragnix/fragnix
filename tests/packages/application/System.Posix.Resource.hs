{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-261cd265/build/System/Posix/Resource.hs" #-}
{-# LINE 1 "System/Posix/Resource.hsc" #-}
{-# LANGUAGE CApiFFI #-}
{-# LINE 2 "System/Posix/Resource.hsc" #-}

{-# LINE 3 "System/Posix/Resource.hsc" #-}
{-# LANGUAGE Safe #-}

{-# LINE 7 "System/Posix/Resource.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Resource
-- Copyright   :  (c) The University of Glasgow 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX resource support
--
-----------------------------------------------------------------------------

module System.Posix.Resource (
    -- * Resource Limits
    ResourceLimit(..), ResourceLimits(..), Resource(..),
    getResourceLimit,
    setResourceLimit,
  ) where


{-# LINE 29 "System/Posix/Resource.hsc" #-}

import System.Posix.Types
import Foreign
import Foreign.C

-- -----------------------------------------------------------------------------
-- Resource limits

data Resource
  = ResourceCoreFileSize
  | ResourceCPUTime
  | ResourceDataSize
  | ResourceFileSize
  | ResourceOpenFiles
  | ResourceStackSize

{-# LINE 45 "System/Posix/Resource.hsc" #-}
  | ResourceTotalMemory

{-# LINE 47 "System/Posix/Resource.hsc" #-}
  deriving Eq

data ResourceLimits
  = ResourceLimits { softLimit, hardLimit :: ResourceLimit }
  deriving Eq

data ResourceLimit
  = ResourceLimitInfinity
  | ResourceLimitUnknown
  | ResourceLimit Integer
  deriving Eq

data {-# CTYPE "struct rlimit" #-} RLimit

foreign import capi unsafe "HsUnix.h getrlimit"
  c_getrlimit :: CInt -> Ptr RLimit -> IO CInt

foreign import capi unsafe "HsUnix.h setrlimit"
  c_setrlimit :: CInt -> Ptr RLimit -> IO CInt

getResourceLimit :: Resource -> IO ResourceLimits
getResourceLimit res = do
  allocaBytes (16) $ \p_rlimit -> do
{-# LINE 70 "System/Posix/Resource.hsc" #-}
    throwErrnoIfMinus1_ "getResourceLimit" $
      c_getrlimit (packResource res) p_rlimit
    soft <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p_rlimit
{-# LINE 73 "System/Posix/Resource.hsc" #-}
    hard <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p_rlimit
{-# LINE 74 "System/Posix/Resource.hsc" #-}
    return (ResourceLimits {
                softLimit = unpackRLimit soft,
                hardLimit = unpackRLimit hard
           })

setResourceLimit :: Resource -> ResourceLimits -> IO ()
setResourceLimit res ResourceLimits{softLimit=soft,hardLimit=hard} = do
  allocaBytes (16) $ \p_rlimit -> do
{-# LINE 82 "System/Posix/Resource.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p_rlimit (packRLimit soft True)
{-# LINE 83 "System/Posix/Resource.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p_rlimit (packRLimit hard False)
{-# LINE 84 "System/Posix/Resource.hsc" #-}
    throwErrnoIfMinus1_ "setResourceLimit" $
        c_setrlimit (packResource res) p_rlimit
    return ()

packResource :: Resource -> CInt
packResource ResourceCoreFileSize  = (4)
{-# LINE 90 "System/Posix/Resource.hsc" #-}
packResource ResourceCPUTime       = (0)
{-# LINE 91 "System/Posix/Resource.hsc" #-}
packResource ResourceDataSize      = (2)
{-# LINE 92 "System/Posix/Resource.hsc" #-}
packResource ResourceFileSize      = (1)
{-# LINE 93 "System/Posix/Resource.hsc" #-}
packResource ResourceOpenFiles     = (7)
{-# LINE 94 "System/Posix/Resource.hsc" #-}
packResource ResourceStackSize     = (3)
{-# LINE 95 "System/Posix/Resource.hsc" #-}

{-# LINE 96 "System/Posix/Resource.hsc" #-}
packResource ResourceTotalMemory   = (9)
{-# LINE 97 "System/Posix/Resource.hsc" #-}

{-# LINE 98 "System/Posix/Resource.hsc" #-}

unpackRLimit :: CRLim -> ResourceLimit
unpackRLimit (18446744073709551615)  = ResourceLimitInfinity
{-# LINE 101 "System/Posix/Resource.hsc" #-}
unpackRLimit other

{-# LINE 103 "System/Posix/Resource.hsc" #-}
    | ((18446744073709551615) :: CRLim) /= (18446744073709551615) &&
{-# LINE 104 "System/Posix/Resource.hsc" #-}
      other == (18446744073709551615) = ResourceLimitUnknown
{-# LINE 105 "System/Posix/Resource.hsc" #-}

{-# LINE 106 "System/Posix/Resource.hsc" #-}

{-# LINE 107 "System/Posix/Resource.hsc" #-}
    | ((18446744073709551615) :: CRLim) /= (18446744073709551615) &&
{-# LINE 108 "System/Posix/Resource.hsc" #-}
      other == (18446744073709551615) = ResourceLimitUnknown
{-# LINE 109 "System/Posix/Resource.hsc" #-}

{-# LINE 110 "System/Posix/Resource.hsc" #-}
    | otherwise = ResourceLimit (fromIntegral other)

packRLimit :: ResourceLimit -> Bool -> CRLim
packRLimit ResourceLimitInfinity _     = (18446744073709551615)
{-# LINE 114 "System/Posix/Resource.hsc" #-}

{-# LINE 115 "System/Posix/Resource.hsc" #-}
packRLimit ResourceLimitUnknown  True  = (18446744073709551615)
{-# LINE 116 "System/Posix/Resource.hsc" #-}

{-# LINE 117 "System/Posix/Resource.hsc" #-}

{-# LINE 118 "System/Posix/Resource.hsc" #-}
packRLimit ResourceLimitUnknown  False = (18446744073709551615)
{-# LINE 119 "System/Posix/Resource.hsc" #-}

{-# LINE 120 "System/Posix/Resource.hsc" #-}
packRLimit (ResourceLimit other) _     = fromIntegral other


-- -----------------------------------------------------------------------------
-- Test code

{-
import System.Posix
import Control.Monad

main = do
 zipWithM_ (\r n -> setResourceLimit r ResourceLimits{
                                        hardLimit = ResourceLimit n,
                                        softLimit = ResourceLimit n })
        allResources [1..]
 showAll
 mapM_ (\r -> setResourceLimit r ResourceLimits{
                                        hardLimit = ResourceLimit 1,
                                        softLimit = ResourceLimitInfinity })
        allResources
   -- should fail


showAll =
  mapM_ (\r -> getResourceLimit r >>= (putStrLn . showRLims)) allResources

allResources =
    [ResourceCoreFileSize, ResourceCPUTime, ResourceDataSize,
        ResourceFileSize, ResourceOpenFiles, ResourceStackSize
#ifdef RLIMIT_AS
        , ResourceTotalMemory
#endif
        ]

showRLims ResourceLimits{hardLimit=h,softLimit=s}
  = "hard: " ++ showRLim h ++ ", soft: " ++ showRLim s

showRLim ResourceLimitInfinity = "infinity"
showRLim ResourceLimitUnknown  = "unknown"
showRLim (ResourceLimit other)  = show other
-}
