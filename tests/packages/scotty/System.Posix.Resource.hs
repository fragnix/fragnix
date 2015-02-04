{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-d76e0d17/build/System/Posix/Resource.hs" #-}
{-# LINE 1 "System/Posix/Resource.hsc" #-}

{-# LINE 4 "System/Posix/Resource.hsc" #-}
{-# LANGUAGE Trustworthy #-}

{-# LINE 6 "System/Posix/Resource.hsc" #-}
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


{-# LINE 28 "System/Posix/Resource.hsc" #-}

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

{-# LINE 44 "System/Posix/Resource.hsc" #-}
  | ResourceTotalMemory

{-# LINE 46 "System/Posix/Resource.hsc" #-}
  deriving Eq

data ResourceLimits
  = ResourceLimits { softLimit, hardLimit :: ResourceLimit }
  deriving Eq

data ResourceLimit
  = ResourceLimitInfinity
  | ResourceLimitUnknown
  | ResourceLimit Integer
  deriving Eq

type RLimit = ()

foreign import ccall unsafe "HsUnix.h __hscore_getrlimit"
  c_getrlimit :: CInt -> Ptr RLimit -> IO CInt

foreign import ccall unsafe "HsUnix.h __hscore_setrlimit"
  c_setrlimit :: CInt -> Ptr RLimit -> IO CInt

getResourceLimit :: Resource -> IO ResourceLimits
getResourceLimit res = do
  allocaBytes (16) $ \p_rlimit -> do
{-# LINE 69 "System/Posix/Resource.hsc" #-}
    throwErrnoIfMinus1_ "getResourceLimit" $
      c_getrlimit (packResource res) p_rlimit
    soft <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p_rlimit
{-# LINE 72 "System/Posix/Resource.hsc" #-}
    hard <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p_rlimit
{-# LINE 73 "System/Posix/Resource.hsc" #-}
    return (ResourceLimits {
                softLimit = unpackRLimit soft,
                hardLimit = unpackRLimit hard
           })

setResourceLimit :: Resource -> ResourceLimits -> IO ()
setResourceLimit res ResourceLimits{softLimit=soft,hardLimit=hard} = do
  allocaBytes (16) $ \p_rlimit -> do
{-# LINE 81 "System/Posix/Resource.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p_rlimit (packRLimit soft True)
{-# LINE 82 "System/Posix/Resource.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p_rlimit (packRLimit hard False)
{-# LINE 83 "System/Posix/Resource.hsc" #-}
    throwErrnoIfMinus1_ "setResourceLimit" $
        c_setrlimit (packResource res) p_rlimit
    return ()

packResource :: Resource -> CInt
packResource ResourceCoreFileSize  = (4)
{-# LINE 89 "System/Posix/Resource.hsc" #-}
packResource ResourceCPUTime       = (0)
{-# LINE 90 "System/Posix/Resource.hsc" #-}
packResource ResourceDataSize      = (2)
{-# LINE 91 "System/Posix/Resource.hsc" #-}
packResource ResourceFileSize      = (1)
{-# LINE 92 "System/Posix/Resource.hsc" #-}
packResource ResourceOpenFiles     = (7)
{-# LINE 93 "System/Posix/Resource.hsc" #-}
packResource ResourceStackSize     = (3)
{-# LINE 94 "System/Posix/Resource.hsc" #-}

{-# LINE 95 "System/Posix/Resource.hsc" #-}
packResource ResourceTotalMemory   = (9)
{-# LINE 96 "System/Posix/Resource.hsc" #-}

{-# LINE 97 "System/Posix/Resource.hsc" #-}

unpackRLimit :: CRLim -> ResourceLimit
unpackRLimit (18446744073709551615)  = ResourceLimitInfinity
{-# LINE 100 "System/Posix/Resource.hsc" #-}
unpackRLimit other

{-# LINE 102 "System/Posix/Resource.hsc" #-}
    | ((18446744073709551615) :: CRLim) /= (18446744073709551615) &&
{-# LINE 103 "System/Posix/Resource.hsc" #-}
      other == (18446744073709551615) = ResourceLimitUnknown
{-# LINE 104 "System/Posix/Resource.hsc" #-}

{-# LINE 105 "System/Posix/Resource.hsc" #-}

{-# LINE 106 "System/Posix/Resource.hsc" #-}
    | ((18446744073709551615) :: CRLim) /= (18446744073709551615) &&
{-# LINE 107 "System/Posix/Resource.hsc" #-}
      other == (18446744073709551615) = ResourceLimitUnknown
{-# LINE 108 "System/Posix/Resource.hsc" #-}

{-# LINE 109 "System/Posix/Resource.hsc" #-}
    | otherwise = ResourceLimit (fromIntegral other)

packRLimit :: ResourceLimit -> Bool -> CRLim
packRLimit ResourceLimitInfinity _     = (18446744073709551615)
{-# LINE 113 "System/Posix/Resource.hsc" #-}

{-# LINE 114 "System/Posix/Resource.hsc" #-}
packRLimit ResourceLimitUnknown  True  = (18446744073709551615)
{-# LINE 115 "System/Posix/Resource.hsc" #-}

{-# LINE 116 "System/Posix/Resource.hsc" #-}

{-# LINE 117 "System/Posix/Resource.hsc" #-}
packRLimit ResourceLimitUnknown  False = (18446744073709551615)
{-# LINE 118 "System/Posix/Resource.hsc" #-}

{-# LINE 119 "System/Posix/Resource.hsc" #-}
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
