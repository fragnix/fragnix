{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "dist/dist-sandbox-261cd265/build/System/PosixCompat/Extensions.hs" #-}

















































{-# LINE 1 "src/System/PosixCompat/Extensions.hsc" #-}
{-# LANGUAGE CPP #-}
{-# LINE 2 "src/System/PosixCompat/Extensions.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | This module provides some functions not present in the unix package.
module System.PosixCompat.Extensions (
    -- * Device IDs.
      CMajor
    , CMinor
    , deviceMajor
    , deviceMinor
    , makeDeviceID
    ) where



{-# LINE 16 "src/System/PosixCompat/Extensions.hsc" #-}

{-# LINE 17 "src/System/PosixCompat/Extensions.hsc" #-}

{-# LINE 18 "src/System/PosixCompat/Extensions.hsc" #-}

import Foreign.C.Types
import System.PosixCompat.Types


type CMajor = CUInt
type CMinor = CUInt

-- | Gets the major number from a 'DeviceID' for a device file.
--
-- The portable implementation always returns @0@.
deviceMajor :: DeviceID -> CMajor

{-# LINE 33 "src/System/PosixCompat/Extensions.hsc" #-}
deviceMajor dev = unix_major dev

foreign import ccall unsafe "unix_major" unix_major :: CDev -> CUInt

{-# LINE 37 "src/System/PosixCompat/Extensions.hsc" #-}

-- | Gets the minor number from a 'DeviceID' for a device file.
--
-- The portable implementation always returns @0@.
deviceMinor :: DeviceID -> CMinor

{-# LINE 45 "src/System/PosixCompat/Extensions.hsc" #-}
deviceMinor dev = unix_minor dev

foreign import ccall unsafe "unix_minor" unix_minor :: CDev -> CUInt

{-# LINE 49 "src/System/PosixCompat/Extensions.hsc" #-}

-- | Creates a 'DeviceID' for a device file given a major and minor number.
makeDeviceID :: CMajor -> CMinor -> DeviceID

{-# LINE 55 "src/System/PosixCompat/Extensions.hsc" #-}
makeDeviceID ma mi = unix_makedev ma mi

foreign import ccall unsafe "unix_makedev" unix_makedev :: CUInt -> CUInt -> CDev

{-# LINE 59 "src/System/PosixCompat/Extensions.hsc" #-}
