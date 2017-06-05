{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-261cd265/build/System/Posix/DynamicLinker/Prim.hs" #-}
{-# LINE 1 "System/Posix/DynamicLinker/Prim.hsc" #-}
{-# LANGUAGE Trustworthy #-}
{-# LINE 2 "System/Posix/DynamicLinker/Prim.hsc" #-}

{-# LINE 3 "System/Posix/DynamicLinker/Prim.hsc" #-}
{-# OPTIONS_GHC -fno-warn-trustworthy-safe #-}

{-# LINE 5 "System/Posix/DynamicLinker/Prim.hsc" #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.DynamicLinker.Prim
-- Copyright   :  (c) Volker Stolz <vs@foldr.org> 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  vs@foldr.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- @dlopen(3)@ and friends
--  Derived from @GModule.chs@ by M.Weber & M.Chakravarty which is part of c2hs.
--  I left the API more or less the same, mostly the flags are different.
--
-----------------------------------------------------------------------------

module System.Posix.DynamicLinker.Prim (
  -- * low level API
  c_dlopen,
  c_dlsym,
  c_dlerror,
  c_dlclose,
  -- dlAddr, -- XXX NYI
  haveRtldNext,
  haveRtldLocal,
  packRTLDFlags,
  RTLDFlags(..),
  packDL,
  DL(..),
 )

where


{-# LINE 40 "System/Posix/DynamicLinker/Prim.hsc" #-}

import Data.Bits        ( (.|.) )
import Foreign.Ptr      ( Ptr, FunPtr, nullPtr )
import Foreign.C.Types
import Foreign.C.String ( CString )


-- |On some hosts (e.g. SuSe and Ubuntu Linux) @RTLD_NEXT@ (and
-- @RTLD_DEFAULT@) are not visible without setting the macro
-- @_GNU_SOURCE@. Since we don\'t want to define this macro, you can use
-- the function 'haveRtldNext' to check wether the flag `Next` is
-- available. Ideally, this will be optimized by the compiler so that it
-- should be as efficient as an @#ifdef@.
--
-- If you fail to test the flag and use it although it is undefined,
-- 'packDL' will throw an error.

haveRtldNext :: Bool


{-# LINE 60 "System/Posix/DynamicLinker/Prim.hsc" #-}
haveRtldNext = True
foreign import ccall unsafe "__hsunix_rtldNext" rtldNext :: Ptr a

{-# LINE 65 "System/Posix/DynamicLinker/Prim.hsc" #-}


{-# LINE 67 "System/Posix/DynamicLinker/Prim.hsc" #-}
foreign import ccall unsafe "__hsunix_rtldDefault" rtldDefault :: Ptr a

{-# LINE 69 "System/Posix/DynamicLinker/Prim.hsc" #-}

haveRtldLocal :: Bool
haveRtldLocal = True
{-# DEPRECATED haveRtldLocal "defaults to True" #-}


-- |Flags for 'System.Posix.DynamicLinker.dlopen'.

data RTLDFlags
  = RTLD_LAZY
  | RTLD_NOW
  | RTLD_GLOBAL
  | RTLD_LOCAL
    deriving (Show, Read)

foreign import ccall unsafe "dlopen" c_dlopen :: CString -> CInt -> IO (Ptr ())
foreign import ccall unsafe "dlsym"  c_dlsym  :: Ptr () -> CString -> IO (FunPtr a)
foreign import ccall unsafe "dlerror" c_dlerror :: IO CString
foreign import ccall unsafe "dlclose" c_dlclose :: (Ptr ()) -> IO CInt

packRTLDFlags :: [RTLDFlags] -> CInt
packRTLDFlags flags = foldl (\ s f -> (packRTLDFlag f) .|. s) 0 flags

packRTLDFlag :: RTLDFlags -> CInt
packRTLDFlag RTLD_LAZY = 1
{-# LINE 94 "System/Posix/DynamicLinker/Prim.hsc" #-}
packRTLDFlag RTLD_NOW = 2
{-# LINE 95 "System/Posix/DynamicLinker/Prim.hsc" #-}
packRTLDFlag RTLD_GLOBAL = 256
{-# LINE 96 "System/Posix/DynamicLinker/Prim.hsc" #-}
packRTLDFlag RTLD_LOCAL = 0
{-# LINE 97 "System/Posix/DynamicLinker/Prim.hsc" #-}


-- |Flags for 'System.Posix.DynamicLinker.dlsym'. Notice that 'Next'
-- might not be available on your particular platform! Use
-- 'haveRtldNext'.
--
-- If 'RTLD_DEFAULT' is not defined on your platform, 'packDL' 'Default'
-- reduces to 'nullPtr'.

data DL = Null | Next | Default | DLHandle (Ptr ()) deriving (Show)

packDL :: DL -> Ptr ()
packDL Null = nullPtr


{-# LINE 112 "System/Posix/DynamicLinker/Prim.hsc" #-}
packDL Next = rtldNext

{-# LINE 116 "System/Posix/DynamicLinker/Prim.hsc" #-}


{-# LINE 118 "System/Posix/DynamicLinker/Prim.hsc" #-}
packDL Default = rtldDefault

{-# LINE 122 "System/Posix/DynamicLinker/Prim.hsc" #-}

packDL (DLHandle h) = h
