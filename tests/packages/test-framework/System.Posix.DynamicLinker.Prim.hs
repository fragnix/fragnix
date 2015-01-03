{-# LINE 1 "dist/dist-sandbox-235ea54e/build/System/Posix/DynamicLinker/Prim.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                                 






                      






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "dist/dist-sandbox-235ea54e/build/System/Posix/DynamicLinker/Prim.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/System/Posix/DynamicLinker/Prim.hs" #-}
{-# LINE 1 "System/Posix/DynamicLinker/Prim.hsc" #-}

{-# LINE 4 "System/Posix/DynamicLinker/Prim.hsc" #-}
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
-- DLOpen and friend
--  Derived from GModule.chs by M.Weber & M.Chakravarty which is part of c2hs
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


{-# LINE 38 "System/Posix/DynamicLinker/Prim.hsc" #-}

import Data.Bits	( (.|.) )
import Foreign.Ptr	( Ptr, FunPtr, nullPtr )
import Foreign.C.Types
import Foreign.C.String	( CString )

-- RTLD_NEXT madness
-- On some host (e.g. SuSe Linux 7.2) RTLD_NEXT is not visible
-- without setting _GNU_SOURCE. Since we don't want to set this
-- flag, here's a different solution: You can use the Haskell
-- function 'haveRtldNext' to check wether the flag is available
-- to you. Ideally, this will be optimized by the compiler so
-- that it should be as efficient as an #ifdef.
--    If you fail to test the flag and use it although it is
-- undefined, 'packOneModuleFlag' will bomb.
--    The same applies to RTLD_LOCAL which isn't available on
-- cygwin.

haveRtldNext :: Bool


{-# LINE 64 "System/Posix/DynamicLinker/Prim.hsc" #-}
haveRtldNext = False

{-# LINE 66 "System/Posix/DynamicLinker/Prim.hsc" #-}


{-# LINE 70 "System/Posix/DynamicLinker/Prim.hsc" #-}

haveRtldLocal :: Bool


{-# LINE 74 "System/Posix/DynamicLinker/Prim.hsc" #-}
haveRtldLocal = True

{-# LINE 78 "System/Posix/DynamicLinker/Prim.hsc" #-}

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
{-# LINE 96 "System/Posix/DynamicLinker/Prim.hsc" #-}


{-# LINE 98 "System/Posix/DynamicLinker/Prim.hsc" #-}
packRTLDFlag RTLD_NOW = 2
{-# LINE 99 "System/Posix/DynamicLinker/Prim.hsc" #-}

{-# LINE 102 "System/Posix/DynamicLinker/Prim.hsc" #-}


{-# LINE 104 "System/Posix/DynamicLinker/Prim.hsc" #-}
packRTLDFlag RTLD_GLOBAL = 256
{-# LINE 105 "System/Posix/DynamicLinker/Prim.hsc" #-}

{-# LINE 108 "System/Posix/DynamicLinker/Prim.hsc" #-}


{-# LINE 110 "System/Posix/DynamicLinker/Prim.hsc" #-}
packRTLDFlag RTLD_LOCAL = 0
{-# LINE 111 "System/Posix/DynamicLinker/Prim.hsc" #-}

{-# LINE 114 "System/Posix/DynamicLinker/Prim.hsc" #-}

-- |Flags for 'System.Posix.DynamicLinker.dlsym'. Notice that 'Next'
-- might not be available on your particular platform!

data DL = Null | Next | Default | DLHandle (Ptr ()) deriving (Show)

packDL :: DL -> Ptr ()
packDL Null = nullPtr

{-# LINE 125 "System/Posix/DynamicLinker/Prim.hsc" #-}
packDL Next = error "RTLD_NEXT not available"

{-# LINE 127 "System/Posix/DynamicLinker/Prim.hsc" #-}

{-# LINE 130 "System/Posix/DynamicLinker/Prim.hsc" #-}
packDL Default = nullPtr

{-# LINE 132 "System/Posix/DynamicLinker/Prim.hsc" #-}
packDL (DLHandle h) = h
