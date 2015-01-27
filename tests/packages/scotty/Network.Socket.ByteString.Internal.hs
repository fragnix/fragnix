{-# LINE 1 "Network/Socket/ByteString/Internal.hs" #-}
# 1 "Network/Socket/ByteString/Internal.hs"
# 1 "<command-line>"
# 12 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 12 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1





























































































# 12 "<command-line>" 2
# 1 "Network/Socket/ByteString/Internal.hs"
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-- |
-- Module      : Network.Socket.ByteString.Internal
-- Copyright   : (c) Johan Tibell 2007-2010
-- License     : BSD-style
--
-- Maintainer  : johan.tibell@gmail.com
-- Stability   : stable
-- Portability : portable
--
module Network.Socket.ByteString.Internal
    (
      mkInvalidRecvArgError

    , c_writev
    , c_sendmsg

    ) where

import System.IO.Error (ioeSetErrorString, mkIOError)


import Foreign.C.Types (CInt(..))
import System.Posix.Types (CSsize(..))
import Foreign.Ptr (Ptr)

import Network.Socket.ByteString.IOVec (IOVec)
import Network.Socket.ByteString.MsgHdr (MsgHdr)


import GHC.IO.Exception (IOErrorType(..))

mkInvalidRecvArgError :: String -> IOError
mkInvalidRecvArgError loc = ioeSetErrorString (mkIOError
                                    InvalidArgument
                                    loc Nothing Nothing) "non-positive length"


foreign import ccall unsafe "writev"
  c_writev :: CInt -> Ptr IOVec -> CInt -> IO CSsize

foreign import ccall unsafe "sendmsg"
  c_sendmsg :: CInt -> Ptr MsgHdr -> CInt -> IO CSsize
