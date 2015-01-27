{-# LINE 1 "dist/dist-sandbox-d76e0d17/build/Network/Socket/ByteString/MsgHdr.hs" #-}













































{-# LINE 1 "Network/Socket/ByteString/MsgHdr.hsc" #-}
{-# LANGUAGE CPP #-}
{-# LINE 2 "Network/Socket/ByteString/MsgHdr.hsc" #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Support module for the POSIX 'sendmsg' system call.
module Network.Socket.ByteString.MsgHdr
    ( MsgHdr(..)
    ) where


{-# LINE 10 "Network/Socket/ByteString/MsgHdr.hsc" #-}

{-# LINE 11 "Network/Socket/ByteString/MsgHdr.hsc" #-}

import Foreign.C.Types (CInt, CSize, CUInt)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import Network.Socket (SockAddr)
import Network.Socket.Internal (zeroMemory)

import Network.Socket.ByteString.IOVec (IOVec)

-- We don't use msg_control, msg_controllen, and msg_flags as these
-- don't exist on OpenSolaris.
data MsgHdr = MsgHdr
    { msgName    :: !(Ptr SockAddr)
    , msgNameLen :: !CUInt
    , msgIov     :: !(Ptr IOVec)
    , msgIovLen  :: !CSize
    }

instance Storable MsgHdr where
  sizeOf _    = (56)
{-# LINE 31 "Network/Socket/ByteString/MsgHdr.hsc" #-}
  alignment _ = alignment (undefined :: CInt)

  peek p = do
    name       <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))       p
{-# LINE 35 "Network/Socket/ByteString/MsgHdr.hsc" #-}
    nameLen    <- ((\hsc_ptr -> peekByteOff hsc_ptr 8))    p
{-# LINE 36 "Network/Socket/ByteString/MsgHdr.hsc" #-}
    iov        <- ((\hsc_ptr -> peekByteOff hsc_ptr 16))        p
{-# LINE 37 "Network/Socket/ByteString/MsgHdr.hsc" #-}
    iovLen     <- ((\hsc_ptr -> peekByteOff hsc_ptr 24))     p
{-# LINE 38 "Network/Socket/ByteString/MsgHdr.hsc" #-}
    return $ MsgHdr name nameLen iov iovLen

  poke p mh = do
    -- We need to zero the msg_control, msg_controllen, and msg_flags
    -- fields, but they only exist on some platforms (e.g. not on
    -- Solaris).  Instead of using CPP, we zero the entire struct.
    zeroMemory p (56)
{-# LINE 45 "Network/Socket/ByteString/MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0))       p (msgName       mh)
{-# LINE 46 "Network/Socket/ByteString/MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8))    p (msgNameLen    mh)
{-# LINE 47 "Network/Socket/ByteString/MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 16))        p (msgIov        mh)
{-# LINE 48 "Network/Socket/ByteString/MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 24))     p (msgIovLen     mh)
{-# LINE 49 "Network/Socket/ByteString/MsgHdr.hsc" #-}
