{-# LINE 1 "dist/dist-sandbox-d76e0d17/build/Network/Socket/ByteString/IOVec.hs" #-}













































{-# LINE 1 "Network/Socket/ByteString/IOVec.hsc" #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LINE 2 "Network/Socket/ByteString/IOVec.hsc" #-}

-- | Support module for the POSIX writev system call.
module Network.Socket.ByteString.IOVec
    ( IOVec(..)
    ) where

import Foreign.C.Types (CChar, CInt, CSize)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))


{-# LINE 13 "Network/Socket/ByteString/IOVec.hsc" #-}

{-# LINE 14 "Network/Socket/ByteString/IOVec.hsc" #-}

data IOVec = IOVec
    { iovBase :: !(Ptr CChar)
    , iovLen  :: !CSize
    }

instance Storable IOVec where
  sizeOf _    = (16)
{-# LINE 22 "Network/Socket/ByteString/IOVec.hsc" #-}
  alignment _ = alignment (undefined :: CInt)

  peek p = do
    base <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 26 "Network/Socket/ByteString/IOVec.hsc" #-}
    len  <- ((\hsc_ptr -> peekByteOff hsc_ptr 8))  p
{-# LINE 27 "Network/Socket/ByteString/IOVec.hsc" #-}
    return $ IOVec base len

  poke p iov = do
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p (iovBase iov)
{-# LINE 31 "Network/Socket/ByteString/IOVec.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8))  p (iovLen  iov)
{-# LINE 32 "Network/Socket/ByteString/IOVec.hsc" #-}
