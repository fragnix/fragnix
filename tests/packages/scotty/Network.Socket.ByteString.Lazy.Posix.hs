{-# LINE 1 "Network/Socket/ByteString/Lazy/Posix.hs" #-}
# 1 "Network/Socket/ByteString/Lazy/Posix.hs"
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
# 1 "Network/Socket/ByteString/Lazy/Posix.hs"
{-# LANGUAGE BangPatterns #-}
module Network.Socket.ByteString.Lazy.Posix
    (
    -- * Send data to a socket
      send
    , sendAll
    ) where

import Control.Monad (liftM)
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Internal (ByteString(..))
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Int (Int64)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (Storable(..))

import Network.Socket (Socket(..))
import Network.Socket.ByteString.IOVec (IOVec(IOVec))
import Network.Socket.ByteString.Internal (c_writev)
import Network.Socket.Internal

-- -----------------------------------------------------------------------------
-- Sending

send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int64    -- ^ Number of bytes sent
send sock@(MkSocket fd _ _ _ _) s = do
  let cs  = take maxNumChunks (L.toChunks s)
      len = length cs
  liftM fromIntegral . allocaArray len $ \ptr ->
    withPokes cs ptr $ \niovs ->
      throwSocketErrorWaitWrite sock "writev" $
        c_writev (fromIntegral fd) ptr niovs
  where
    withPokes ss p f = loop ss p 0 0
      where loop (c:cs) q k !niovs
                | k < maxNumBytes =
                    unsafeUseAsCStringLen c $ \(ptr,len) -> do
                      poke q $ IOVec ptr (fromIntegral len)
                      loop cs (q `plusPtr` sizeOf (undefined :: IOVec))
                              (k + fromIntegral len) (niovs + 1)
                | otherwise = f niovs
            loop _ _ _ niovs = f niovs
    maxNumBytes  = 4194304 :: Int  -- maximum number of bytes to transmit in one system call
    maxNumChunks = 1024    :: Int  -- maximum number of chunks to transmit in one system call

sendAll :: Socket      -- ^ Connected socket
        -> ByteString  -- ^ Data to send
        -> IO ()
sendAll sock bs = do
  sent <- send sock bs
  let bs' = L.drop sent bs
  unless (L.null bs') $ sendAll sock bs'
