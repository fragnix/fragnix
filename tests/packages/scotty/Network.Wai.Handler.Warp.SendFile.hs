{-# LINE 1 "Network/Wai/Handler/Warp/SendFile.hs" #-}
# 1 "Network/Wai/Handler/Warp/SendFile.hs"
# 1 "<command-line>"
# 9 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 9 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1













































































































































































































# 9 "<command-line>" 2
# 1 "Network/Wai/Handler/Warp/SendFile.hs"
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.SendFile where

import Data.ByteString (ByteString)
import Network.Sendfile
import Network.Socket (Socket)
import qualified Network.Wai.Handler.Warp.FdCache as F
import Network.Wai.Handler.Warp.Types

defaultSendFile :: Socket -> FilePath -> Integer -> Integer -> IO () -> [ByteString] -> IO ()
defaultSendFile s path off len act hdr = sendfileWithHeader s path (PartOfFile off len) act hdr



setSendFile :: Connection -> Maybe F.MutableFdCache -> Connection
setSendFile conn Nothing    = conn
setSendFile conn (Just fdcs) = case connSendFileOverride conn of
    NotOverride -> conn
    Override s  -> conn { connSendFile = sendFile fdcs s }

sendFile :: F.MutableFdCache -> Socket -> FilePath -> Integer -> Integer -> IO () -> [ByteString] -> IO ()
sendFile fdcs s path off len act hdr = do
    (fd, fresher) <- F.getFd fdcs path
    sendfileFdWithHeader s fd (PartOfFile off len) (act>>fresher) hdr
