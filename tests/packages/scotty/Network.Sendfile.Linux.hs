{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-261cd265/build/Network/Sendfile/Linux.hs" #-}
{-# LINE 1 "Network/Sendfile/Linux.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LINE 2 "Network/Sendfile/Linux.hsc" #-}

module Network.Sendfile.Linux (
    sendfile
  , sendfile'
  , sendfileFd
  , sendfileFd'
  , sendfileWithHeader
  , sendfileFdWithHeader
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString as B
import Data.ByteString.Internal
import Data.Int
import Foreign.C.Error (eAGAIN, getErrno, throwErrno)
import Foreign.C.Types
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.ForeignPtr
import Foreign.Storable (poke, sizeOf)
import GHC.Conc (threadWaitWrite)
import Network.Sendfile.Types
import Network.Socket
import System.Posix.Files
import System.Posix.IO
import qualified System.Posix.IO.ByteString as B
import System.Posix.Types


{-# LINE 33 "Network/Sendfile/Linux.hsc" #-}

{-# LINE 34 "Network/Sendfile/Linux.hsc" #-}

isLargeOffset :: Bool
isLargeOffset = sizeOf (0 :: COff) == 8

isLargeSize :: Bool
isLargeSize = sizeOf (0 :: CSize) == 8

safeSize :: CSize
safeSize
  | isLargeSize = 2^(60 :: Int)
  | otherwise   = 2^(30 :: Int)

----------------------------------------------------------------

-- |
-- Simple binding for sendfile() of Linux.
-- Used system calls:
--
--  - EntireFile -- open(), stat(), sendfile(), and close()
--
--  - PartOfFile -- open(), sendfile(), and close()
--
-- If the size of the file is unknown when sending the entire file,
-- specifying PartOfFile is much faster.
--
-- The fourth action argument is called when a file is sent as chunks.
-- Chucking is inevitable if the socket is non-blocking (this is the
-- default) and the file is large. The action is called after a chunk
-- is sent and bofore waiting the socket to be ready for writing.

sendfile :: Socket -> FilePath -> FileRange -> IO () -> IO ()
sendfile sock path range hook = bracket setup teardown $ \fd ->
    sendfileFd sock fd range hook
  where
    setup = openFd path ReadOnly Nothing defaultFileFlags{nonBlock=True}
    teardown = closeFd

sendfile' :: Fd -> ByteString -> FileRange -> IO () -> IO ()
sendfile' dst path range hook = bracket setup teardown $ \src ->
    sendfileFd' dst src range hook
  where
    setup = B.openFd path ReadOnly Nothing defaultFileFlags{nonBlock=True}
    teardown = closeFd

-- |
-- Simple binding for sendfile() of Linux.
-- Used system calls:
--
--  - EntireFile -- stat() and sendfile()
--
--  - PartOfFile -- sendfile()
--
-- If the size of the file is unknown when sending the entire file,
-- specifying PartOfFile is much faster.
--
-- The fourth action argument is called when a file is sent as chunks.
-- Chucking is inevitable if the socket is non-blocking (this is the
-- default) and the file is large. The action is called after a chunk
-- is sent and bofore waiting the socket to be ready for writing.
sendfileFd :: Socket -> Fd -> FileRange -> IO () -> IO ()
sendfileFd sock fd range hook = sendfileFd' dst fd range hook
  where
    dst = Fd $ fdSocket sock

sendfileFd' :: Fd -> Fd -> FileRange -> IO () -> IO ()
sendfileFd' dst src range hook =
    alloca $ \offp -> case range of
        EntireFile -> do
            poke offp 0
            -- System call is very slow. Use PartOfFile instead.
            len <- fileSize <$> getFdStatus src
            let len' = fromIntegral len
            sendfileloop dst src offp len' hook
        PartOfFile off len -> do
            poke offp (fromIntegral off)
            let len' = fromIntegral len
            sendfileloop dst src offp len' hook

sendfileloop :: Fd -> Fd -> Ptr COff -> CSize -> IO () -> IO ()
sendfileloop dst src offp len hook = do
    -- Multicore IO manager use edge-trigger mode.
    -- So, calling threadWaitWrite only when errnor is eAGAIN.
    let toSend
          | len > safeSize = safeSize
          | otherwise      = len
    bytes <- c_sendfile dst src offp toSend
    case bytes of
        -1 -> do
            errno <- getErrno
            if errno == eAGAIN then do
                threadWaitWrite dst
                sendfileloop dst src offp len hook
              else
                throwErrno "Network.SendFile.Linux.sendfileloop"
        0  -> return () -- the file is truncated
        _  -> do
            hook
            let left = len - fromIntegral bytes
            when (left /= 0) $ sendfileloop dst src offp left hook

-- Dst Src in order. take care
foreign import ccall unsafe "sendfile"
    c_sendfile32 :: Fd -> Fd -> Ptr COff -> CSize -> IO CSsize

foreign import ccall unsafe "sendfile64"
    c_sendfile64 :: Fd -> Fd -> Ptr COff -> CSize -> IO CSsize

c_sendfile :: Fd -> Fd -> Ptr COff -> CSize -> IO CSsize
c_sendfile
  | isLargeOffset = c_sendfile64
  | otherwise     = c_sendfile32

----------------------------------------------------------------

-- |
-- Simple binding for send() and sendfile() of Linux.
-- Used system calls:
--
--  - EntireFile -- send(), open(), stat(), sendfile(), and close()
--
--  - PartOfFile -- send(), open(), sendfile(), and close()
--
-- The fifth header is sent with send() + the MSG_MORE flag. If the
-- file is small enough, the header and the file is send in a single
-- TCP packet.
--
-- If the size of the file is unknown when sending the entire file,
-- specifying PartOfFile is much faster.
--
-- The fourth action argument is called when a file is sent as chunks.
-- Chucking is inevitable if the socket is non-blocking (this is the
-- default) and the file is large. The action is called after a chunk
-- is sent and bofore waiting the socket to be ready for writing.

sendfileWithHeader :: Socket -> FilePath -> FileRange -> IO () -> [ByteString] -> IO ()
sendfileWithHeader sock path range hook hdr = do
    -- Copying is much faster than syscall.
    sendMsgMore sock $ B.concat hdr
    sendfile sock path range hook

-- |
-- Simple binding for send() and sendfile() of Linux.
-- Used system calls:
--
--  - EntireFile -- send(), stat() and sendfile()
--
--  - PartOfFile -- send() and sendfile()
--
-- The fifth header is sent with send() + the MSG_MORE flag. If the
-- file is small enough, the header and the file is send in a single
-- TCP packet.
--
-- If the size of the file is unknown when sending the entire file,
-- specifying PartOfFile is much faster.
--
-- The fourth action argument is called when a file is sent as chunks.
-- Chucking is inevitable if the socket is non-blocking (this is the
-- default) and the file is large. The action is called after a chunk
-- is sent and bofore waiting the socket to be ready for writing.

sendfileFdWithHeader :: Socket -> Fd -> FileRange -> IO () -> [ByteString] -> IO ()
sendfileFdWithHeader sock fd range hook hdr = do
    -- Copying is much faster than syscall.
    sendMsgMore sock $ B.concat hdr
    sendfileFd sock fd range hook

sendMsgMore :: Socket -> ByteString -> IO ()
sendMsgMore sock bs = withForeignPtr fptr $ \ptr -> do
    let buf = castPtr (ptr `plusPtr` off)
        siz = fromIntegral len
    sendloop s buf siz
  where
    s = Fd $ fdSocket sock
    PS fptr off len = bs

sendloop :: Fd -> Ptr CChar -> CSize -> IO ()
sendloop s buf len = do
    bytes <- c_send s buf len (32768)
{-# LINE 212 "Network/Sendfile/Linux.hsc" #-}
    if bytes == -1 then do
        errno <- getErrno
        if errno == eAGAIN then do
            threadWaitWrite s
            sendloop s buf len
          else
            throwErrno "Network.SendFile.Linux.sendloop"
      else do
        let sent = fromIntegral bytes
        when (sent /= len) $ do
            let left = len - sent
                ptr = buf `plusPtr` fromIntegral bytes
            sendloop s ptr left

foreign import ccall unsafe "send"
  c_send :: Fd -> Ptr CChar -> CSize -> CInt -> IO CSsize
