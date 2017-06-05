{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "System/Linux/Sendfile.hs" #-}
module System.Linux.Sendfile (
    sendfile
  , sendfileFd
  , FileRange(..)
  ) where

import Data.ByteString (ByteString)
import Network.Sendfile.Linux (sendfile', sendfileFd')
import Network.Sendfile.Types (FileRange(..))
import System.Posix.Types (Fd)

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

sendfile :: Fd -> ByteString -> FileRange -> IO () -> IO ()
sendfile = sendfile'

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

sendfileFd :: Fd -> Fd -> FileRange -> IO () -> IO ()
sendfileFd = sendfileFd'
