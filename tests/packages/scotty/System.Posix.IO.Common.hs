{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-d76e0d17/build/System/Posix/IO/Common.hs" #-}
{-# LINE 1 "System/Posix/IO/Common.hsc" #-}
{-# LANGUAGE NondecreasingIndentation, RecordWildCards #-}
{-# LINE 2 "System/Posix/IO/Common.hsc" #-}

{-# LINE 5 "System/Posix/IO/Common.hsc" #-}
{-# LANGUAGE Trustworthy #-}

{-# LINE 7 "System/Posix/IO/Common.hsc" #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.IO.Common
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-----------------------------------------------------------------------------

module System.Posix.IO.Common (
    -- * Input \/ Output

    -- ** Standard file descriptors
    stdInput, stdOutput, stdError,

    -- ** Opening and closing files
    OpenMode(..),
    OpenFileFlags(..), defaultFileFlags,
    open_,
    closeFd,

    -- ** Reading\/writing data
    -- |Programmers using the 'fdRead' and 'fdWrite' API should be aware that
    -- EAGAIN exceptions may occur for non-blocking IO!

    fdRead, fdWrite,
    fdReadBuf, fdWriteBuf,

    -- ** Seeking
    fdSeek,

    -- ** File options
    FdOption(..),
    queryFdOption,
    setFdOption,

    -- ** Locking
    FileLock,
    LockRequest(..),
    getLock,  setLock,
    waitToSetLock,

    -- ** Pipes
    createPipe,

    -- ** Duplicating file descriptors
    dup, dupTo,

    -- ** Converting file descriptors to\/from Handles
    handleToFd,
    fdToHandle,

  ) where

import System.IO
import System.IO.Error
import System.Posix.Types
import qualified System.Posix.Internals as Base

import Foreign
import Foreign.C


{-# LINE 74 "System/Posix/IO/Common.hsc" #-}
import GHC.IO.Handle.Internals
import GHC.IO.Handle.Types
import qualified GHC.IO.FD as FD
import qualified GHC.IO.Handle.FD as FD
import GHC.IO.Exception
import Data.Typeable (cast)

{-# LINE 81 "System/Posix/IO/Common.hsc" #-}


{-# LINE 86 "System/Posix/IO/Common.hsc" #-}


{-# LINE 88 "System/Posix/IO/Common.hsc" #-}

-- -----------------------------------------------------------------------------
-- Pipes
-- |The 'createPipe' function creates a pair of connected file
-- descriptors. The first component is the fd to read from, the second
-- is the write end.  Although pipes may be bidirectional, this
-- behaviour is not portable and programmers should use two separate
-- pipes for this purpose.  May throw an exception if this is an
-- invalid descriptor.

createPipe :: IO (Fd, Fd)
createPipe =
  allocaArray 2 $ \p_fd -> do
    throwErrnoIfMinus1_ "createPipe" (c_pipe p_fd)
    rfd <- peekElemOff p_fd 0
    wfd <- peekElemOff p_fd 1
    return (Fd rfd, Fd wfd)

foreign import ccall unsafe "pipe"
   c_pipe :: Ptr CInt -> IO CInt

-- -----------------------------------------------------------------------------
-- Duplicating file descriptors

-- | May throw an exception if this is an invalid descriptor.
dup :: Fd -> IO Fd
dup (Fd fd) = do r <- throwErrnoIfMinus1 "dup" (c_dup fd); return (Fd r)

-- | May throw an exception if this is an invalid descriptor.
dupTo :: Fd -> Fd -> IO Fd
dupTo (Fd fd1) (Fd fd2) = do
  r <- throwErrnoIfMinus1 "dupTo" (c_dup2 fd1 fd2)
  return (Fd r)

foreign import ccall unsafe "dup"
   c_dup :: CInt -> IO CInt

foreign import ccall unsafe "dup2"
   c_dup2 :: CInt -> CInt -> IO CInt

-- -----------------------------------------------------------------------------
-- Opening and closing files

stdInput, stdOutput, stdError :: Fd
stdInput   = Fd (0)
{-# LINE 133 "System/Posix/IO/Common.hsc" #-}
stdOutput  = Fd (1)
{-# LINE 134 "System/Posix/IO/Common.hsc" #-}
stdError   = Fd (2)
{-# LINE 135 "System/Posix/IO/Common.hsc" #-}

data OpenMode = ReadOnly | WriteOnly | ReadWrite

-- |Correspond to some of the int flags from C's fcntl.h.
data OpenFileFlags =
 OpenFileFlags {
    append    :: Bool, -- ^ O_APPEND
    exclusive :: Bool, -- ^ O_EXCL
    noctty    :: Bool, -- ^ O_NOCTTY
    nonBlock  :: Bool, -- ^ O_NONBLOCK
    trunc     :: Bool  -- ^ O_TRUNC
 }


-- |Default values for the 'OpenFileFlags' type. False for each of
-- append, exclusive, noctty, nonBlock, and trunc.
defaultFileFlags :: OpenFileFlags
defaultFileFlags =
 OpenFileFlags {
    append    = False,
    exclusive = False,
    noctty    = False,
    nonBlock  = False,
    trunc     = False
  }


-- |Open and optionally create this file.  See 'System.Posix.Files'
-- for information on how to use the 'FileMode' type.
open_  :: CString
       -> OpenMode
       -> Maybe FileMode -- ^Just x => creates the file with the given modes, Nothing => the file must exist.
       -> OpenFileFlags
       -> IO Fd
open_ str how maybe_mode (OpenFileFlags appendFlag exclusiveFlag nocttyFlag
                                nonBlockFlag truncateFlag) = do
    fd <- c_open str all_flags mode_w
    return (Fd fd)
  where
    all_flags  = creat .|. flags .|. open_mode

    flags =
       (if appendFlag    then (1024)   else 0) .|.
{-# LINE 178 "System/Posix/IO/Common.hsc" #-}
       (if exclusiveFlag then (128)     else 0) .|.
{-# LINE 179 "System/Posix/IO/Common.hsc" #-}
       (if nocttyFlag    then (256)   else 0) .|.
{-# LINE 180 "System/Posix/IO/Common.hsc" #-}
       (if nonBlockFlag  then (2048) else 0) .|.
{-# LINE 181 "System/Posix/IO/Common.hsc" #-}
       (if truncateFlag  then (512)    else 0)
{-# LINE 182 "System/Posix/IO/Common.hsc" #-}

    (creat, mode_w) = case maybe_mode of
                        Nothing -> (0,0)
                        Just x  -> ((64), x)
{-# LINE 186 "System/Posix/IO/Common.hsc" #-}

    open_mode = case how of
                   ReadOnly  -> (0)
{-# LINE 189 "System/Posix/IO/Common.hsc" #-}
                   WriteOnly -> (1)
{-# LINE 190 "System/Posix/IO/Common.hsc" #-}
                   ReadWrite -> (2)
{-# LINE 191 "System/Posix/IO/Common.hsc" #-}

foreign import ccall unsafe "__hscore_open"
   c_open :: CString -> CInt -> CMode -> IO CInt

-- |Close this file descriptor.  May throw an exception if this is an
-- invalid descriptor.

closeFd :: Fd -> IO ()
closeFd (Fd fd) = throwErrnoIfMinus1_ "closeFd" (c_close fd)

foreign import ccall unsafe "HsUnix.h close"
   c_close :: CInt -> IO CInt

-- -----------------------------------------------------------------------------
-- Converting file descriptors to/from Handles

-- | Extracts the 'Fd' from a 'Handle'.  This function has the side effect
-- of closing the 'Handle' and flushing its write buffer, if necessary.
handleToFd :: Handle -> IO Fd

-- | Converts an 'Fd' into a 'Handle' that can be used with the
-- standard Haskell IO library (see "System.IO").
fdToHandle :: Fd -> IO Handle


{-# LINE 216 "System/Posix/IO/Common.hsc" #-}
handleToFd h@(FileHandle _ m) = do
  withHandle' "handleToFd" h m $ handleToFd' h
handleToFd h@(DuplexHandle _ r w) = do
  _ <- withHandle' "handleToFd" h r $ handleToFd' h
  withHandle' "handleToFd" h w $ handleToFd' h
  -- for a DuplexHandle, make sure we mark both sides as closed,
  -- otherwise a finalizer will come along later and close the other
  -- side. (#3914)

handleToFd' :: Handle -> Handle__ -> IO (Handle__, Fd)
handleToFd' h h_@Handle__{haType=_,..} = do
  case cast haDevice of
    Nothing -> ioError (ioeSetErrorString (mkIOError IllegalOperation
                                           "handleToFd" (Just h) Nothing)
                        "handle is not a file descriptor")
    Just fd -> do
     -- converting a Handle into an Fd effectively means
     -- letting go of the Handle; it is put into a closed
     -- state as a result.
     flushWriteBuffer h_
     FD.release fd
     return (Handle__{haType=ClosedHandle,..}, Fd (FD.fdFD fd))

fdToHandle fd = FD.fdToHandle (fromIntegral fd)

{-# LINE 241 "System/Posix/IO/Common.hsc" #-}


{-# LINE 251 "System/Posix/IO/Common.hsc" #-}

-- -----------------------------------------------------------------------------
-- Fd options

data FdOption = AppendOnWrite     -- ^O_APPEND
              | CloseOnExec       -- ^FD_CLOEXEC
              | NonBlockingRead   -- ^O_NONBLOCK
              | SynchronousWrites -- ^O_SYNC

fdOption2Int :: FdOption -> CInt
fdOption2Int CloseOnExec       = (1)
{-# LINE 262 "System/Posix/IO/Common.hsc" #-}
fdOption2Int AppendOnWrite     = (1024)
{-# LINE 263 "System/Posix/IO/Common.hsc" #-}
fdOption2Int NonBlockingRead   = (2048)
{-# LINE 264 "System/Posix/IO/Common.hsc" #-}
fdOption2Int SynchronousWrites = (1052672)
{-# LINE 265 "System/Posix/IO/Common.hsc" #-}

-- | May throw an exception if this is an invalid descriptor.
queryFdOption :: Fd -> FdOption -> IO Bool
queryFdOption (Fd fd) opt = do
  r <- throwErrnoIfMinus1 "queryFdOption" (Base.c_fcntl_read fd flag)
  return ((r .&. fdOption2Int opt) /= 0)
 where
  flag    = case opt of
              CloseOnExec       -> (1)
{-# LINE 274 "System/Posix/IO/Common.hsc" #-}
              _                 -> (3)
{-# LINE 275 "System/Posix/IO/Common.hsc" #-}

-- | May throw an exception if this is an invalid descriptor.
setFdOption :: Fd -> FdOption -> Bool -> IO ()
setFdOption (Fd fd) opt val = do
  r <- throwErrnoIfMinus1 "setFdOption" (Base.c_fcntl_read fd getflag)
  let r' | val       = r .|. opt_val
         | otherwise = r .&. (complement opt_val)
  throwErrnoIfMinus1_ "setFdOption"
                      (Base.c_fcntl_write fd setflag (fromIntegral r'))
 where
  (getflag,setflag)= case opt of
              CloseOnExec       -> ((1),(2))
{-# LINE 287 "System/Posix/IO/Common.hsc" #-}
              _                 -> ((3),(4))
{-# LINE 288 "System/Posix/IO/Common.hsc" #-}
  opt_val = fdOption2Int opt

-- -----------------------------------------------------------------------------
-- Seeking

mode2Int :: SeekMode -> CInt
mode2Int AbsoluteSeek = (0)
{-# LINE 295 "System/Posix/IO/Common.hsc" #-}
mode2Int RelativeSeek = (1)
{-# LINE 296 "System/Posix/IO/Common.hsc" #-}
mode2Int SeekFromEnd  = (2)
{-# LINE 297 "System/Posix/IO/Common.hsc" #-}

-- | May throw an exception if this is an invalid descriptor.
fdSeek :: Fd -> SeekMode -> FileOffset -> IO FileOffset
fdSeek (Fd fd) mode off =
  throwErrnoIfMinus1 "fdSeek" (Base.c_lseek fd off (mode2Int mode))

-- -----------------------------------------------------------------------------
-- Locking

data LockRequest = ReadLock
                 | WriteLock
                 | Unlock

type FileLock = (LockRequest, SeekMode, FileOffset, FileOffset)

-- | May throw an exception if this is an invalid descriptor.
getLock :: Fd -> FileLock -> IO (Maybe (ProcessID, FileLock))
getLock (Fd fd) lock =
  allocaLock lock $ \p_flock -> do
    throwErrnoIfMinus1_ "getLock" (Base.c_fcntl_lock fd (5) p_flock)
{-# LINE 317 "System/Posix/IO/Common.hsc" #-}
    result <- bytes2ProcessIDAndLock p_flock
    return (maybeResult result)
  where
    maybeResult (_, (Unlock, _, _, _)) = Nothing
    maybeResult x = Just x

type CFLock     = ()

allocaLock :: FileLock -> (Ptr CFLock -> IO a) -> IO a
allocaLock (lockreq, mode, start, len) io =
  allocaBytes (32) $ \p -> do
{-# LINE 328 "System/Posix/IO/Common.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0))   p (lockReq2Int lockreq :: CShort)
{-# LINE 329 "System/Posix/IO/Common.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 2)) p (fromIntegral (mode2Int mode) :: CShort)
{-# LINE 330 "System/Posix/IO/Common.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8))  p start
{-# LINE 331 "System/Posix/IO/Common.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 16))    p len
{-# LINE 332 "System/Posix/IO/Common.hsc" #-}
    io p

lockReq2Int :: LockRequest -> CShort
lockReq2Int ReadLock  = (0)
{-# LINE 336 "System/Posix/IO/Common.hsc" #-}
lockReq2Int WriteLock = (1)
{-# LINE 337 "System/Posix/IO/Common.hsc" #-}
lockReq2Int Unlock    = (2)
{-# LINE 338 "System/Posix/IO/Common.hsc" #-}

bytes2ProcessIDAndLock :: Ptr CFLock -> IO (ProcessID, FileLock)
bytes2ProcessIDAndLock p = do
  req   <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))   p
{-# LINE 342 "System/Posix/IO/Common.hsc" #-}
  mode  <- ((\hsc_ptr -> peekByteOff hsc_ptr 2)) p
{-# LINE 343 "System/Posix/IO/Common.hsc" #-}
  start <- ((\hsc_ptr -> peekByteOff hsc_ptr 8))  p
{-# LINE 344 "System/Posix/IO/Common.hsc" #-}
  len   <- ((\hsc_ptr -> peekByteOff hsc_ptr 16))    p
{-# LINE 345 "System/Posix/IO/Common.hsc" #-}
  pid   <- ((\hsc_ptr -> peekByteOff hsc_ptr 24))    p
{-# LINE 346 "System/Posix/IO/Common.hsc" #-}
  return (pid, (int2req req, int2mode mode, start, len))
 where
  int2req :: CShort -> LockRequest
  int2req (0) = ReadLock
{-# LINE 350 "System/Posix/IO/Common.hsc" #-}
  int2req (1) = WriteLock
{-# LINE 351 "System/Posix/IO/Common.hsc" #-}
  int2req (2) = Unlock
{-# LINE 352 "System/Posix/IO/Common.hsc" #-}
  int2req _ = error $ "int2req: bad argument"

  int2mode :: CShort -> SeekMode
  int2mode (0) = AbsoluteSeek
{-# LINE 356 "System/Posix/IO/Common.hsc" #-}
  int2mode (1) = RelativeSeek
{-# LINE 357 "System/Posix/IO/Common.hsc" #-}
  int2mode (2) = SeekFromEnd
{-# LINE 358 "System/Posix/IO/Common.hsc" #-}
  int2mode _ = error $ "int2mode: bad argument"

-- | May throw an exception if this is an invalid descriptor.
setLock :: Fd -> FileLock -> IO ()
setLock (Fd fd) lock = do
  allocaLock lock $ \p_flock ->
    throwErrnoIfMinus1_ "setLock" (Base.c_fcntl_lock fd (6) p_flock)
{-# LINE 365 "System/Posix/IO/Common.hsc" #-}

-- | May throw an exception if this is an invalid descriptor.
waitToSetLock :: Fd -> FileLock -> IO ()
waitToSetLock (Fd fd) lock = do
  allocaLock lock $ \p_flock ->
    throwErrnoIfMinus1_ "waitToSetLock"
        (Base.c_fcntl_lock fd (7) p_flock)
{-# LINE 372 "System/Posix/IO/Common.hsc" #-}

-- -----------------------------------------------------------------------------
-- fd{Read,Write}

-- | Read data from an 'Fd' and convert it to a 'String' using the locale encoding.
-- Throws an exception if this is an invalid descriptor, or EOF has been
-- reached.
fdRead :: Fd
       -> ByteCount -- ^How many bytes to read
       -> IO (String, ByteCount) -- ^The bytes read, how many bytes were read.
fdRead _fd 0 = return ("", 0)
fdRead fd nbytes = do
    allocaBytes (fromIntegral nbytes) $ \ buf -> do
    rc <- fdReadBuf fd buf nbytes
    case rc of
      0 -> ioError (ioeSetErrorString (mkIOError EOF "fdRead" Nothing Nothing) "EOF")
      n -> do
       s <- peekCStringLen (castPtr buf, fromIntegral n)
       return (s, n)

-- | Read data from an 'Fd' into memory.  This is exactly equivalent
-- to the POSIX @read@ function.
fdReadBuf :: Fd
          -> Ptr Word8 -- ^ Memory in which to put the data
          -> ByteCount -- ^ Maximum number of bytes to read
          -> IO ByteCount -- ^ Number of bytes read (zero for EOF)
fdReadBuf _fd _buf 0 = return 0
fdReadBuf fd buf nbytes =
  fmap fromIntegral $
    throwErrnoIfMinus1Retry "fdReadBuf" $
      c_safe_read (fromIntegral fd) (castPtr buf) nbytes

foreign import ccall safe "read"
   c_safe_read :: CInt -> Ptr CChar -> CSize -> IO CSsize

-- | Write a 'String' to an 'Fd' using the locale encoding.
fdWrite :: Fd -> String -> IO ByteCount
fdWrite fd str =
  withCStringLen str $ \ (buf,len) ->
    fdWriteBuf fd (castPtr buf) (fromIntegral len)

-- | Write data from memory to an 'Fd'.  This is exactly equivalent
-- to the POSIX @write@ function.
fdWriteBuf :: Fd
           -> Ptr Word8    -- ^ Memory containing the data to write
           -> ByteCount    -- ^ Maximum number of bytes to write
           -> IO ByteCount -- ^ Number of bytes written
fdWriteBuf fd buf len =
  fmap fromIntegral $
    throwErrnoIfMinus1Retry "fdWriteBuf" $
      c_safe_write (fromIntegral fd) (castPtr buf) len

foreign import ccall safe "write"
   c_safe_write :: CInt -> Ptr CChar -> CSize -> IO CSsize
