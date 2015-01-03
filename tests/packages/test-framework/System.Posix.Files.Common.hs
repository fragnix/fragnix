{-# LINE 1 "dist/dist-sandbox-235ea54e/build/System/Posix/Files/Common.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                                 






                      






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "dist/dist-sandbox-235ea54e/build/System/Posix/Files/Common.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/System/Posix/Files/Common.hs" #-}
{-# LINE 1 "System/Posix/Files/Common.hsc" #-}

{-# LINE 4 "System/Posix/Files/Common.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Files.Common
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- Functions defined by the POSIX standards for manipulating and querying the
-- file system. Names of underlying POSIX functions are indicated whenever
-- possible. A more complete documentation of the POSIX functions together
-- with a more detailed description of different error conditions are usually
-- available in the system's manual pages or from
-- <http://www.unix.org/version3/online.html> (free registration required).
--
-- When a function that calls an underlying POSIX function fails, the errno
-- code is converted to an 'IOError' using 'Foreign.C.Error.errnoToIOError'.
-- For a list of which errno codes may be generated, consult the POSIX
-- documentation for the underlying function.
--
-----------------------------------------------------------------------------


{-# LINE 29 "System/Posix/Files/Common.hsc" #-}

module System.Posix.Files.Common (
    -- * File modes
    -- FileMode exported by System.Posix.Types
    unionFileModes, intersectFileModes,
    nullFileMode,
    ownerReadMode, ownerWriteMode, ownerExecuteMode, ownerModes,
    groupReadMode, groupWriteMode, groupExecuteMode, groupModes,
    otherReadMode, otherWriteMode, otherExecuteMode, otherModes,
    setUserIDMode, setGroupIDMode,
    stdFileMode,   accessModes,
    fileTypeModes,
    blockSpecialMode, characterSpecialMode, namedPipeMode, regularFileMode,
    directoryMode, symbolicLinkMode, socketMode,

    -- ** Setting file modes
    setFdMode, setFileCreationMask,

    -- * File status
    FileStatus(..),
    -- ** Obtaining file status
    getFdStatus,
    -- ** Querying file status
    deviceID, fileID, fileMode, linkCount, fileOwner, fileGroup,
    specialDeviceID, fileSize, accessTime, modificationTime,
    statusChangeTime,
    accessTimeHiRes, modificationTimeHiRes, statusChangeTimeHiRes,
    setFdTimesHiRes, touchFd,
    isBlockDevice, isCharacterDevice, isNamedPipe, isRegularFile,
    isDirectory, isSymbolicLink, isSocket,

    -- * Setting file sizes
    setFdSize,

    -- * Changing file ownership
    setFdOwnerAndGroup,

    -- * Find system-specific limits for a file
    PathVar(..), getFdPathVar, pathVarConst,

    -- * Low level types and functions

{-# LINE 71 "System/Posix/Files/Common.hsc" #-}
    CTimeSpec(..),
    toCTimeSpec,
    c_utimensat,

{-# LINE 75 "System/Posix/Files/Common.hsc" #-}
    CTimeVal(..),
    toCTimeVal,
    c_utimes,

{-# LINE 79 "System/Posix/Files/Common.hsc" #-}
    c_lutimes,

{-# LINE 81 "System/Posix/Files/Common.hsc" #-}
  ) where

import System.Posix.Types
import System.IO.Unsafe
import Data.Bits

{-# LINE 92 "System/Posix/Files/Common.hsc" #-}
import Data.Int
import Data.Ratio

{-# LINE 95 "System/Posix/Files/Common.hsc" #-}
import Data.Time.Clock.POSIX
import System.Posix.Internals
import Foreign.C
import Foreign.ForeignPtr

{-# LINE 100 "System/Posix/Files/Common.hsc" #-}
import Foreign.Marshal (withArray)

{-# LINE 102 "System/Posix/Files/Common.hsc" #-}
import Foreign.Ptr
import Foreign.Storable

-- -----------------------------------------------------------------------------
-- POSIX file modes

-- The abstract type 'FileMode', constants and operators for
-- manipulating the file modes defined by POSIX.

-- | No permissions.
nullFileMode :: FileMode
nullFileMode = 0

-- | Owner has read permission.
ownerReadMode :: FileMode
ownerReadMode = (256)
{-# LINE 118 "System/Posix/Files/Common.hsc" #-}

-- | Owner has write permission.
ownerWriteMode :: FileMode
ownerWriteMode = (128)
{-# LINE 122 "System/Posix/Files/Common.hsc" #-}

-- | Owner has execute permission.
ownerExecuteMode :: FileMode
ownerExecuteMode = (64)
{-# LINE 126 "System/Posix/Files/Common.hsc" #-}

-- | Group has read permission.
groupReadMode :: FileMode
groupReadMode = (32)
{-# LINE 130 "System/Posix/Files/Common.hsc" #-}

-- | Group has write permission.
groupWriteMode :: FileMode
groupWriteMode = (16)
{-# LINE 134 "System/Posix/Files/Common.hsc" #-}

-- | Group has execute permission.
groupExecuteMode :: FileMode
groupExecuteMode = (8)
{-# LINE 138 "System/Posix/Files/Common.hsc" #-}

-- | Others have read permission.
otherReadMode :: FileMode
otherReadMode = (4)
{-# LINE 142 "System/Posix/Files/Common.hsc" #-}

-- | Others have write permission.
otherWriteMode :: FileMode
otherWriteMode = (2)
{-# LINE 146 "System/Posix/Files/Common.hsc" #-}

-- | Others have execute permission.
otherExecuteMode :: FileMode
otherExecuteMode = (1)
{-# LINE 150 "System/Posix/Files/Common.hsc" #-}

-- | Set user ID on execution.
setUserIDMode :: FileMode
setUserIDMode = (2048)
{-# LINE 154 "System/Posix/Files/Common.hsc" #-}

-- | Set group ID on execution.
setGroupIDMode :: FileMode
setGroupIDMode = (1024)
{-# LINE 158 "System/Posix/Files/Common.hsc" #-}

-- | Owner, group and others have read and write permission.
stdFileMode :: FileMode
stdFileMode = ownerReadMode  .|. ownerWriteMode .|.
	      groupReadMode  .|. groupWriteMode .|.
	      otherReadMode  .|. otherWriteMode

-- | Owner has read, write and execute permission.
ownerModes :: FileMode
ownerModes = (448)
{-# LINE 168 "System/Posix/Files/Common.hsc" #-}

-- | Group has read, write and execute permission.
groupModes :: FileMode
groupModes = (56)
{-# LINE 172 "System/Posix/Files/Common.hsc" #-}

-- | Others have read, write and execute permission.
otherModes :: FileMode
otherModes = (7)
{-# LINE 176 "System/Posix/Files/Common.hsc" #-}

-- | Owner, group and others have read, write and execute permission.
accessModes :: FileMode
accessModes = ownerModes .|. groupModes .|. otherModes

-- | Combines the two file modes into one that contains modes that appear in
-- either.
unionFileModes :: FileMode -> FileMode -> FileMode
unionFileModes m1 m2 = m1 .|. m2

-- | Combines two file modes into one that only contains modes that appear in
-- both.
intersectFileModes :: FileMode -> FileMode -> FileMode
intersectFileModes m1 m2 = m1 .&. m2

fileTypeModes :: FileMode
fileTypeModes = (61440)
{-# LINE 193 "System/Posix/Files/Common.hsc" #-}

blockSpecialMode :: FileMode
blockSpecialMode = (24576)
{-# LINE 196 "System/Posix/Files/Common.hsc" #-}

characterSpecialMode :: FileMode
characterSpecialMode = (8192)
{-# LINE 199 "System/Posix/Files/Common.hsc" #-}

namedPipeMode :: FileMode
namedPipeMode = (4096)
{-# LINE 202 "System/Posix/Files/Common.hsc" #-}

regularFileMode :: FileMode
regularFileMode = (32768)
{-# LINE 205 "System/Posix/Files/Common.hsc" #-}

directoryMode :: FileMode
directoryMode = (16384)
{-# LINE 208 "System/Posix/Files/Common.hsc" #-}

symbolicLinkMode :: FileMode
symbolicLinkMode = (40960)
{-# LINE 211 "System/Posix/Files/Common.hsc" #-}

socketMode :: FileMode
socketMode = (49152)
{-# LINE 214 "System/Posix/Files/Common.hsc" #-}

-- | @setFdMode fd mode@ acts like 'setFileMode' but uses a file descriptor
-- @fd@ instead of a 'FilePath'.
--
-- Note: calls @fchmod@.
setFdMode :: Fd -> FileMode -> IO ()
setFdMode (Fd fd) m =
  throwErrnoIfMinus1_ "setFdMode" (c_fchmod fd m)

foreign import ccall unsafe "fchmod"
  c_fchmod :: CInt -> CMode -> IO CInt

-- | @setFileCreationMask mode@ sets the file mode creation mask to @mode@.
-- Modes set by this operation are subtracted from files and directories upon
-- creation. The previous file creation mask is returned.
--
-- Note: calls @umask@.
setFileCreationMask :: FileMode -> IO FileMode
setFileCreationMask mask = c_umask mask

-- -----------------------------------------------------------------------------
-- stat() support

-- | POSIX defines operations to get information, such as owner, permissions,
-- size and access times, about a file. This information is represented by the
-- 'FileStatus' type.
--
-- Note: see @chmod@.
newtype FileStatus = FileStatus (ForeignPtr CStat)

-- | ID of the device on which this file resides.
deviceID         :: FileStatus -> DeviceID
-- | inode number
fileID           :: FileStatus -> FileID
-- | File mode (such as permissions).
fileMode         :: FileStatus -> FileMode
-- | Number of hard links to this file.
linkCount        :: FileStatus -> LinkCount
-- | ID of owner.
fileOwner        :: FileStatus -> UserID
-- | ID of group.
fileGroup        :: FileStatus -> GroupID
-- | Describes the device that this file represents.
specialDeviceID  :: FileStatus -> DeviceID
-- | Size of the file in bytes. If this file is a symbolic link the size is
-- the length of the pathname it contains.
fileSize         :: FileStatus -> FileOffset
-- | Time of last access.
accessTime       :: FileStatus -> EpochTime
-- | Time of last access in sub-second resolution.
accessTimeHiRes  :: FileStatus -> POSIXTime
-- | Time of last modification.
modificationTime :: FileStatus -> EpochTime
-- | Time of last modification in sub-second resolution.
modificationTimeHiRes :: FileStatus -> POSIXTime
-- | Time of last status change (i.e. owner, group, link count, mode, etc.).
statusChangeTime :: FileStatus -> EpochTime
-- | Time of last status change (i.e. owner, group, link count, mode, etc.) in sub-second resolution.
statusChangeTimeHiRes :: FileStatus -> POSIXTime

deviceID (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 0))
{-# LINE 276 "System/Posix/Files/Common.hsc" #-}
fileID (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 8))
{-# LINE 278 "System/Posix/Files/Common.hsc" #-}
fileMode (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 24))
{-# LINE 280 "System/Posix/Files/Common.hsc" #-}
linkCount (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 16))
{-# LINE 282 "System/Posix/Files/Common.hsc" #-}
fileOwner (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 28))
{-# LINE 284 "System/Posix/Files/Common.hsc" #-}
fileGroup (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 32))
{-# LINE 286 "System/Posix/Files/Common.hsc" #-}
specialDeviceID (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 40))
{-# LINE 288 "System/Posix/Files/Common.hsc" #-}
fileSize (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 48))
{-# LINE 290 "System/Posix/Files/Common.hsc" #-}
accessTime (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 72))
{-# LINE 292 "System/Posix/Files/Common.hsc" #-}
modificationTime (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 88))
{-# LINE 294 "System/Posix/Files/Common.hsc" #-}
statusChangeTime (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 104))
{-# LINE 296 "System/Posix/Files/Common.hsc" #-}

accessTimeHiRes (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ \stat_ptr -> do
    sec  <- ((\hsc_ptr -> peekByteOff hsc_ptr 72)) stat_ptr :: IO EpochTime
{-# LINE 300 "System/Posix/Files/Common.hsc" #-}

{-# LINE 301 "System/Posix/Files/Common.hsc" #-}
    nsec <- ((\hsc_ptr -> peekByteOff hsc_ptr 80)) stat_ptr :: IO (Int64)
{-# LINE 302 "System/Posix/Files/Common.hsc" #-}
    let frac = toInteger nsec % 10^(9::Int)

{-# LINE 318 "System/Posix/Files/Common.hsc" #-}
    return $ fromRational $ toRational sec + frac

modificationTimeHiRes (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ \stat_ptr -> do
    sec  <- ((\hsc_ptr -> peekByteOff hsc_ptr 88)) stat_ptr :: IO EpochTime
{-# LINE 323 "System/Posix/Files/Common.hsc" #-}

{-# LINE 324 "System/Posix/Files/Common.hsc" #-}
    nsec <- ((\hsc_ptr -> peekByteOff hsc_ptr 96)) stat_ptr :: IO (Int64)
{-# LINE 325 "System/Posix/Files/Common.hsc" #-}
    let frac = toInteger nsec % 10^(9::Int)

{-# LINE 341 "System/Posix/Files/Common.hsc" #-}
    return $ fromRational $ toRational sec + frac

statusChangeTimeHiRes (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ \stat_ptr -> do
    sec  <- ((\hsc_ptr -> peekByteOff hsc_ptr 104)) stat_ptr :: IO EpochTime
{-# LINE 346 "System/Posix/Files/Common.hsc" #-}

{-# LINE 347 "System/Posix/Files/Common.hsc" #-}
    nsec <- ((\hsc_ptr -> peekByteOff hsc_ptr 112)) stat_ptr :: IO (Int64)
{-# LINE 348 "System/Posix/Files/Common.hsc" #-}
    let frac = toInteger nsec % 10^(9::Int)

{-# LINE 364 "System/Posix/Files/Common.hsc" #-}
    return $ fromRational $ toRational sec + frac

-- | Checks if this file is a block device.
isBlockDevice     :: FileStatus -> Bool
-- | Checks if this file is a character device.
isCharacterDevice :: FileStatus -> Bool
-- | Checks if this file is a named pipe device.
isNamedPipe       :: FileStatus -> Bool
-- | Checks if this file is a regular file device.
isRegularFile     :: FileStatus -> Bool
-- | Checks if this file is a directory device.
isDirectory       :: FileStatus -> Bool
-- | Checks if this file is a symbolic link device.
isSymbolicLink    :: FileStatus -> Bool
-- | Checks if this file is a socket device.
isSocket          :: FileStatus -> Bool

isBlockDevice stat =
  (fileMode stat `intersectFileModes` fileTypeModes) == blockSpecialMode
isCharacterDevice stat =
  (fileMode stat `intersectFileModes` fileTypeModes) == characterSpecialMode
isNamedPipe stat =
  (fileMode stat `intersectFileModes` fileTypeModes) == namedPipeMode
isRegularFile stat =
  (fileMode stat `intersectFileModes` fileTypeModes) == regularFileMode
isDirectory stat =
  (fileMode stat `intersectFileModes` fileTypeModes) == directoryMode
isSymbolicLink stat =
  (fileMode stat `intersectFileModes` fileTypeModes) == symbolicLinkMode
isSocket stat =
  (fileMode stat `intersectFileModes` fileTypeModes) == socketMode

-- | @getFdStatus fd@ acts as 'getFileStatus' but uses a file descriptor @fd@.
--
-- Note: calls @fstat@.
getFdStatus :: Fd -> IO FileStatus
getFdStatus (Fd fd) = do
  fp <- mallocForeignPtrBytes (144)
{-# LINE 402 "System/Posix/Files/Common.hsc" #-}
  withForeignPtr fp $ \p ->
    throwErrnoIfMinus1_ "getFdStatus" (c_fstat fd p)
  return (FileStatus fp)

-- -----------------------------------------------------------------------------
-- Setting file times


{-# LINE 410 "System/Posix/Files/Common.hsc" #-}
data CTimeSpec = CTimeSpec EpochTime CLong

instance Storable CTimeSpec where
    sizeOf    _ = (16)
{-# LINE 414 "System/Posix/Files/Common.hsc" #-}
    alignment _ = alignment (undefined :: CInt)
    poke p (CTimeSpec sec nsec) = do
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p sec
{-# LINE 417 "System/Posix/Files/Common.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p nsec
{-# LINE 418 "System/Posix/Files/Common.hsc" #-}
    peek p = do
        sec  <- (\hsc_ptr -> peekByteOff hsc_ptr 0) p
{-# LINE 420 "System/Posix/Files/Common.hsc" #-}
        nsec <- (\hsc_ptr -> peekByteOff hsc_ptr 8) p
{-# LINE 421 "System/Posix/Files/Common.hsc" #-}
        return $ CTimeSpec sec nsec

toCTimeSpec :: POSIXTime -> CTimeSpec
toCTimeSpec t = CTimeSpec (CTime sec) (truncate $ 10^(9::Int) * frac)
  where
    (sec, frac) = if (frac' < 0) then (sec' - 1, frac' + 1) else (sec', frac')
    (sec', frac') = properFraction $ toRational t

{-# LINE 429 "System/Posix/Files/Common.hsc" #-}


{-# LINE 431 "System/Posix/Files/Common.hsc" #-}
foreign import ccall unsafe "utimensat"
    c_utimensat :: CInt -> CString -> Ptr CTimeSpec -> CInt -> IO CInt

{-# LINE 434 "System/Posix/Files/Common.hsc" #-}


{-# LINE 436 "System/Posix/Files/Common.hsc" #-}
foreign import ccall unsafe "futimens"
    c_futimens :: CInt -> Ptr CTimeSpec -> IO CInt

{-# LINE 439 "System/Posix/Files/Common.hsc" #-}

data CTimeVal = CTimeVal CLong CLong

instance Storable CTimeVal where
    sizeOf    _ = (16)
{-# LINE 444 "System/Posix/Files/Common.hsc" #-}
    alignment _ = alignment (undefined :: CInt)
    poke p (CTimeVal sec usec) = do
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p sec
{-# LINE 447 "System/Posix/Files/Common.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p usec
{-# LINE 448 "System/Posix/Files/Common.hsc" #-}
    peek p = do
        sec  <- (\hsc_ptr -> peekByteOff hsc_ptr 0) p
{-# LINE 450 "System/Posix/Files/Common.hsc" #-}
        usec <- (\hsc_ptr -> peekByteOff hsc_ptr 8) p
{-# LINE 451 "System/Posix/Files/Common.hsc" #-}
        return $ CTimeVal sec usec

toCTimeVal :: POSIXTime -> CTimeVal
toCTimeVal t = CTimeVal sec (truncate $ 10^(6::Int) * frac)
  where
    (sec, frac) = if (frac' < 0) then (sec' - 1, frac' + 1) else (sec', frac')
    (sec', frac') = properFraction $ toRational t

foreign import ccall unsafe "utimes"
    c_utimes :: CString -> Ptr CTimeVal -> IO CInt


{-# LINE 463 "System/Posix/Files/Common.hsc" #-}
foreign import ccall unsafe "lutimes"
    c_lutimes :: CString -> Ptr CTimeVal -> IO CInt

{-# LINE 466 "System/Posix/Files/Common.hsc" #-}


{-# LINE 468 "System/Posix/Files/Common.hsc" #-}
foreign import ccall unsafe "futimes"
    c_futimes :: CInt -> Ptr CTimeVal -> IO CInt

{-# LINE 471 "System/Posix/Files/Common.hsc" #-}

-- | Like 'setFileTimesHiRes' but uses a file descriptor instead of a path.
-- This operation is not supported on all platforms. On these platforms,
-- this function will raise an exception.
--
-- Note: calls @futimens@ or @futimes@.
--
-- /Since: 2.7.0.0/
setFdTimesHiRes :: Fd -> POSIXTime -> POSIXTime -> IO ()

{-# LINE 481 "System/Posix/Files/Common.hsc" #-}
setFdTimesHiRes (Fd fd) atime mtime =
  withArray [toCTimeSpec atime, toCTimeSpec mtime] $ \times ->
    throwErrnoIfMinus1_ "setFdTimesHiRes" (c_futimens fd times)

{-# LINE 492 "System/Posix/Files/Common.hsc" #-}

-- | Like 'touchFile' but uses a file descriptor instead of a path.
-- This operation is not supported on all platforms. On these platforms,
-- this function will raise an exception.
--
-- Note: calls @futimes@.
--
-- /Since: 2.7.0.0/
touchFd :: Fd -> IO ()

{-# LINE 502 "System/Posix/Files/Common.hsc" #-}
touchFd (Fd fd) =
  throwErrnoIfMinus1_ "touchFd" (c_futimes fd nullPtr)

{-# LINE 508 "System/Posix/Files/Common.hsc" #-}

-- -----------------------------------------------------------------------------
-- fchown()

-- | Acts as 'setOwnerAndGroup' but uses a file descriptor instead of a
-- 'FilePath'.
--
-- Note: calls @fchown@.
setFdOwnerAndGroup :: Fd -> UserID -> GroupID -> IO ()
setFdOwnerAndGroup (Fd fd) uid gid =
  throwErrnoIfMinus1_ "setFdOwnerAndGroup" (c_fchown fd uid gid)

foreign import ccall unsafe "fchown"
  c_fchown :: CInt -> CUid -> CGid -> IO CInt

-- -----------------------------------------------------------------------------
-- ftruncate()

-- | Acts as 'setFileSize' but uses a file descriptor instead of a 'FilePath'.
--
-- Note: calls @ftruncate@.
setFdSize :: Fd -> FileOffset -> IO ()
setFdSize (Fd fd) off =
  throwErrnoIfMinus1_ "setFdSize" (c_ftruncate fd off)

-- -----------------------------------------------------------------------------
-- pathconf()/fpathconf() support

data PathVar
  = FileSizeBits		  {- _PC_FILESIZEBITS     -}
  | LinkLimit                     {- _PC_LINK_MAX         -}
  | InputLineLimit                {- _PC_MAX_CANON        -}
  | InputQueueLimit               {- _PC_MAX_INPUT        -}
  | FileNameLimit                 {- _PC_NAME_MAX         -}
  | PathNameLimit                 {- _PC_PATH_MAX         -}
  | PipeBufferLimit               {- _PC_PIPE_BUF         -}
				  -- These are described as optional in POSIX:
  				  {- _PC_ALLOC_SIZE_MIN     -}
  				  {- _PC_REC_INCR_XFER_SIZE -}
  				  {- _PC_REC_MAX_XFER_SIZE  -}
  				  {- _PC_REC_MIN_XFER_SIZE  -}
 				  {- _PC_REC_XFER_ALIGN     -}
  | SymbolicLinkLimit		  {- _PC_SYMLINK_MAX      -}
  | SetOwnerAndGroupIsRestricted  {- _PC_CHOWN_RESTRICTED -}
  | FileNamesAreNotTruncated      {- _PC_NO_TRUNC         -}
  | VDisableChar		  {- _PC_VDISABLE         -}
  | AsyncIOAvailable		  {- _PC_ASYNC_IO         -}
  | PrioIOAvailable		  {- _PC_PRIO_IO          -}
  | SyncIOAvailable		  {- _PC_SYNC_IO          -}

pathVarConst :: PathVar -> CInt
pathVarConst v = case v of
	LinkLimit     			-> (0)
{-# LINE 561 "System/Posix/Files/Common.hsc" #-}
	InputLineLimit			-> (1)
{-# LINE 562 "System/Posix/Files/Common.hsc" #-}
	InputQueueLimit			-> (2)
{-# LINE 563 "System/Posix/Files/Common.hsc" #-}
	FileNameLimit			-> (3)
{-# LINE 564 "System/Posix/Files/Common.hsc" #-}
	PathNameLimit			-> (4)
{-# LINE 565 "System/Posix/Files/Common.hsc" #-}
	PipeBufferLimit			-> (5)
{-# LINE 566 "System/Posix/Files/Common.hsc" #-}
	SetOwnerAndGroupIsRestricted	-> (6)
{-# LINE 567 "System/Posix/Files/Common.hsc" #-}
	FileNamesAreNotTruncated	-> (7)
{-# LINE 568 "System/Posix/Files/Common.hsc" #-}
	VDisableChar			-> (8)
{-# LINE 569 "System/Posix/Files/Common.hsc" #-}


{-# LINE 571 "System/Posix/Files/Common.hsc" #-}
	SyncIOAvailable		-> (9)
{-# LINE 572 "System/Posix/Files/Common.hsc" #-}

{-# LINE 575 "System/Posix/Files/Common.hsc" #-}


{-# LINE 577 "System/Posix/Files/Common.hsc" #-}
	AsyncIOAvailable	-> (10)
{-# LINE 578 "System/Posix/Files/Common.hsc" #-}

{-# LINE 581 "System/Posix/Files/Common.hsc" #-}


{-# LINE 583 "System/Posix/Files/Common.hsc" #-}
	PrioIOAvailable		-> (11)
{-# LINE 584 "System/Posix/Files/Common.hsc" #-}

{-# LINE 587 "System/Posix/Files/Common.hsc" #-}


{-# LINE 591 "System/Posix/Files/Common.hsc" #-}
	FileSizeBits		-> error "_PC_FILESIZEBITS not available"

{-# LINE 593 "System/Posix/Files/Common.hsc" #-}


{-# LINE 597 "System/Posix/Files/Common.hsc" #-}
	SymbolicLinkLimit	-> error "_PC_SYMLINK_MAX not available"

{-# LINE 599 "System/Posix/Files/Common.hsc" #-}

-- | @getFdPathVar var fd@ obtains the dynamic value of the requested
-- configurable file limit or option associated with the file or directory
-- attached to the open channel @fd@. For defined file limits, @getFdPathVar@
-- returns the associated value.  For defined file options, the result of
-- @getFdPathVar@ is undefined, but not failure.
--
-- Note: calls @fpathconf@.
getFdPathVar :: Fd -> PathVar -> IO Limit
getFdPathVar (Fd fd) v =
    throwErrnoIfMinus1 "getFdPathVar" $
      c_fpathconf fd (pathVarConst v)

foreign import ccall unsafe "fpathconf"
  c_fpathconf :: CInt -> CInt -> IO CLong
