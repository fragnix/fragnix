{-# LINE 1 "dist/dist-sandbox-d76e0d17/build/System/PosixCompat/Files.hs" #-}











































{-# LINE 1 "src/System/PosixCompat/Files.hsc" #-}
{-# LANGUAGE CPP #-}
{-# LINE 2 "src/System/PosixCompat/Files.hsc" #-}

{-|
This module makes the operations exported by @System.Posix.Files@
available on all platforms. On POSIX systems it re-exports operations from
@System.Posix.Files@. On other platforms it emulates the operations as far
as possible.

/NOTE: the portable implementations are not well tested, in some cases
functions are only stubs./
-}
module System.PosixCompat.Files (
    -- * File modes
    -- FileMode exported by System.Posix.Types
      unionFileModes
    , intersectFileModes
    , nullFileMode
    , ownerReadMode
    , ownerWriteMode
    , ownerExecuteMode
    , ownerModes
    , groupReadMode
    , groupWriteMode
    , groupExecuteMode
    , groupModes
    , otherReadMode
    , otherWriteMode
    , otherExecuteMode
    , otherModes
    , setUserIDMode
    , setGroupIDMode
    , stdFileMode
    , accessModes

    -- ** Setting file modes
    , setFileMode
    , setFdMode
    , setFileCreationMask

    -- ** Checking file existence and permissions
    , fileAccess
    , fileExist

    -- * File status
    , FileStatus
    -- ** Obtaining file status
    , getFileStatus
    , getFdStatus
    , getSymbolicLinkStatus
    -- ** Querying file status
    , deviceID
    , fileID
    , fileMode
    , linkCount
    , fileOwner
    , fileGroup
    , specialDeviceID
    , fileSize
    , accessTime
    , modificationTime
    , statusChangeTime
    , isBlockDevice
    , isCharacterDevice
    , isNamedPipe
    , isRegularFile
    , isDirectory
    , isSymbolicLink
    , isSocket

    -- * Creation
    , createNamedPipe
    , createDevice

    -- * Hard links
    , createLink
    , removeLink

    -- * Symbolic links
    , createSymbolicLink
    , readSymbolicLink

    -- * Renaming files
    , rename

    -- * Changing file ownership
    , setOwnerAndGroup
    , setFdOwnerAndGroup
    , setSymbolicLinkOwnerAndGroup

    -- * Changing file timestamps
    , setFileTimes
    , touchFile

    -- * Setting file sizes
    , setFileSize
    , setFdSize

    -- * Find system-specific limits for a file
    , PathVar(..)
    , getPathVar
    , getFdPathVar
    ) where


{-# LINE 105 "src/System/PosixCompat/Files.hsc" #-}


{-# LINE 107 "src/System/PosixCompat/Files.hsc" #-}

import System.Posix.Files


{-# LINE 116 "src/System/PosixCompat/Files.hsc" #-}


{-# LINE 462 "src/System/PosixCompat/Files.hsc" #-}
