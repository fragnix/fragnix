{-# LINE 1 "System/EasyFile/Missing.hs" #-}
# 1 "System/EasyFile/Missing.hs"
# 1 "<command-line>"
# 8 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 8 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1











































































































# 8 "<command-line>" 2
# 1 "System/EasyFile/Missing.hs"
{-# LANGUAGE CPP #-}

module System.EasyFile.Missing where

----------------------------------------------------------------

import Control.Applicative
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word (Word64)





import System.Posix.Files
import System.Posix.Types


----------------------------------------------------------------

{-|
  This function tells whether or not a file\/directory is symbolic
  link.
-}
isSymlink :: FilePath -> IO Bool



isSymlink file = isSymbolicLink <$> getSymbolicLinkStatus file


{-|
  This function returns the link counter of a file\/directory.
-}
getLinkCount :: FilePath -> IO (Maybe Int)



getLinkCount file = Just . fromIntegral . linkCount <$> getFileStatus file


{-|
  This function returns whether or not a directory has sub-directories.
-}
hasSubDirectories :: FilePath -> IO (Maybe Bool)



hasSubDirectories file = do
  Just n <- getLinkCount file
  return $ Just (n > 2)


----------------------------------------------------------------

{-|
The 'getCreationTime' operation returns the
UTC time at which the file or directory was created.
The time is only available on Windows.
-}
getCreationTime :: FilePath -> IO (Maybe UTCTime)



getCreationTime _ = return Nothing


{-|
The 'getCreationTime' operation returns the
UTC time at which the file or directory was changed.
The time is only available on Unix and Mac.
Note that Unix's rename() does not change ctime but
MacOS's rename() does.
-}
getChangeTime :: FilePath -> IO (Maybe UTCTime)



getChangeTime file = Just . epochTimeToUTCTime . statusChangeTime <$> getFileStatus file


{-|
The 'getModificationTime' operation returns the
UTC time at which the file or directory was last modified.

The operation may fail with:

* 'isPermissionError' if the user is not permitted to access
  the modification time; or

* 'isDoesNotExistError' if the file or directory does not exist.

-}
getModificationTime :: FilePath -> IO UTCTime



getModificationTime file = epochTimeToUTCTime . modificationTime <$> getFileStatus file


{-
  http://msdn.microsoft.com/en-us/library/ms724290%28VS.85%29.aspx
  The NTFS file system delays updates to the last access time for
  a file by up to 1 hour after the last access.
-}
{-|
The 'getModificationTime' operation returns the
UTC time at which the file or directory was last accessed.
-}
getAccessTime :: FilePath -> IO UTCTime



getAccessTime file = epochTimeToUTCTime . accessTime <$> getFileStatus file


----------------------------------------------------------------

# 151 "System/EasyFile/Missing.hs"
epochTimeToUTCTime :: EpochTime -> UTCTime
epochTimeToUTCTime = posixSecondsToUTCTime . realToFrac


-- | Getting the size of the file.
getFileSize :: FilePath -> IO Word64







getFileSize file = fromIntegral . fileSize <$> getFileStatus file
