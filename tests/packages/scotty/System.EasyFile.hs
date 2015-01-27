{-# LINE 1 "System/EasyFile.hs" #-}
{-|
This is a module of cross-platform file handling for Unix\/Mac\/Windows.

The standard module "System.Directory" and "System.FilePath" have
following shortcomings:

* getModificationTime exists in "System.Directory". But getAccessTime,
  getChangeTime, getCreationTime do not exist.

* getModificationTime returns obsoleted type, 'ClockTime'. It should
  return modern type, 'UTCTime', I believe.

* Some file functions are missing. A function to tell the link counter,
  for instance.

* Path separator is not unified. Even though Windows accepts \'\/\' as a
  file separator, getCurrentDirectory in "System.Directory" returns \'\\\'
  as a file separator. So, we need to specify regular expression like
  this: \"[\/\\\\]foo[\/\\\\]bar[\/\\\\]baz\".

* getHomeDirectory returns @HOMEDRIVE@\/@HOMEPATH@ instead of the @HOME@
  environment variable on Windows.

This module aims to resolve these problems and provides:

* 'getModificationTime', 'getAccessTime', 'getChangeTime', and
  'getCreationTime'. They return 'UTCTime'.

* 'isSymlink', 'getLinkCount', and 'hasSubDirectories'.

* \'\/\' as the single 'pathSeparator'. For instance,
  'getCurrentDirectory' returns a path whose separator is \'\/\'
  even on Windows.

* 'getHomeDirectory2' which refers the @HOME@ environment variable.

* Necessary functions in "System.Directory" and "System.FilePath".

-}

module System.EasyFile (
  -- * Actions on directories
    createDirectory
  , createDirectoryIfMissing
  , removeDirectory
  , removeDirectoryRecursive
  , renameDirectory
  , getDirectoryContents
  , getCurrentDirectory
  , setCurrentDirectory
  -- * Pre-defined directories
  , getHomeDirectory
  , getHomeDirectory2 -- missing
  , getAppUserDataDirectory
  , getUserDocumentsDirectory
  , getTemporaryDirectory
  -- * Actions on files
  , removeFile
  , renameFile
  , copyFile
  , canonicalizePath
--   , makeRelativeToCurrentDirectory -- xxx
--   , findExecutable -- xxx
  -- * Existence tests
  , doesFileExist
  , doesDirectoryExist
  -- * Permissions
  , Permissions(..)
  , getPermissions
  , setPermissions
  , copyPermissions
  -- * Timestamps
  , getCreationTime
  , getChangeTime
  , getModificationTime
  , getAccessTime
  -- * Size
  , getFileSize
  -- * File\/directory information
  , isSymlink
  , getLinkCount
  , hasSubDirectories
  , module System.EasyFile.FilePath
  ) where

----------------------------------------------------------------

import System.EasyFile.Directory
import System.EasyFile.FilePath
import System.EasyFile.Missing
