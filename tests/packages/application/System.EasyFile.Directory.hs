{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "System/EasyFile/Directory.hs" #-}

















































{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.EasyFile.Directory (
    module System.EasyFile.Directory
  , module System.Directory
  ) where

import System.Directory (
    createDirectory
  , createDirectoryIfMissing
  , removeDirectory
  , removeDirectoryRecursive
  , renameDirectory
  , getDirectoryContents
  , setCurrentDirectory
  , removeFile
  , renameFile
  , copyFile
  , canonicalizePath
  , doesFileExist
  , doesDirectoryExist
  , Permissions(..)
  , getPermissions
  , setPermissions
  )

import qualified System.Directory as D (
    getCurrentDirectory
  , getHomeDirectory
  , getAppUserDataDirectory
  , getUserDocumentsDirectory
  , getTemporaryDirectory
  , copyPermissions
  )

import Control.Applicative
import qualified Control.Exception as E 
import System.Environment

----------------------------------------------------------------

{- |If the operating system has a notion of current directories,
'getCurrentDirectory' returns an absolute path to the
current directory of the calling process.

The operation may fail with:

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'isDoesNotExistError' \/ 'NoSuchThing'
There is no path referring to the current directory.
@[EPERM, ENOENT, ESTALE...]@

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EACCES]@

* 'ResourceExhausted'
Insufficient resources are available to perform the operation.

* 'UnsupportedOperation'
The operating system has no notion of current directory.

-}
getCurrentDirectory :: IO FilePath
getCurrentDirectory = fixPath <$> D.getCurrentDirectory

{- | Returns the current user's home directory.

The directory returned is expected to be writable by the current user,
but note that it isn't generally considered good practice to store
application-specific data here; use 'getAppUserDataDirectory'
instead.

On Unix, 'getHomeDirectory' returns the value of the @HOME@
environment variable.  On Windows, the system is queried for a
suitable path; a typical path might be
@C:/Documents And Settings/user@.

The operation may fail with:

* 'UnsupportedOperation'
The operating system has no notion of home directory.

* 'isDoesNotExistError'
The home directory for the current user does not exist, or
cannot be found.
-}
getHomeDirectory :: IO FilePath
getHomeDirectory = fixPath <$> D.getHomeDirectory

{- | Returns the current user's home directory from
the @HOME@ environment variable.
-}

getHomeDirectory2 :: IO (Maybe FilePath)
getHomeDirectory2 = (Just . fixPath <$> getEnv "HOME") `E.catch` 
                    \(_ :: E.IOException) -> return Nothing

{- | Returns the pathname of a directory in which application-specific
data for the current user can be stored.  The result of
'getAppUserDataDirectory' for a given application is specific to
the current user.

The argument should be the name of the application, which will be used
to construct the pathname (so avoid using unusual characters that
might result in an invalid pathname).

Note: the directory may not actually exist, and may need to be created
first.  It is expected that the parent directory exists and is
writable.

On Unix, this function returns @$HOME\/.appName@.  On Windows, a
typical path might be

> C:/Documents And Settings/user/Application Data/appName

The operation may fail with:

* 'UnsupportedOperation'
The operating system has no notion of application-specific data directory.

* 'isDoesNotExistError'
The home directory for the current user does not exist, or
cannot be found.
-}

getAppUserDataDirectory :: String -> IO FilePath
getAppUserDataDirectory x = fixPath <$> D.getAppUserDataDirectory x

{- | Returns the current user's document directory.

The directory returned is expected to be writable by the current user,
but note that it isn't generally considered good practice to store
application-specific data here; use 'getAppUserDataDirectory'
instead.

On Unix, 'getUserDocumentsDirectory' returns the value of the @HOME@
environment variable.  On Windows, the system is queried for a
suitable path; a typical path might be
@C:\/Documents and Settings\/user\/My Documents@.

The operation may fail with:

* 'UnsupportedOperation'
The operating system has no notion of document directory.

* 'isDoesNotExistError'
The document directory for the current user does not exist, or
cannot be found.
-}
getUserDocumentsDirectory :: IO FilePath
getUserDocumentsDirectory = fixPath <$> D.getUserDocumentsDirectory

{- | Returns the current directory for temporary files.

On Unix, 'getTemporaryDirectory' returns the value of the @TMPDIR@
environment variable or \"\/tmp\" if the variable isn\'t defined.
On Windows, the function checks for the existence of environment variables in
the following order and uses the first path found:

* TMP environment variable.

* TEMP environment variable.

* USERPROFILE environment variable.

* The Windows directory

The operation may fail with:

* 'UnsupportedOperation'
The operating system has no notion of temporary directory.

The function doesn\'t verify whether the path exists.
-}

getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = fixPath <$> D.getTemporaryDirectory

----------------------------------------------------------------

fixPath :: FilePath -> FilePath
fixPath = id

----------------------------------------------------------------
-- Just adding documentation.

{-|
  This function copy the permission of the first file to the second.
-}
copyPermissions :: FilePath -> FilePath -> IO ()
copyPermissions = D.copyPermissions
