module Fragnix.Utils where

import Control.Monad (forM, filterM)
import Control.Exception (catch, throwIO)
import System.Directory (listDirectory,doesDirectoryExist,doesFileExist, removeFile)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)


-- | Find all files in the given directory and subdirectories.
-- If the given path refers to a file, we return just this file.
listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive path = do
  isFile <- doesFileExist path
  if isFile
    then do
      return [path]
    else do
      children <- listDirectory path
      let childrenPaths = map (\childPath -> path </> childPath) children
      childrenDirectories <- filterM doesDirectoryExist childrenPaths
      childrenFiles <- filterM doesFileExist childrenPaths
      subChildrenFiles <- forM childrenDirectories listFilesRecursive
      return (childrenFiles ++ concat subChildrenFiles)


-- | Delete file if it exists at path.
deleteFileIfExists :: FilePath -> IO ()
deleteFileIfExists file = removeFile file `catch` handleException
  where handleException e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
