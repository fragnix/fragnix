module Fragnix.Utils (listFilesRecursive) where

import Fragnix.Paths (fragnixBasePath)

import Control.Monad (filterM, forM)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))


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
      let childrenWithoutFragnix = filter (/= fragnixBasePath) children
      let childrenPaths = map (\childPath -> path </> childPath) childrenWithoutFragnix
      childrenDirectories <- filterM doesDirectoryExist childrenPaths
      childrenFiles <- filterM doesFileExist childrenPaths
      subChildrenFiles <- forM childrenDirectories listFilesRecursive
      return (childrenFiles ++ concat subChildrenFiles)

