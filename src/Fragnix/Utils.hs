module Fragnix.Utils where

import Control.Monad (forM, filterM)
import System.Directory (listDirectory,doesDirectoryExist,doesFileExist)
import System.FilePath ((</>))


-- | Find all files in the given directory and subdirectories.
listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive directory = do
  children <- listDirectory directory
  let childrenPaths = map (\path -> directory </> path) children
  childrenDirectories <- filterM doesDirectoryExist childrenPaths
  childrenFiles <- filterM doesFileExist childrenPaths
  subChildrenFiles <- forM childrenDirectories listFilesRecursive
  return (childrenFiles ++ concat subChildrenFiles)

