module Preprocess where

import Fragnix.Utils (listFilesRecursive)
import Fragnix.Paths (preprocessedPath)

import System.Process (rawSystem)
import System.Directory (removeDirectoryRecursive, createDirectoryIfMissing)
import System.FilePath ((</>), takeExtension, splitDirectories, joinPath)
import Data.List (intersperse)
import Control.Monad (forM_)


-- | Run GHC's preprocessor on files in the given directory.
-- Output is in fragnix folder.
preprocess :: FilePath -> IO ()
preprocess directory = do
  createDirectoryIfMissing True preprocessedPath
  removeDirectoryRecursive preprocessedPath
  createDirectoryIfMissing True preprocessedPath
  filePaths <- listFilesRecursive directory
  let modulePaths = filter (\path -> takeExtension path == ".hs") filePaths
  forM_ modulePaths (\path -> do
    rawSystem "ghc-8.0.2" [
      "-E",
      "-optP","-P",
      "-optL","-P",
      "-Iinclude",
      "-o", modulePreprocessedPath path,
      path])

-- | Replace slashes by dots.
modulePreprocessedPath :: FilePath -> FilePath
modulePreprocessedPath path = joinPath [
  preprocessedPath,
  concat (intersperse "." (splitDirectories path))]

