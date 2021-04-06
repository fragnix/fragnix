{-# LANGUAGE OverloadedStrings #-}

module Init where

import Fragnix.Paths(fragnixBasePath, environmentPath, builtinEnvironmentPath, cbitsPath, includePath)
import Utils (archiveRequest, extract)

import Network.HTTP.Req (responseBody)
import System.Directory (createDirectoryIfMissing)
import Control.Monad (forM_)

init :: IO ()
init = do
  createDirectoryIfMissing True fragnixBasePath
  createDirectoryIfMissing True environmentPath
  forM_ [("builtin.tar.gz", builtinEnvironmentPath), ("cbits.tar.gz", cbitsPath), ("include.tar.gz", includePath)] (\(archive, path) -> do
    r <- archiveRequest archive
    extract path $ responseBody r)
