{-# LANGUAGE OverloadedStrings #-}

module Init where

import Fragnix.Paths(fragnixBasePath, environmentPath, builtinEnvironmentPath, cbitsPath, includePath)
import Utils (archiveRequest, extract)

import Network.HTTP.Req (responseBody)
import System.Directory (createDirectoryIfMissing)
import Control.Monad (forM_)
import Data.Text (pack)

init :: IO ()
init = do
  createDirectoryIfMissing True fragnixBasePath
  createDirectoryIfMissing True environmentPath
  forM_ [("builtin", builtinEnvironmentPath), ("cbits", cbitsPath), ("include", includePath)] (\(archive, path) -> do
    putStrLn $ "Fetching " ++ archive ++ " ..."
    r <- archiveRequest . pack $ archive ++ ".tar.gz"
    extract path $ responseBody r)
