{-# LANGUAGE OverloadedStrings #-}

module Init (init) where

import Prelude hiding (init)
import Fragnix.Paths(fragnixBasePath, environmentPath, builtinEnvironmentPath, cbitsPath, includePath)
import Fragnix.Environment (persistEnvironment)
import Utils (archiveRequest, extract)

import Network.HTTP.Req (responseBody)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Control.Monad (forM_)
import Data.Text (pack)
import Data.Map (empty)

init :: IO ()
init = do
  createDirectoryIfMissing True fragnixBasePath
  createDirectoryIfMissing True environmentPath
  forM_ [("builtin", builtinEnvironmentPath), ("cbits", cbitsPath), ("include", includePath)] (\(archive, path) -> do
    putStrLn $ "Fetching " ++ archive ++ " ..."
    r <- archiveRequest . pack $ archive ++ ".tar.gz"
    extract path $ responseBody r)
  persistEnvironment (environmentPath </> "Fragnix") empty