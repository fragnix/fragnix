{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module Init where

import Fragnix.Core.Config (Config (..))

import Fragnix.Basket (basketToLoafIDs)
import Fragnix.Config (readConfig, writeConfig)
import Fragnix.Paths
    (builtinEnvironmentPath, cbitsPath, configPath, environmentPath,
    fragnixBasePath, includePath)
import Get (fetchForeignSlice, getLoaf, getSlice)
import Utils (WithDeps (..), archiveRequest, extract)

import Control.Monad (forM_)
import qualified Data.Map as Map (empty)
import Data.Text (pack)
import Network.HTTP.Req (responseBody)
import System.Directory (createDirectoryIfMissing, doesFileExist)

init :: IO ()
init = do
  createDirectoryIfMissing True fragnixBasePath
  createDirectoryIfMissing True environmentPath
  forM_ [("builtin", builtinEnvironmentPath), ("cbits", cbitsPath), ("include", includePath)] (\(archive, path) -> do
    putStrLn $ "Fetching " ++ archive ++ " ..."
    r <- archiveRequest . pack $ archive ++ ".tar.gz"
    extract path $ responseBody r)
  configExists <- doesFileExist configPath
  if configExists
    then do
      Config {loaves, slices, foreignSlices} <- readConfig configPath
      forM_ slices (\sliceID -> getSlice sliceID WithDeps)
      forM_ foreignSlices fetchForeignSlice
      forM_ (basketToLoafIDs loaves) (\loafID -> getLoaf loafID WithDeps)
    else do
      writeConfig configPath Config { name = ""
                                    , authors = ""
                                    , description = ""
                                    , loaves = Map.empty
                                    , slices = []
                                    , foreignSlices = []
                                    , mainIs = "Main.hs"
                                    , cbitsFolder = "cbits"
                                    , license = ""
                                    , categories = []}

