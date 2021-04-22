module Fragnix.Config where

import Prelude hiding (readFile, writeFile)

import Fragnix.Core.Config (Config)

import Data.ByteString (readFile)
import Data.Yaml (decodeThrow, encodeFile)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName, (</>))

-- | Read the config file
readConfig :: FilePath -> IO Config
readConfig configPath = do
  configFile <- readFile configPath
  decodeThrow configFile

-- | Write the config file
writeConfig :: FilePath -> Config -> IO ()
writeConfig configPath config = do
  let configFilePath = configPath </> "fragnix.yaml"
  createDirectoryIfMissing True (dropFileName configPath)
  encodeFile configFilePath config