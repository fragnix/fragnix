{-# LANGUAGE OverloadedStrings #-}

module Fragnix.Environment
    ( loadMetaEnvironment
    , persistMetaEnvironment
    , writeEnvironment) where

import Fragnix.Core.Environment (Environment)
import Fragnix.Core.MetaEnvironment (MetaEnvironment)

import Control.Monad (filterM, forM, forM_)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BS (pack, writeFile)
import Data.Char (ord)
import qualified Data.Map as Map (fromList, toList)
import Data.Text (pack, unpack)

import System.Directory
    (createDirectoryIfMissing, doesFileExist, getDirectoryContents)
import System.FilePath ((</>))

loadMetaEnvironment :: FilePath -> IO MetaEnvironment
loadMetaEnvironment path = do
    createDirectoryIfMissing True path
    filenames <- getDirectoryContents path
    let pathmodulnames = map (\filename -> (filename, path </> filename)) filenames
    existingPathModulNames <- filterM (doesFileExist . snd) pathmodulnames
    fmap Map.fromList (forM existingPathModulNames (\(modulname,modulpath) -> do
        env <- readEnvironment modulpath
        return (pack modulname, env)))

readEnvironment :: FilePath -> IO Environment
readEnvironment path = do
    file <- readFile path
    return $ read file

-- Replaces "writeSymbols" from haskell-names. Uses encodePretty instead.
writeEnvironment :: FilePath -> Environment -> IO ()
writeEnvironment path env =
    BS.writeFile path $ encodePretty env <> BS.pack [fromIntegral $ ord '\n']

persistMetaEnvironment :: FilePath -> MetaEnvironment -> IO ()
persistMetaEnvironment path meta = do
    createDirectoryIfMissing True path
    forM_ (Map.toList meta) (\(name, env) -> do
        let modulpath = path </> unpack name
        writeEnvironment modulpath env)
