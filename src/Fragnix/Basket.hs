{-# LANGUAGE NamedFieldPuns #-}
module Fragnix.Basket where

import Prelude hiding (readFile, writeFile)

import Fragnix.Core.Basket (Basket)
import Fragnix.Core.Loaf (Loaf (..))
import Fragnix.Loaf (loafToSymbols, pickLoaf, readLoafFile)

import Control.Monad (filterM, forM)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (readFile, writeFile)
import qualified Data.Map as Map (fromList, toList)
import Data.Text (unpack)
import Language.Haskell.Exts.Syntax (ModuleName (..))
import Language.Haskell.Names (Environment)
import System.Directory
    (createDirectoryIfMissing, doesFileExist, getDirectoryContents)
import System.FilePath ((</>))


basketToEnvironment :: Basket -> IO Environment
basketToEnvironment b = do
  envList <- forM (Map.toList b) (\(name, loaves) -> do
  loaf <- pickLoaf loaves
  symbols <- loafToSymbols loaf
  return (ModuleName () (unpack name), symbols))
  return $ Map.fromList envList

readBasket :: FilePath -> IO Basket
readBasket path = do
  createDirectoryIfMissing True path
  filenames <- getDirectoryContents path
  let pathmodulnames = map (path </>) filenames
  existingPathModulNames <- filterM doesFileExist pathmodulnames
  fmap Map.fromList (forM existingPathModulNames (\file -> do
    Loaf {name, loafID}<- readLoafFile file
    return (name,[loafID])))

writeBasket :: FilePath -> Basket -> IO ()
writeBasket path b = do
  createDirectoryIfMissing True path
  writeFile path (encodePretty b)
