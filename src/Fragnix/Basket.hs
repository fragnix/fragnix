{-# LANGUAGE NamedFieldPuns #-}
module Fragnix.Basket where

import Prelude hiding (readFile, writeFile)

import Fragnix.Core.Basket (Basket)
import Fragnix.Core.Loaf (Loaf (..), LoafID)
import Fragnix.Loaf (loavesToEnv, pickLoaf, readLoafFile)

import Control.Monad (filterM, forM)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (readFile, writeFile)
import Data.Functor ((<&>))
import qualified Data.Map as Map (fromList, toList)
import Data.Text (unpack)
import Language.Haskell.Exts.Syntax (ModuleName (..))
import Language.Haskell.Names (Environment)
import System.Directory
    (createDirectoryIfMissing, doesFileExist, getDirectoryContents)
import System.FilePath ((</>))

basketToEnvironment :: Basket -> IO Environment
basketToEnvironment b = basketToLoaves b <&> loavesToEnv

basketToLoaves :: Basket -> IO [Loaf]
basketToLoaves b = forM (Map.toList b) pickLoaf

basketToLoafIDs :: Basket -> [LoafID]
basketToLoafIDs b = fmap (head . snd) (Map.toList b)


{-readBasket :: FilePath -> IO Basket
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
  writeFile path (encodePretty b)-}
