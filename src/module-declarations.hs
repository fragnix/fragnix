module Main where

import Fragnix.ModuleDeclarations (modulDeclarations)
import Fragnix.Declaration (writeDeclarations)

import System.Directory (
    getDirectoryContents,createDirectoryIfMissing,doesFileExist)

import qualified Data.Map as Map (keys)
import Control.Monad (filterM)

main :: IO ()
main = do
    modulepaths <- getDirectoryContents "fragnix/modules" >>= filterM doesFileExist
    declarationmap <- modulDeclarations modulepaths
    createDirectoryIfMissing True "fragnix/declarations"
    writeDeclarations "fragnix/declarations/declarations.json" (Map.keys declarationmap)

