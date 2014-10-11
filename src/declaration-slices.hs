module Main where

import Fragnix.Declaration (readDeclarations)
import Fragnix.Nest (writeSlice)

import System.Directory (getDirectoryContents)

import Control.Monad (forM,forM_)

main :: IO ()
main = do
    declarations <- readDeclarations "fragnix/declarations/declarations.json"
    slices <- declarationSlices declarations
    forM_ slices writeSlice

declarationSlices = undefined
