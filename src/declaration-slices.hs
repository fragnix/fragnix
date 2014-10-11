module Main where

import Fragnix.Nest (writeSlice)

import System.Directory (getDirectoryContents)

import Control.Monad (forM,forM_)

main :: IO ()
main = do
    declarationpaths <- getDirectoryContents "fragnix/declarations"
    declarations <- forM declarationpaths readDeclarations
    slices <- declarationSlices (concat declarations)
    forM_ slices writeSlice

readDeclarations = undefined

declarationSlices = undefined
