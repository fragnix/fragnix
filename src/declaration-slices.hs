module Main where

import Fragnix.Slice (writeSlice)
import Fragnix.Declaration (readDeclarations)
import Fragnix.DeclarationSlices (declarationSlices)

import Control.Monad (forM_)

main :: IO ()
main = do
    _ <- readDeclarations "fragnix/declarations/declarations.json"
    let (slices,_) = declarationSlices undefined
    forM_ slices writeSlice
