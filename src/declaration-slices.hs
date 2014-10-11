module Main where

import Fragnix.Declaration (readDeclarations)
import Fragnix.DeclarationSlices (declarationSlices)
import Fragnix.Nest (writeSlice)

import Control.Monad (forM_)

main :: IO ()
main = do
    declarations <- readDeclarations "fragnix/declarations/declarations.json"
    let slices = declarationSlices declarations
    forM_ slices writeSlice
