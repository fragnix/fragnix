module Main where

import Fragnix.Declaration (writeDeclarations)
import Fragnix.Slice (writeSlice)
import Fragnix.ModuleDeclarations (modulDeclarations)
import Fragnix.DeclarationSlices (declarationSlices)
import Fragnix.SliceCompiler (sliceCompiler)

import Control.Monad (forM_)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    declarations <- modulDeclarations args
    writeDeclarations "fragnix/declarations/declarations.json" declarations
    let slices = declarationSlices declarations
    forM_ slices writeSlice
    sliceCompiler 7942675488051212534 >>= print
