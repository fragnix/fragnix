module Main where

import Fragnix.Declaration (writeDeclarations)
import Fragnix.Slice (writeSlice)
import Fragnix.Symbol (mainsymbol)
import Fragnix.ModuleDeclarations (modulDeclarations)
import Fragnix.DeclarationSlices (declarationSlices)
import Fragnix.SliceCompiler (sliceCompiler)

import qualified Data.Map as Map (lookup)
import Control.Monad (forM_)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    declarations <- modulDeclarations args
    writeDeclarations "fragnix/declarations/declarations.json" declarations
    let (slices,globalscope) = declarationSlices declarations
    forM_ slices writeSlice
    case Map.lookup mainsymbol globalscope of
        Nothing -> putStrLn "No main slice!"
        Just mainSliceID -> sliceCompiler mainSliceID >>= print
