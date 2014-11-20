module Main where

import Fragnix.Declaration (writeDeclarations)
import Fragnix.Slice (writeSlice,Slice(Slice))
import Fragnix.ModuleDeclarations (modulDeclarations)
import Fragnix.DeclarationSlices (declarationSlices)
import Fragnix.SliceCompiler (sliceCompilerMain,sliceCompiler)
import Fragnix.Symbols (mainsymbol)

import qualified Data.Map as Map (lookup)
import Control.Monad (forM_,forM)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitSuccess,ExitFailure))

fragnixExecutable :: IO ()
fragnixExecutable = do
    args <- getArgs
    declarations <- modulDeclarations args
    writeDeclarations "fragnix/declarations/declarations.json" declarations
    let (slices,globalscope) = declarationSlices declarations
    forM_ slices writeSlice
    case Map.lookup mainsymbol globalscope of
        Nothing -> putStrLn "No main slice!"
        Just mainSliceID -> sliceCompilerMain mainSliceID >>= print

main :: IO ()
main = fragnixExecutable
