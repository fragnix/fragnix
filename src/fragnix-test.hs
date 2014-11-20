module Main where

import Fragnix.Declaration (writeDeclarations)
import Fragnix.Slice (writeSlice,Slice(Slice))
import Fragnix.ModuleDeclarations (modulDeclarations)
import Fragnix.DeclarationSlices (declarationSlices)
import Fragnix.SliceCompiler (sliceCompiler)

import Control.Monad (forM_,forM)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitSuccess,ExitFailure))

fragnixTest :: IO ()
fragnixTest = do
    args <- getArgs
    declarations <- modulDeclarations args
    writeDeclarations "fragnix/declarations/declarations.json" declarations
    let (slices,_) = declarationSlices declarations
    forM_ slices writeSlice
    let sliceIDs = [sliceID | Slice sliceID _ _ _ <- slices]
    exitCodes <- forM sliceIDs (\sliceID -> sliceCompiler sliceID)
    let successes = length [() | ExitSuccess <- exitCodes]
        failures = length [() | ExitFailure _ <- exitCodes]
    putStrLn ("Successes: " ++ show successes)
    putStrLn ("Failures: " ++ show failures)

main :: IO ()
main = fragnixTest
