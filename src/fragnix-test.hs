module Main where

import Fragnix.Declaration (writeDeclarations)
import Fragnix.Slice (writeSlice,Slice(Slice))
import Fragnix.ModuleDeclarations (modulDeclarations)
import Fragnix.DeclarationSlices (declarationSlices)
import Fragnix.SliceCompiler (sliceCompiler)

import Control.Monad (forM_,forM)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitSuccess,ExitFailure))


-- | Take a list of module paths on the command line and try to compile every single
-- slice found in them.
main :: IO ()
main = do

    args <- getArgs

    declarations <- modulDeclarations args
    writeDeclarations "fragnix/declarations/declarations.json" declarations

    slices <- declarationSlices declarations
    forM_ slices writeSlice

    let sliceIDs = [sliceID | Slice sliceID _ _ _ <- slices]
    exitCodes <- forM sliceIDs (\sliceID -> sliceCompiler sliceID)
    let successes = length [() | ExitSuccess <- exitCodes]
        failures = length [() | ExitFailure _ <- exitCodes]
    putStrLn ("Successes: " ++ show successes)
    putStrLn ("Failures: " ++ show failures)

