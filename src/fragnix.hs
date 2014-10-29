module Main where

import Fragnix.Declaration (writeDeclarations)
import Fragnix.Slice (writeSlice,Slice(Slice))
import Fragnix.Symbol (mainsymbol)
import Fragnix.ModuleDeclarations (modulDeclarations)
import Fragnix.DeclarationSlices (declarationSlices)
import Fragnix.SliceCompiler (sliceCompilerMain,sliceCompiler)

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
