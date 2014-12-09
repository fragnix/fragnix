module Main where

import Fragnix.Declaration (writeDeclarations)
import Fragnix.Slice (writeSlice,Reference(OtherSlice))
import Fragnix.ModuleDeclarations (modulDeclarations)
import Fragnix.DeclarationSlices (declarationSlices)
import Fragnix.SliceCompiler (sliceCompilerMain)
import Fragnix.Environment (loadEnvironment,environmentPath,mainsymbol)

import qualified Data.Map as Map (lookup)
import Control.Monad (forM_)
import System.Environment (getArgs)

-- | Take a list of module paths on the command line and compile the 'main' symbol
-- to an executable.
main :: IO ()
main = do

    args <- getArgs

    declarations <- modulDeclarations args
    writeDeclarations "fragnix/temp/declarations/declarations.json" declarations

    slices <- declarationSlices declarations
    forM_ slices writeSlice

    environment <- loadEnvironment environmentPath

    case Map.lookup mainsymbol environment of
        Just (OtherSlice mainSliceID) -> sliceCompilerMain mainSliceID >>= print
        _ -> putStrLn "No main slice!"

