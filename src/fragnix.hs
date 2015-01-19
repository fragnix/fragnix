module Main where

import Fragnix.Declaration (writeDeclarations)
import Fragnix.Slice (writeSlice)
import Fragnix.ModuleDeclarations (moduleDeclarations)
import Fragnix.DeclarationSlices (declarationSlices)
import Fragnix.SliceCompiler (sliceCompilerMain)

import Control.Monad (forM_)
import System.Environment (getArgs)

-- | Take a list of module paths on the command line and compile the 'main' symbol
-- to an executable.
main :: IO ()
main = do

    args <- getArgs

    declarations <- moduleDeclarations args
    writeDeclarations "fragnix/temp/declarations/declarations.json" declarations

    let (slices,_) = declarationSlices declarations
    forM_ slices writeSlice

    putStrLn "Finding main slice not implemented!"
    let mainSliceID = undefined

    sliceCompilerMain mainSliceID

    return ()
