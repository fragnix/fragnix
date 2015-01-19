module Main where

import Fragnix.Declaration (writeDeclarations)
import Fragnix.Slice (writeSlice)
import Fragnix.Environment (
    loadEnvironment,updateEnvironment,persistEnvironment,
    environmentPath,builtinEnvironmentPath,
    findMainSliceIDs)
import Fragnix.ModuleDeclarations (
    parse,moduleDeclarationsWithEnvironment,moduleSymbols)
import Fragnix.DeclarationSlices (declarationSlices)
import Fragnix.SliceCompiler (sliceCompilerMain)

import Control.Monad (forM_,forM)
import qualified Data.Map as Map (union)
import System.Environment (getArgs)

-- | Take a list of module paths on the command line and compile the 'main' symbol
-- to an executable.
main :: IO ()
main = do

    modulePaths <- getArgs

    builtinEnvironment <- loadEnvironment builtinEnvironmentPath
    userEnvironment <- loadEnvironment environmentPath
    let environment = Map.union builtinEnvironment userEnvironment
    modules <- forM modulePaths parse

    let declarations = moduleDeclarationsWithEnvironment environment modules
    writeDeclarations "fragnix/temp/declarations/declarations.json" declarations

    let (slices,symbolSlices) = declarationSlices declarations
    forM_ slices writeSlice

    let updatedEnvironment = updateEnvironment symbolSlices (moduleSymbols environment modules)
    persistEnvironment environmentPath updatedEnvironment

    case findMainSliceIDs symbolSlices of
        [] -> putStrLn "No main symbol in modules."
        [mainSliceID] -> do
            sliceCompilerMain mainSliceID
            return ()
        _ -> putStrLn "Multiple main symbols in modules."

    return ()
