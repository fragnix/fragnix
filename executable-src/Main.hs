module Main where

import Fragnix.Declaration (writeDeclarations)
import Fragnix.Slice (writeSliceDefault)
import Fragnix.Environment (
    loadEnvironment,persistEnvironment,
    environmentPath,builtinEnvironmentPath)
import Fragnix.SliceSymbols (
    updateEnvironment,findMainSliceIDs)
import Fragnix.ModuleDeclarations (
    parse,moduleDeclarationsWithEnvironment,
    moduleNameErrors,moduleSymbols)
import Fragnix.DeclarationSlices (declarationSlices)
import Fragnix.SliceCompiler (sliceCompilerMain)

import Language.Haskell.Names (ppError)

import Control.Monad (forM_,forM)
import qualified Data.Map as Map (union)
import System.Environment (getArgs)

-- | Take a list of module paths on the command line and compile the 'main' symbol
-- to an executable.
main :: IO ()
main = do

    modulePaths <- getArgs

    putStrLn "Loading environment ..."

    builtinEnvironment <- loadEnvironment builtinEnvironmentPath
    userEnvironment <- loadEnvironment environmentPath
    let environment = Map.union builtinEnvironment userEnvironment

    putStrLn "Parsing modules ..."

    modules <- forM modulePaths parse

    putStrLn "Extracting declarations ..."

    let declarations = moduleDeclarationsWithEnvironment environment modules
    writeDeclarations "fragnix/temp/declarations/declarations.json" declarations

--    let nameErrors = moduleNameErrors environment modules
--    forM_ nameErrors (\nameError -> putStrLn ("Warning: " ++ ppError nameError))

    putStrLn "Slicing ..."

    let (slices,symbolSlices) = declarationSlices declarations
    forM_ slices writeSliceDefault

    putStrLn "Updating environment ..."

    let updatedEnvironment = updateEnvironment symbolSlices (moduleSymbols environment modules)
    persistEnvironment environmentPath updatedEnvironment

    case findMainSliceIDs symbolSlices of
        [] -> putStrLn "No main symbol in modules."
        [mainSliceID] -> do
            putStrLn ("Compiling " ++ show mainSliceID)
            sliceCompilerMain mainSliceID
            return ()
        _ -> putStrLn "Multiple main symbols in modules."

    return ()
