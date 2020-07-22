module Build where

import Fragnix.Declaration (
    writeDeclarations)
import Fragnix.Slice (
    writeSlice)
import Fragnix.Environment (
    loadEnvironment,persistEnvironment)
import Fragnix.SliceSymbols (
    updateEnvironment,findMainSliceIDs)
import Fragnix.ModuleDeclarations (
    parse, moduleDeclarationsWithEnvironment,
    moduleSymbols)
import Fragnix.DeclarationLocalSlices (
    declarationLocalSlices)
import Fragnix.HashLocalSlices (
    hashLocalSlices)
import Fragnix.SliceSymbols (
    lookupLocalIDs)
import Fragnix.SliceCompiler (
    writeSliceModules, invokeGHCMain, invokeGHC)
import Fragnix.Utils (
    listFilesRecursive)
import Fragnix.Paths (
    slicesPath,builtinEnvironmentPath,environmentPath,declarationsPath,preprocessedPath)

-- import Language.Haskell.Names (ppError)

import System.Clock (
    getTime, Clock(Monotonic), toNanoSecs, diffTimeSpec)
import qualified Data.Map as Map (union,elems)

import Data.Foldable (for_)
import Control.Monad (forM)
import Data.List (intersperse)
import System.Directory (removeDirectoryRecursive, createDirectoryIfMissing)
import System.FilePath ((</>), takeExtension, splitDirectories, joinPath)
import System.Process (rawSystem)
import Text.Printf (printf)


-- | Take a list of module paths on the command line and compile the 'main' symbol
-- to an executable.
build :: ShouldPreprocess -> FilePath -> IO ()
build shouldPreprocess directory = do
    putStrLn "Loading environment ..."

    environment <- timeIt (do
        builtinEnvironment <- loadEnvironment builtinEnvironmentPath
        userEnvironment <- loadEnvironment environmentPath
        return (Map.union builtinEnvironment userEnvironment))

    modulePaths <- case shouldPreprocess of

      DoPreprocess -> do
        putStrLn "Preprocessing ..."
        timeIt (do
          createDirectoryIfMissing True preprocessedPath
          removeDirectoryRecursive preprocessedPath
          createDirectoryIfMissing True preprocessedPath
          filePaths <- listFilesRecursive directory
          let modulePaths = filter isHaskellFile filePaths
          forM modulePaths (\path -> do
            rawSystem "ghc-8.0.2" [
              "-E",
              "-optP","-P",
              "-optL","-P",
              "-Iinclude",
              "-o", modulePreprocessedPath path,
              path]
            return (modulePreprocessedPath path)))

      NoPreprocess -> do
        filePaths <- listFilesRecursive directory
        return (filter isHaskellFile filePaths)

    putStrLn "Parsing modules ..."

    modules <- timeIt (do
        forM modulePaths parse)

    putStrLn "Extracting declarations ..."

    let declarations = moduleDeclarationsWithEnvironment environment modules
    timeIt (writeDeclarations declarationsPath declarations)

--    let nameErrors = moduleNameErrors environment modules
--    forM_ nameErrors (\nameError -> putStrLn ("Warning: " ++ ppError nameError))

    putStrLn "Slicing ..."

    let (localSlices, symbolLocalIDs) = declarationLocalSlices declarations
    let (localSliceIDMap, slices) = hashLocalSlices localSlices
    timeIt (for_ slices (\slice -> writeSlice slicesPath slice))

    putStrLn "Updating environment ..."

    let symbolSliceIDs = lookupLocalIDs symbolLocalIDs localSliceIDMap
    let updatedEnvironment = updateEnvironment symbolSliceIDs (moduleSymbols environment modules)
    timeIt (persistEnvironment environmentPath updatedEnvironment)

    case findMainSliceIDs symbolSliceIDs of
        [] -> do
            putStrLn "No main symbol in modules."
            putStrLn "Compiling all slices."
            let sliceIDs = Map.elems symbolSliceIDs
            putStrLn "Generating compilation units..."
            timeIt (for_ sliceIDs writeSliceModules)
            putStrLn "Invoking GHC"
            _ <- timeIt (for_ sliceIDs invokeGHC)
            return ()
        [mainSliceID] -> do
            putStrLn ("Compiling " ++ show mainSliceID)
            putStrLn "Generating compilation units..."
            timeIt (writeSliceModules mainSliceID)
            putStrLn "Invoking GHC"
            _ <- timeIt (invokeGHCMain mainSliceID)
            return ()
        _ -> putStrLn "Multiple main symbols in modules."

    return ()

data ShouldPreprocess
  = DoPreprocess
  | NoPreprocess

-- | Replace slashes by dots.
modulePreprocessedPath :: FilePath -> FilePath
modulePreprocessedPath path = joinPath [
  preprocessedPath,
  concat (intersperse "." (splitDirectories path))]

isHaskellFile :: FilePath -> Bool
isHaskellFile path = takeExtension path == ".hs"

-- | Execute the given action and print the time it took.
timeIt :: IO a -> IO a
timeIt action = do
    timeBefore <- getTime Monotonic
    result <- action
    timeAfter <- getTime Monotonic
    let timeDifference = fromIntegral (toNanoSecs (diffTimeSpec timeBefore timeAfter)) * 1e-9 :: Double
    printf "Took %6.2fs\n" timeDifference
    return result
