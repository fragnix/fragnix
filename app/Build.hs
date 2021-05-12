{-# LANGUAGE NamedFieldPuns #-}
module Build where

import Fragnix.Config (readConfig)
import Fragnix.Core.Config (Config (..))
import Fragnix.Declaration (writeDeclarations)
import Fragnix.DeclarationLocalSlices (declarationLocalSlices)
import qualified Fragnix.Environment as E (loadEnvironment)
import Fragnix.ForeignSlice
    (fileToForeignSlice, writeForeignFiles, writeForeignSlice)
import Fragnix.HashLocalSlices (hashLocalSlices)
import Fragnix.Loaf (loavesToEnv, persistEnvironment, readAllLoaves)
import Fragnix.ModuleDeclarations
    (moduleDeclarationsWithEnvironment, moduleSymbols, parse)
import Fragnix.Paths
    (builtinEnvironmentPath, configPath, declarationsPath, environmentPath,
    foreignCodeExtensions, foreignSlicesPath, preprocessedPath, slicesPath)
import Fragnix.Slice (writeSlice)
import Fragnix.SliceCompiler (invokeGHC, invokeGHCMain, writeSlicesModules)
import Fragnix.SliceSymbols
    (findMainSliceIDs, lookupLocalIDs, updateEnvironment)
import Fragnix.Utils (listFilesRecursive)

-- import Language.Haskell.Names (ppError)

import qualified Data.Map as Map (elems, union)
import System.Clock (Clock (Monotonic), diffTimeSpec, getTime, toNanoSecs)

import Control.Monad (forM, forM_)
import Data.Foldable (for_)
import Data.List (intersperse, nub)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath (joinPath, splitDirectories, takeExtension)
import System.Process (rawSystem)
import Text.Printf (printf)


-- | Take a list of module paths on the command line and compile the 'main' symbol
-- to an executable.
build :: ShouldDist -> ShouldPreprocess -> [FilePath] -> IO ()
build shouldDist shouldPreprocess directories = do
    putStrLn "Reading fragnix.yaml ..."

    Config { cbitsFolder } <- timeIt (readConfig configPath)

    putStrLn "Finding targets ..."

    (filePaths, foreignFilePaths) <- timeIt (do
        filePaths <- concat <$> forM directories listFilesRecursive
        allForeignFilePaths <- listFilesRecursive cbitsFolder
        let foreignFilePaths = filter isForeignCode allForeignFilePaths
        return (filePaths, foreignFilePaths))

    putStrLn "Loading environment ..."

    environment <- timeIt (do
        builtinEnvironment <- E.loadEnvironment builtinEnvironmentPath
        loaves <- readAllLoaves environmentPath
        let userEnvironment = loavesToEnv loaves
        return (Map.union builtinEnvironment userEnvironment))

    modulePaths <- case shouldPreprocess of

      DoPreprocess -> do
        putStrLn "Preprocessing ..."
        timeIt (do
          createDirectoryIfMissing True preprocessedPath
          removeDirectoryRecursive preprocessedPath
          createDirectoryIfMissing True preprocessedPath
          let modulePaths = filter isHaskellFile filePaths
          forM modulePaths (\path -> do
            _ <- rawSystem "ghc" [
              "-E",
              "-optP","-P",
              "-optL","-P",
              "-Iinclude",
              "-o", modulePreprocessedPath path,
              path]
            return (modulePreprocessedPath path)))

      NoPreprocess -> do
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

    foreignSlices <- forM foreignFilePaths fileToForeignSlice

    let (localSlices, symbolLocalIDs) = declarationLocalSlices declarations foreignSlices
    let (localSliceIDMap, slices) = hashLocalSlices localSlices
    timeIt (for_ slices (\slice -> writeSlice slicesPath slice))

    case shouldDist of

      ShouldCompile -> do

        putStrLn "Updating environment ..."

        let symbolSliceIDs = lookupLocalIDs symbolLocalIDs localSliceIDMap
        let updatedEnvironment = updateEnvironment symbolSliceIDs (moduleSymbols environment modules)
        timeIt (persistEnvironment environmentPath updatedEnvironment)

        forM_ foreignSlices $ writeForeignSlice foreignSlicesPath

        case findMainSliceIDs symbolSliceIDs of
            [] -> do
                putStrLn "No main symbol in modules."
            [mainSliceID] -> do
                putStrLn ("Compiling " ++ show mainSliceID)
                putStrLn "Generating compilation units..."
                timeIt (writeSlicesModules [mainSliceID])
                timeIt (writeForeignFiles [mainSliceID])
                putStrLn "Invoking GHC"
                _ <- timeIt (invokeGHCMain mainSliceID)
                return ()
            _ -> putStrLn "Multiple main symbols in modules."

      ShouldDist outputDirectory -> do

        putStrLn "Writing environment ..."

        let symbolSliceIDs = lookupLocalIDs symbolLocalIDs localSliceIDMap
        let updatedEnvironment = updateEnvironment symbolSliceIDs (moduleSymbols environment modules)
        createDirectoryIfMissing True outputDirectory
        timeIt (persistEnvironment outputDirectory updatedEnvironment)

        forM_ foreignSlices $ writeForeignSlice foreignSlicesPath

        putStrLn "Generating compilation units..."

        let sliceIDs = Map.elems symbolSliceIDs
        timeIt (writeSlicesModules sliceIDs)

        putStrLn "Invoking GHC"

        _ <- timeIt (invokeGHC (nub sliceIDs))
        return ()


data ShouldPreprocess
  = DoPreprocess
  | NoPreprocess

data ShouldDist
  = ShouldCompile
  | ShouldDist FilePath

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

isForeignCode :: FilePath -> Bool
isForeignCode filepath = takeExtension filepath `elem` foreignCodeExtensions
