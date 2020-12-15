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
    parseWithComments, moduleDeclarationsWithEnvironment,
    moduleSymbols, moduleComments)
import Fragnix.DeclarationLocalSlices (
    declarationLocalSlices)
import Fragnix.HashLocalSlices (
    hashLocalSlices)
import Fragnix.SliceSymbols (
    lookupLocalIDs)
import Fragnix.SliceCompiler (
    writeSlicesModules, invokeGHCMain, invokeGHC)
import Fragnix.Utils (
    listFilesRecursive)
import Fragnix.Paths (
    slicesPath,builtinEnvironmentPath,environmentPath,declarationsPath,preprocessedPath)

-- import Language.Haskell.Names (ppError)

import System.Clock (
    getTime, Clock(Monotonic), toNanoSecs, diffTimeSpec)
import qualified Data.Map as Map (union,elems,lookup)
import Data.Text (unpack)

import Data.Foldable (for_)
import Control.Monad (forM)
import Data.List (intersperse,nub)
import Data.Maybe (mapMaybe,maybeToList,listToMaybe)
import System.Directory (removeDirectoryRecursive, createDirectoryIfMissing)
import System.FilePath ((</>), takeExtension, splitDirectories, joinPath)
import System.Process (rawSystem)
import Text.Printf (printf)


-- | Take a list of module paths on the command line and compile the 'main' symbol
-- to an executable.
build :: ShouldDist -> ShouldPreprocess -> ShouldExtractComments -> [FilePath] -> IO ()
build shouldDist shouldPreprocess shouldExtractComments directories = do
    putStrLn "Finding targets ..."

    filePaths <- timeIt (do
        concat <$> forM directories listFilesRecursive)

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
        return (filter isHaskellFile filePaths)

    putStrLn "Parsing modules ..."

    modulesWithComments <- timeIt (do
        forM modulePaths parseWithComments)

    putStrLn "Extracting declarations ..."

    let modules = map fst modulesWithComments
    let declarations = moduleDeclarationsWithEnvironment environment modules
    timeIt (writeDeclarations declarationsPath declarations)

--    let nameErrors = moduleNameErrors environment modules
--    forM_ nameErrors (\nameError -> putStrLn ("Warning: " ++ ppError nameError))

    putStrLn "Slicing ..."

    let (localSlices, symbolLocalIDs) = declarationLocalSlices declarations
    let (localSliceIDMap, slices) = hashLocalSlices localSlices
    timeIt (for_ slices (\slice -> writeSlice slicesPath slice))

    let symbolSliceIDs = lookupLocalIDs symbolLocalIDs localSliceIDMap
    let updatedEnvironment = updateEnvironment symbolSliceIDs (moduleSymbols environment modules)

    case shouldExtractComments of
      NoExtractComments -> do
        return ()
      DoExtractComments -> do
        for_ modulesWithComments (\(modul, comments) -> do
          let symbolsComments = moduleComments modul comments
          let sliceIDComments = do
                (symbols, comment) <- symbolsComments
                -- TODO what if a slice id maps to multiple lists of comments?
                let sliceIDs = mapMaybe (\symbol -> Map.lookup symbol symbolSliceIDs) symbols
                -- TODO make sure slice IDs are all the same
                sliceID <- maybeToList (listToMaybe sliceIDs)
                return (sliceID, comment)
          -- TODO define this in module Paths
          createDirectoryIfMissing True "comments"
          for_ sliceIDComments (\(sliceID, comment) ->
            writeFile ("comments/" ++ unpack sliceID) comment))

    case shouldDist of

      ShouldCompile -> do

        putStrLn "Updating environment ..."

        timeIt (persistEnvironment environmentPath updatedEnvironment)

        case findMainSliceIDs symbolSliceIDs of
            [] -> do
                putStrLn "No main symbol in modules."
            [mainSliceID] -> do
                putStrLn ("Compiling " ++ show mainSliceID)
                putStrLn "Generating compilation units..."
                timeIt (writeSlicesModules [mainSliceID])
                putStrLn "Invoking GHC"
                _ <- timeIt (invokeGHCMain mainSliceID)
                return ()
            _ -> putStrLn "Multiple main symbols in modules."

      ShouldDist outputDirectory -> do

        putStrLn "Writing environment ..."

        createDirectoryIfMissing True outputDirectory
        timeIt (persistEnvironment outputDirectory updatedEnvironment)

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

data ShouldExtractComments
  = DoExtractComments
  | NoExtractComments


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
