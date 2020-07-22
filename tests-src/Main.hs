{-# LANGUAGE OverloadedStrings #-}
module Main where

import Fragnix.ModuleDeclarations (
    parse,moduleDeclarationsWithEnvironment,moduleSymbols)
import Fragnix.Declaration (writeDeclarations)
import Fragnix.DeclarationLocalSlices (declarationLocalSlices)
import Fragnix.HashLocalSlices (hashLocalSlices,replaceSliceID)
import Fragnix.Slice (Slice(Slice),writeSlice)
import Fragnix.LocalSlice (LocalSliceID(LocalSliceID))
import Fragnix.Environment (loadEnvironment)
import Fragnix.SliceSymbols (updateEnvironment,lookupLocalIDs)
import Fragnix.SliceCompiler (writeSliceModules,invokeGHC)
import Fragnix.Paths (slicesPath,builtinEnvironmentPath,compilationunitsPath,declarationsPath)

import Test.Tasty (testGroup,TestTree)
import Test.Tasty.Golden (goldenVsFileDiff)
import Test.Tasty.Golden.Manage (defaultMain)

import Language.Haskell.Exts (prettyPrint)
import Language.Haskell.Names (symbolName)
import Data.Text as Text (append)

import Control.Monad (forM_,forM,when)
import qualified Data.Map as Map (toList)
import Data.Foldable (toList)
import System.Exit (ExitCode(ExitSuccess,ExitFailure))

import System.Directory (getDirectoryContents,removeDirectoryRecursive,doesDirectoryExist)
import System.FilePath ((</>),takeExtension)


main :: IO ()
main = do
    packagenames <- getDirectoryContents examplePackageFolder >>=
        return . filter (not . (=='.') . head)
    quicknames <- getDirectoryContents exampleQuickFolder >>=
        return . filter (not . (=='.') . head)
    defaultMain (testGroup "tests" [
        testGroup "packages" (map (testCase examplePackageFolder) packagenames),
        testGroup "quick" (map (testCase exampleQuickFolder) quicknames)])

examplePackageFolder :: FilePath
examplePackageFolder = "tests/packages"

exampleQuickFolder :: FilePath
exampleQuickFolder = "tests/quick"

testCase :: FilePath -> String -> TestTree
testCase folder testname = goldenVsFileDiff
    testname
    (\ref new -> ["diff", "-u", ref, new])
    (folder </> testname </> "golden")
    (folder </> testname </> "out")
    (testModules (folder </> testname))


testModules :: FilePath -> IO ()
testModules folder = do

    modulefilenames <- getDirectoryContents folder >>=
        return . filter (\filename -> takeExtension filename == ".hs")
    let modulepaths = map (\filename -> folder </> filename) modulefilenames

    builtinEnvironment <- loadEnvironment builtinEnvironmentPath
    modules <- forM modulepaths parse

    let declarations = moduleDeclarationsWithEnvironment builtinEnvironment modules
    writeDeclarations declarationsPath declarations

    let (localSlices, symbolLocalIDs) = declarationLocalSlices declarations
    let (localSliceIDMap, slices) = hashLocalSlices localSlices
    let symbolSliceIDs = lookupLocalIDs symbolLocalIDs localSliceIDMap
    forM_ slices (\slice -> writeSlice slicesPath slice)

    let environment = updateEnvironment symbolSliceIDs (moduleSymbols builtinEnvironment modules)
        moduleSymbolResults = do
            (moduleName,symbols) <- Map.toList environment
            return (prettyPrint moduleName ++ " " ++ unwords (map (prettyPrint . symbolName) symbols))

    sliceModuleDirectoryExists <- doesDirectoryExist compilationunitsPath
    when sliceModuleDirectoryExists (removeDirectoryRecursive compilationunitsPath)

    let sliceIDs = [sliceID | Slice sliceID _ _ _ _ <- toList slices]
    exitCodes <- forM sliceIDs (\sliceID -> do
        writeSliceModules sliceID
        invokeGHC sliceID)
    let successes = length [() | ExitSuccess   <- exitCodes]
        failures  = length [() | ExitFailure _ <- exitCodes]

    let localSlices = map (replaceSliceID (\sliceID ->
            LocalSliceID (Text.append "T" sliceID))) slices

    let (_, rehashedLocalSlices) = hashLocalSlices localSlices

    let rehashedSlicesEqual = rehashedLocalSlices == slices

    let result = unlines ([
            "Successes: " ++ show successes,
            "Failures: " ++ show failures,
            "Rehashing: " ++ show rehashedSlicesEqual] ++
            moduleSymbolResults)
    writeFile (folder </> "out") result


