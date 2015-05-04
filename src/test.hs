{-# LANGUAGE OverloadedStrings #-}
module Main where

import Fragnix.ModuleDeclarations (
    parse,moduleDeclarationsWithEnvironment,moduleSymbols)
import Fragnix.Declaration (writeDeclarations)
import Fragnix.DeclarationSlices (declarationSlices)
import Fragnix.Slice (Slice(Slice),writeSliceDefault)
import Fragnix.Environment (
    loadEnvironment,builtinEnvironmentPath)
import Fragnix.SliceSymbols (updateEnvironment)
import Fragnix.SliceCompiler (sliceCompiler,sliceModuleDirectory)

import Test.Tasty (testGroup,TestTree)
import Test.Tasty.Golden (goldenVsFileDiff)
import Test.Tasty.Golden.Manage (defaultMain)

import Language.Haskell.Exts (prettyPrint)
import Language.Haskell.Names (symbolName)

import Control.Monad (forM_,forM)
import qualified Data.Map as Map (toList)
import System.Exit (ExitCode(ExitSuccess,ExitFailure))

import System.Directory (getDirectoryContents,removeDirectoryRecursive)
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
    writeDeclarations "fragnix/temp/declarations/declarations.json" declarations

    let (slices,symbolSlices) = declarationSlices declarations
    forM_ slices writeSliceDefault

    let environment = updateEnvironment symbolSlices (moduleSymbols builtinEnvironment modules)
        moduleSymbolResults = do
            (moduleName,symbols) <- Map.toList environment
            return (prettyPrint moduleName ++ " " ++ unwords (map (prettyPrint . symbolName) symbols))

    removeDirectoryRecursive sliceModuleDirectory

    let sliceIDs = [sliceID | Slice sliceID _ _ _ <- slices]
    exitCodes <- forM sliceIDs (\sliceID -> sliceCompiler sliceID)
    let successes = length [() | ExitSuccess   <- exitCodes]
        failures  = length [() | ExitFailure _ <- exitCodes]

    let result = unlines ([
            "Successes: " ++ show successes,
            "Failures: " ++ show failures] ++
            moduleSymbolResults)
    writeFile (folder </> "out") result
