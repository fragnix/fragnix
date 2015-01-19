{-# LANGUAGE OverloadedStrings #-}
module Main where

import Fragnix.ModuleDeclarations (parse,moduleDeclarationsWithEnvironment)
import Fragnix.Declaration (writeDeclarations)
import Fragnix.DeclarationSlices (declarationSlices)
import Fragnix.Slice (Slice(Slice),writeSlice)
import Fragnix.Environment (loadEnvironment,builtinEnvironmentPath)
import Fragnix.SliceCompiler (sliceCompiler)

import Test.Tasty (testGroup,TestTree)
import Test.Tasty.Golden (goldenVsFile)
import Test.Tasty.Golden.Manage (defaultMain)

import Control.Monad (forM_,forM)
import System.Exit (ExitCode(ExitSuccess,ExitFailure))

import System.Directory (getDirectoryContents)
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
testCase folder testname = goldenVsFile
    testname
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

    let (slices,_) = declarationSlices declarations
    forM_ slices writeSlice

    let sliceIDs = [sliceID | Slice sliceID _ _ _ <- slices]
    exitCodes <- forM sliceIDs (\sliceID -> sliceCompiler sliceID)
    let successes = length [() | ExitSuccess   <- exitCodes]
        failures  = length [() | ExitFailure _ <- exitCodes]

    let result = unlines [
            "Successes: " ++ show successes,
            "Failures: " ++ show failures]
    writeFile (folder </> "out") result
