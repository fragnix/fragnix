{-# LANGUAGE OverloadedStrings #-}
module Main where

import Fragnix.ModuleDeclarations (moduleDeclarations)
import Fragnix.Declaration (writeDeclarations)
import Fragnix.DeclarationSlices (declarationSlices)
import Fragnix.Slice (Slice(Slice),writeSlice)
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
    testfolders <- getDirectoryContents exampleFolder >>=
        return . filter (not . (=='.') . head)
    defaultMain (testGroup "Tests" (map testCase testfolders))

exampleFolder :: FilePath
exampleFolder = "tests/examples/"


testCase :: String -> TestTree
testCase testname = goldenVsFile
    testname
    (exampleFolder </> testname </> "golden")
    (exampleFolder </> testname </> "out")
    (testModules (exampleFolder </> testname))


testModules :: FilePath -> IO ()
testModules folder = do

    modulefilenames <- getDirectoryContents folder >>=
        return . filter (\filename -> takeExtension filename == ".hs")
    let modulepaths = map (\filename -> folder </> filename) modulefilenames

    declarations <- moduleDeclarations modulepaths
    writeDeclarations "fragnix/temp/declarations/declarations.json" declarations

    slices <- declarationSlices declarations
    forM_ slices writeSlice

    let sliceIDs = [sliceID | Slice sliceID _ _ _ <- slices]
    exitCodes <- forM sliceIDs (\sliceID -> sliceCompiler sliceID)
    let successes = length [() | ExitSuccess   <- exitCodes]
        failures  = length [() | ExitFailure _ <- exitCodes]

    let result = unlines [
            "Successes: " ++ show successes,
            "Failures: " ++ show failures]
    writeFile (folder </> "out") result
