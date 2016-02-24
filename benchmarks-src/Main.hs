module Main where

import Fragnix.Environment (
    loadEnvironment, builtinEnvironmentPath)
import Fragnix.ModuleDeclarations (
    parse, moduleDeclarationsWithEnvironment)
import Fragnix.DeclarationSlices (
    declarationSlices)
import Fragnix.Declaration (
    Declaration)
import Fragnix.Slice (
    Slice)

import Criterion.Main (defaultMain, bench, nfIO, nf)

import Language.Haskell.Names (Symbol)

import Language.Haskell.Exts (ModuleName)
import Language.Haskell.Exts.Annotated (Module, SrcSpan)

import Control.DeepSeq (NFData)
import Control.Monad (forM)
import System.Directory (getDirectoryContents)


main :: IO ()
main = do

    let path = "benchmarks/containers/"
    directoryContents <- getDirectoryContents path
    let modulePaths = map (path ++) (filter (not . (=='.') . head) directoryContents)

    builtinEnvironment <- loadEnvironment builtinEnvironmentPath

    modules <- forM modulePaths parse

    let declarations = moduleDeclarationsWithEnvironment builtinEnvironment modules

    defaultMain [
        bench "loadEnvironment" (nfIO (loadEnvironment builtinEnvironmentPath)),
        bench "parse" (nfIO (forM modulePaths parse)),
        bench "moduleDeclarations" (nf (moduleDeclarationsWithEnvironment builtinEnvironment) modules),
        bench "declarationSlices" (nf (fst . declarationSlices) declarations)]

instance NFData ModuleName
instance NFData Symbol
instance (NFData a) => NFData (Module a)
instance NFData SrcSpan
instance NFData Declaration
instance NFData Slice
