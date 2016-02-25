{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
module Main where

import Fragnix.Environment (
    loadEnvironment, builtinEnvironmentPath)
import Fragnix.ModuleDeclarations (
    parse, moduleDeclarationsWithEnvironment)
import Fragnix.DeclarationSlices (
    declarationSlices)
import Fragnix.Declaration (
    Declaration, Genre)
import Fragnix.Slice (
    Slice, Language, Fragment, Use, Reference, UsedName, Name,
    Instance, InstancePart)

import Criterion.Main (defaultMain, bench, nfIO, nf, whnfIO)

import Language.Haskell.Names (Symbol(..))

import Language.Haskell.Exts (ModuleName, Extension(..), KnownExtension(..))
import qualified Language.Haskell.Exts as HaskellExts (Name)

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
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
        bench "parse" (whnfIO (forM modulePaths parse)),
        bench "moduleDeclarations" (nf (moduleDeclarationsWithEnvironment builtinEnvironment) modules),
        bench "declarationSlices" (nf (fst . declarationSlices) declarations)]


instance NFData ModuleName
instance NFData Symbol
instance NFData Declaration
instance NFData Genre
instance NFData HaskellExts.Name
instance NFData Extension
instance NFData KnownExtension
instance NFData Slice
instance NFData Language
instance NFData Fragment
instance NFData Use
instance NFData Reference
instance NFData UsedName
instance NFData Name
instance NFData Instance
instance NFData InstancePart

deriving instance Generic KnownExtension
deriving instance Generic Extension
deriving instance Generic Symbol
