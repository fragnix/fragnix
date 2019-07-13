{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
module Main where

import Fragnix.Environment (
    loadEnvironment, builtinEnvironmentPath)
import Fragnix.ModuleDeclarations (
    parse, moduleDeclarationsWithEnvironment)
import Fragnix.DeclarationLocalSlices (
    declarationLocalSlices, fragmentLocalSlices,
    declarationSCCs)
import Fragnix.HashLocalSlices (
    hashLocalSlices, hashSlices2, sliceMap)
import Fragnix.SliceSymbols (
    findMainSliceIDs)
import Fragnix.SliceCompiler (
    writeSliceModules, sliceInstances, invokeGHCMain)
import Fragnix.Declaration (
    Declaration, Genre)
import Fragnix.Slice (
    Slice, Language, Fragment, Use, Reference, UsedName, Name,
    Instance, InstancePart,
    writeSliceDefault)
import Fragnix.LocalSlice (
    LocalSlice, LocalSliceID, LocalUse, LocalReference, LocalInstance)
import Fragnix.SliceSymbols (
    lookupLocalIDs)


import Criterion.Main (defaultMain, bgroup, bench, nfIO, nf, whnfIO, env)

import Language.Haskell.Names (
    Symbol(..), Scoped(..), NameInfo(..), Error(..),
    resolve, annotate)

import Language.Haskell.Exts (
    ModuleName, Extension(..), KnownExtension(..),
    QName, ModuleName, SpecialCon, Boxed)
import Language.Haskell.Exts.SrcLoc (
    SrcSpan(..), SrcLoc(..))
import qualified Language.Haskell.Exts as HaskellExts (
    Name)

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Control.Monad (forM)
import Data.Foldable (for_, toList)
import System.Directory (getDirectoryContents, removeDirectoryRecursive)


main :: IO ()
main = do

    let path = "benchmarks/containers/"
    directoryContents <- getDirectoryContents path
    let modulePaths = map (path ++) (filter (not . (=='.') . head) directoryContents)

    builtinEnvironment <- loadEnvironment builtinEnvironmentPath

    modules <- forM modulePaths parse

    let environment = resolve modules builtinEnvironment

    let declarations = moduleDeclarationsWithEnvironment builtinEnvironment modules

    let fragmentNodes = declarationSCCs declarations

    let tempSliceMap = sliceMap (fragmentLocalSlices fragmentNodes)

    let (localSlices, symbolLocalIDs) = declarationLocalSlices declarations

    let slices = hashLocalSlices localSlices

    let symbolSliceIDs = lookupLocalIDs symbolLocalIDs slices

    let mainSliceID = head (findMainSliceIDs symbolSliceIDs)

    defaultMain [
        bench "loadEnvironment" (
            nfIO (loadEnvironment builtinEnvironmentPath)),
        bench "parse" (
            whnfIO (forM modulePaths parse)),
        bgroup "moduleDeclarations" [
            bench "moduleDeclarations" (
                nf (moduleDeclarationsWithEnvironment builtinEnvironment) modules),
            bench "resolve" (
                nf (resolve modules) builtinEnvironment),
            bench "annotate" (
                nf (map (toList . annotate environment)) modules)],
        bgroup "declarationSlices" [
            bench "declarationLocalSlices" (
                nf declarationLocalSlices declarations),
            bench "declarationSCCs" (
                nf declarationSCCs declarations),
            bench "fragmentLocalSlices" (
                nf fragmentLocalSlices fragmentNodes),
            bench "hashSlices2" (
                nf hashSlices2 tempSliceMap)],
        env (for_ slices writeSliceDefault) (\_ ->
            bgroup "sliceCompiler" [
                bench "sliceCompiler" (
                    whnfIO (do
                        removeDirectoryRecursive "fragnix/temp/compilationunits/"
                        writeSliceModules mainSliceID
                        invokeGHCMain mainSliceID)),
                bench "sliceInstances" (
                    nf sliceInstances (toList slices)),
                bench "writeSliceModules" (
                    nfIO (do
                        removeDirectoryRecursive "fragnix/temp/compilationunits/"
                        writeSliceModules mainSliceID))])]


instance NFData Symbol

instance (NFData a) => NFData (Scoped a)
instance (NFData a) => NFData (NameInfo a)
instance (NFData a) => NFData (Error a)
instance (NFData a) => NFData (ModuleName a)
instance (NFData a) => NFData (SpecialCon a)
instance (NFData a) => NFData (HaskellExts.Name a)
instance (NFData a) => NFData (QName a)
instance NFData SrcSpan
instance NFData SrcLoc
instance NFData Boxed

instance NFData Declaration
instance NFData Genre
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
instance NFData LocalSlice
instance NFData LocalSliceID
instance NFData LocalUse
instance NFData LocalReference
instance NFData LocalInstance

deriving instance Generic (Error a)
deriving instance Generic (NameInfo a)
deriving instance Generic SrcSpan
deriving instance Generic (Scoped a)
deriving instance Generic KnownExtension
deriving instance Generic Extension
deriving instance Generic Symbol
