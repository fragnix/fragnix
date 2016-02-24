module Main where

import Fragnix.Environment (
    loadEnvironment, builtinEnvironmentPath)
import Fragnix.ModuleDeclarations (
    parse, moduleDeclarationsWithEnvironment)
import Fragnix.DeclarationSlices (
    sliceIDMap, TempID, TempSlice,
    declarationGraph, fragmentSCCs, buildTempSlice,
    sliceBindings, sliceConstructors, instanceTempIDs, sliceMap)

import Criterion.Main (defaultMain, bench, nf)

import Data.Map (Map)
import Control.Monad (forM)
import System.Directory (getDirectoryContents)


main :: IO ()
main = do
    tempSliceMap <- prepareTempSliceMap
    defaultMain [
        bench "sliceIDMap" (nf sliceIDMap tempSliceMap)]


prepareTempSliceMap :: IO (Map TempID TempSlice)
prepareTempSliceMap = do

    builtinEnvironment <- loadEnvironment builtinEnvironmentPath

    let path = "benchmarks/containers/"
    directoryContents <- getDirectoryContents path
    let modulePaths = map (path ++) (filter (not . (=='.') . head) directoryContents)

    modules <- forM modulePaths parse

    let declarations = moduleDeclarationsWithEnvironment builtinEnvironment modules

        fragmentNodes = fragmentSCCs (declarationGraph declarations)

        sliceBindingsMap = sliceBindings fragmentNodes

        constructorMap = sliceConstructors fragmentNodes
        instanceTempIDList = instanceTempIDs sliceBindingsMap fragmentNodes

        tempSlices = map
            (buildTempSlice sliceBindingsMap constructorMap instanceTempIDList)
            fragmentNodes
        tempSliceMap = sliceMap tempSlices

    return tempSliceMap


