module Fragnix.Environment where

import Fragnix.Slice (SliceID)

import Language.Haskell.Names (
    Symbol(symbolName,symbolModule))
import Language.Haskell.Names.Interfaces (
    readInterface,writeInterface)
import Language.Haskell.Exts (
    ModuleName(ModuleName),Name(Ident),prettyPrint)

import Data.Map (
    Map)
import qualified Data.Map as Map (
    fromList,toList,lookup,map)
import System.FilePath (
    (</>))
import System.Directory (
    createDirectoryIfMissing,doesFileExist,getDirectoryContents)
import Control.Monad (
    filterM,forM,forM_,guard)

type Environment = Map ModuleName [Symbol]

loadEnvironment :: FilePath -> IO Environment
loadEnvironment path = do
    createDirectoryIfMissing True path
    filenames <- getDirectoryContents path
    let pathmodulnames = map (\filename -> (ModuleName filename,path </> filename)) filenames
    existingPathModulNames <- filterM (doesFileExist . snd) pathmodulnames
    fmap Map.fromList (forM existingPathModulNames (\(modulname,modulpath) -> do
        symbols <- readInterface modulpath
        return (modulname,symbols)))

persistEnvironment :: FilePath -> Environment -> IO ()
persistEnvironment path environment = do
    createDirectoryIfMissing True path
    forM_ (Map.toList environment) (\(modulname,symbols) -> do
        let modulpath = path </> prettyPrint modulname
        writeInterface modulpath symbols)

environmentPath :: FilePath
environmentPath = "fragnix" </> "environment"

builtinEnvironmentPath :: FilePath
builtinEnvironmentPath = "fragnix" </> "builtin_environment"


-- | Find the possible IDs of the main slice in a map from symbol to slice.
findMainSliceIDs :: Map Symbol SliceID -> [SliceID]
findMainSliceIDs symbolSlices = do
    (symbol,sliceID) <- Map.toList symbolSlices
    guard (symbolName symbol == Ident "main")
    return sliceID

-- | Replace in the given environment the original module of all symbols
-- with the ID of the binding slice.
updateEnvironment :: Map Symbol SliceID -> Environment -> Environment
updateEnvironment symbolSlices environment =
    Map.map (map (updateSymbol symbolSlices)) environment

-- | When the given symbol is in the map replace its original module with
-- the SliceID from the map.
updateSymbol :: Map Symbol SliceID -> Symbol -> Symbol
updateSymbol symbolSlices symbol = case Map.lookup symbol symbolSlices of
    Nothing -> symbol
    Just sliceID -> symbol { symbolModule = ModuleName (show sliceID) }
