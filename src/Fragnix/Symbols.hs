module Fragnix.Symbols where

import Language.Haskell.Names.Interfaces (readInterface,writeInterface)
import Language.Haskell.Names (Symbol)
import Language.Haskell.Exts (ModuleName(ModuleName),prettyPrint)

import Data.Map (Map,fromList,toList)
import System.FilePath ((</>))
import System.Directory (
    getDirectoryContents,doesFileExist,createDirectoryIfMissing)
import Control.Monad (forM,filterM,forM_)

loadSymbols :: FilePath -> IO (Map ModuleName [Symbol])
loadSymbols path = do
    createDirectoryIfMissing True path
    filenames <- getDirectoryContents path
    let pathmodulnames = map (\filename -> (ModuleName filename,path </> filename)) filenames
    existingPathModulNames <- filterM (doesFileExist . snd) pathmodulnames
    fmap fromList (forM existingPathModulNames (\(modulname,modulpath) -> do
        symbols <- readInterface modulpath
        return (modulname,symbols)))

persistSymbols :: FilePath -> Map ModuleName [Symbol] -> IO ()
persistSymbols path symbolmap = do
    createDirectoryIfMissing True path
    forM_ (toList symbolmap) (\(modulname,symbols) -> do
        let modulpath = path </> prettyPrint modulname
        writeInterface modulpath symbols)

primitiveSymbolsPath :: FilePath
primitiveSymbolsPath = "fragnix" </> "primitive_names"

symbolsPath :: FilePath
symbolsPath = "fragnix" </> "names"

