module Fragnix.Environment where


import Language.Haskell.Names (
    Symbol(Value))
import Language.Haskell.Names.Interfaces (
    readInterface,writeInterface)
import Language.Haskell.Exts (
    ModuleName(ModuleName),Name(Ident),prettyPrint)

import Data.Map (
    Map)
import qualified Data.Map as Map (
    fromList,toList)
import System.FilePath (
    (</>))
import System.Directory (
    createDirectoryIfMissing,doesFileExist,getDirectoryContents)
import Control.Monad (
    filterM,forM,forM_)

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

mainsymbol :: Symbol
mainsymbol = Value (ModuleName "Main") (Ident "main")
