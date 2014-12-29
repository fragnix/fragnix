module Fragnix.Environment where

import Fragnix.Slice (Reference(Primitive))

import Language.Haskell.Names (Symbol(Value))
import Language.Haskell.Exts (ModuleName(ModuleName),Name(Ident),prettyPrint)

import Data.Map (Map)
import qualified Data.Map as Map (fromList,toList,empty)
import System.FilePath ((</>),dropFileName)
import System.Directory (createDirectoryIfMissing,doesFileExist)
import Data.Aeson (eitherDecode,encode)
import Data.Text (pack)
import qualified Data.ByteString.Lazy as ByteString (readFile,writeFile)

type Environment = Map ModuleName [Symbol]

loadEnvironment :: FilePath -> IO Environment
loadEnvironment path = do
    createDirectoryIfMissing True path
    filenames <- getDirectoryContents path
    let pathmodulnames = map (\filename -> (ModuleName filename,path </> filename)) filenames
    existingPathModulNames <- filterM (doesFileExist . snd) pathmodulnames
    fmap fromList (forM existingPathModulNames (\(modulname,modulpath) -> do
        symbols <- readInterface modulpath
        return (modulname,symbols)))

persistEnvironment :: FilePath -> Environment -> IO ()
persistEnvironment path environment = do
    createDirectoryIfMissing True path
    forM_ (toList environment) (\(modulname,symbols) -> do
        let modulpath = path </> prettyPrint modulname
        writeInterface modulpath symbols)

loadBuiltinEnvironment :: IO Environment
loadBuiltinEnvironment = loadEnvironment builtinEnvironmentPath

environmentPath :: FilePath
environmentPath = "fragnix" </> "environment"

builtinEnvironmentPath :: FilePath
builtinEnvironmentPath = "fragnix" </> "builtin_environment"

mainsymbol :: Symbol
mainsymbol = Value (ModuleName "Main") (Ident "main")
