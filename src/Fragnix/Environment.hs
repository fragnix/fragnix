module Fragnix.Environment where

import Fragnix.Slice (Reference(Primitive))
import Fragnix.Symbols (loadSymbols,primitiveSymbolsPath)

import Language.Haskell.Names (Symbol(Value))
import Language.Haskell.Exts (ModuleName(ModuleName),Name(Ident),prettyPrint)

import Data.Map (Map)
import qualified Data.Map as Map (fromList,toList,empty)
import System.FilePath ((</>),dropFileName)
import System.Directory (createDirectoryIfMissing,doesFileExist)
import Data.Aeson (eitherDecode,encode)
import Data.Text (pack)
import qualified Data.ByteString.Lazy as ByteString (readFile,writeFile)

type Environment = Map Symbol Reference

loadEnvironment :: FilePath -> IO Environment
loadEnvironment path = do
    exists <- doesFileExist path
    if exists
        then do
            file <- ByteString.readFile path
            case eitherDecode file of
                Right environment -> return (Map.fromList environment)
                Left errormessage -> error ("Failed to load environment at " ++ path ++ "\n" ++ errormessage)
        else return Map.empty

persistEnvironment :: FilePath -> Environment -> IO ()
persistEnvironment path environment = do
    createDirectoryIfMissing True (dropFileName path)
    ByteString.writeFile path (encode (Map.toList environment))

loadPrimitiveEnvironment :: IO Environment
loadPrimitiveEnvironment = do
    symbolmap <- loadSymbols primitiveSymbolsPath
    return (Map.fromList (do
        (modulname,symbols) <- Map.toList symbolmap
        symbol <- symbols
        return (symbol,Primitive (pack (prettyPrint modulname)))))

environmentPath :: FilePath
environmentPath = "fragnix" </> "environment" </> "environment.json"

mainsymbol :: Symbol
mainsymbol = Value (ModuleName "Main") (Ident "main")
