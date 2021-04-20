module Fragnix.Environment
 ( loadEnvironment
 , persistEnvironment
 , writeSymbols
 ) where

import Language.Haskell.Exts (ModuleName (ModuleName), prettyPrint)
import Language.Haskell.Names (Symbol)
import Language.Haskell.Names.Environment (Environment, readSymbols)

import Control.Monad (filterM, forM, forM_)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BS (pack, writeFile)
import Data.Char (ord)
import qualified Data.Map as Map (fromList, toList)
import System.Directory
    (createDirectoryIfMissing, doesFileExist, getDirectoryContents)
import System.FilePath ((</>))


loadEnvironment :: FilePath -> IO Environment
loadEnvironment path = do
    createDirectoryIfMissing True path
    filenames <- getDirectoryContents path
    let pathmodulnames = map (\filename -> (ModuleName () filename,path </> filename)) filenames
    existingPathModulNames <- filterM (doesFileExist . snd) pathmodulnames
    fmap Map.fromList (forM existingPathModulNames (\(modulname,modulpath) -> do
        symbols <- readSymbols modulpath
        return (modulname,symbols)))

-- Replaces "writeSymbols" from haskell-names. Uses encodePretty instead.
writeSymbols :: FilePath -> [Symbol] -> IO ()
writeSymbols path symbols =
  BS.writeFile path $
    encodePretty symbols `mappend` BS.pack [fromIntegral $ ord '\n']

persistEnvironment :: FilePath -> Environment -> IO ()
persistEnvironment path environment = do
    createDirectoryIfMissing True path
    forM_ (Map.toList environment) (\(modulname,symbols) -> do
        let modulpath = path </> prettyPrint modulname
        writeSymbols modulpath symbols)

