module Main where

import Fragnix.ModuleDeclarations (modulDeclarations)
import Fragnix.Declaration (writeDeclarations)

import System.Directory (getDirectoryContents,createDirectoryIfMissing)

main :: IO ()
main = do
    modulepaths <- getDirectoryContents "fragnix/modules"
    declarations <- modulDeclarations "fragnix/names" modulepaths
    createDirectoryIfMissing True "fragnix/declarations"
    writeDeclarations "fragnix/declarations/declarations.json" declarations

