module Main where

import Fragnix.ModuleDeclarations (moduleDeclarations)
import Fragnix.Declaration (writeDeclarations)

import System.Directory (getDirectoryContents)

main :: IO ()
main = do
    modulepaths <- getDirectoryContents "fragnix/modules"
    declarations <- moduleDeclarations modulepaths
    writeDeclarations "fragnix/declarations/declarations.json" declarations

