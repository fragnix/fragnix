module Main where

import Fragnix.Primitive (loadPrimitiveSymbols)
import Fragnix.ModuleDeclarations (modulDeclarationsAndNames)

import System.Process (rawSystem)

main :: IO ()
main = do
    let packageid = "containers-0.5.5.1"
    exitCode <- rawSystem "cabal" [
        "install","--force-reinstalls",
        "--gcc-option=-I/usr/lib/ghc/include",
        "--haskell-suite","-w","haskell-modules",
        packageid]
    print exitCode
    primitivesymbols <- loadPrimitiveSymbols
    installedpackageid <- undefined
    modulDeclarationsAndNames primitivesymbols undefined
    return ()



