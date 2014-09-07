module Main where

import Fragnix.Compiler (compile,sliceModuleName,slicePath)

import System.Process (rawSystem)



main :: IO ()
main = do
    compile 0
    rawSystem "ghc" ["-ifragnix","-main-is",sliceModuleName 0,slicePath 0] >>= print


