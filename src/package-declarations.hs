{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Process (rawSystem)

main :: IO ()
main = do
    let packagequalifier = "containers"
    exitCode <- rawSystem "cabal" [
        "install","--force-reinstalls",
        "--gcc-option=-I/usr/lib/ghc/include",
        "--haskell-suite","-w","module-declarations",
        packagequalifier]
    print exitCode
