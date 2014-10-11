{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Process (rawSystem)

main :: IO ()
main = do
    let packagequalifier = "containers"
    exitCode <- rawSystem "cabal" [
        "install","--force-reinstalls",
        "--package-db=fragnix/builtin/packages.db",
        "--package-db=fragnix/names/packages.db",
        "--prefix=fragnix/names/",
        "--gcc-option=-I/usr/lib/ghc/include",
        "--haskell-suite","-w",".cabal-sandbox/bin/module-declarations",
        packagequalifier]
    print exitCode
