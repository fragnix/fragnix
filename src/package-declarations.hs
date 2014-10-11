{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Process (rawSystem)
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
    let packagequalifier = "containers"
    createDirectoryIfMissing True "fragnix/names/"
    writeFile "fragnix/names/packages.db" "[]"
    exitCode <- rawSystem "cabal" [
        "install","--force-reinstalls",
        "--package-db=fragnix/builtin/packages.db",
        "--package-db=fragnix/names/packages.db",
        "--prefix=/home/pschuster/Projects/fragnix-test/fragnix/names/",
        "--libdir=$prefix",
        "--libsubdir=$pkgid",
        "--gcc-option=-I/usr/lib/ghc/include",
        "--haskell-suite","-w","module-declarations",
        packagequalifier]
    print exitCode
