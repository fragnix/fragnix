{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Process (rawSystem)
import System.Environment (getArgs)

main :: IO ()
main = do
    [packagequalifier] <- getArgs
    exitCode <- rawSystem "cabal" [
        "install","--force-reinstalls",
        "--gcc-option=-I/usr/lib/ghc/include",
        "--haskell-suite","-w",".cabal-sandbox/bin/haskell-modules",
        packagequalifier]
    print exitCode
