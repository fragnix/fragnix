{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Process (rawSystem)
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

main :: IO ()
main = do
    [packagequalifier] <- getArgs
    currentdirectory <- getCurrentDirectory
    exitCode <- rawSystem "cabal" [
        "install","--force-reinstalls",
        "--gcc-option=-I/usr/lib/ghc/include",
        "--haskell-suite","-w","haskell-modules",
        "--prefix=" ++ (currentdirectory </> "fragnix" </> "temp"),
        "--package-db=" ++ (currentdirectory </> "fragnix" </> "temp" </> "packages.db"),
        packagequalifier]
    print exitCode
