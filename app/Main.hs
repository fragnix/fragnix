module Main where

import System.Environment (getArgs)

import Build (build)

main :: IO ()
main = do
    modulePaths <- getArgs
    build modulePaths

