module Main where

import Fragnix.Resolver (extractSlices)

main :: IO ()
main = extractSlices "tests/examples/HelloFragnix.hs" >>= print


