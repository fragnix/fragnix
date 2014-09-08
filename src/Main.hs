module Main where

import Fragnix.Resolver (extractSlices)
import Fragnix.Nest (writeSlice)
import Fragnix.Compiler (compile)

main :: IO ()
main = extractSlices "tests/examples/HelloFragnix.hs" >>= mapM writeSlice >> compile 1 >>= print


