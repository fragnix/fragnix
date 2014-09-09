module Main where

import Fragnix.Resolver (resolve)
import Fragnix.Nest (writeSlice)
import Fragnix.Compiler (compile)

main :: IO ()
main = resolve "tests/examples/HelloFragnix.hs" >>= mapM writeSlice >> compile 1 >>= print


