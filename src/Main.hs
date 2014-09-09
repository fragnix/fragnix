module Main where

import Fragnix.Resolver (resolve)
import Fragnix.Nest (writeSlice)
import Fragnix.Compiler (compile)

main :: IO ()
main = do
    (slices,mainID) <- resolve "tests/examples/HelloFragnix.hs"
    mapM writeSlice slices
    compile mainID >>= print


