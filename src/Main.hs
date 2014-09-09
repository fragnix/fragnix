module Main where

import Fragnix.Resolver (resolve)
import Fragnix.Nest (writeSlice)
import Fragnix.Compiler (compile)

import System.Exit (ExitCode)

fragnix :: FilePath -> IO ExitCode
fragnix filePath = do
    (slices,mainID) <- resolve filePath
    mapM writeSlice slices
    compile mainID

main :: IO ()
main = fragnix "tests/examples/HelloFragnix.hs" >>= print
