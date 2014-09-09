module Main where

import Fragnix.Resolver (resolve)
import Fragnix.Nest (writeSlice)
import Fragnix.Compiler (compile)

import System.Exit (ExitCode)

fragnix :: FilePath -> IO ExitCode
fragnix filePath = do
    putStr ("Resolving " ++ filePath ++ " ... ")
    (slices,mainID) <- resolve filePath
    putStrLn (show (length slices) ++ " slices!")
    putStr "Inserting ... "
    mapM writeSlice slices
    putStrLn "done"
    putStr ("Compiling " ++ show mainID ++ " ... ")
    compile mainID

main :: IO ()
main = fragnix "tests/examples/DataTypeNewtype.hs" >>= print
