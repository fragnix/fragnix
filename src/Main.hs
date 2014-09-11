module Main where

import Fragnix.Slice (Slice(Slice))
import Fragnix.Resolver (resolve)
import Fragnix.Nest (writeSlice,doesSliceExist)
import Fragnix.Compiler (compile)

import Control.Monad (forM,forM_)
import System.Environment (getArgs)
import System.Exit (ExitCode)

fragnix :: FilePath -> IO ExitCode
fragnix filePath = do
    putStr ("Resolving " ++ filePath ++ " ... ")
    (slices,mainID) <- resolve filePath
    putStrLn (show (length slices) ++ " slices!")
    putStr "Inserting ... "
    existingSlices <- forM slices (\(Slice sliceID _ _) -> doesSliceExist sliceID)
    forM_ slices writeSlice
    putStrLn (show (length (filter not existingSlices)) ++ " new!")
    compile mainID

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> fragnix path >> return ()
        _ -> putStrLn "Usage: fragnix Main.hs"
