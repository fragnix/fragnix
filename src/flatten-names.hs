module Main where

import System.Directory.Tree (readDirectory,writeDirectoryWith)
import System.Environment (getArgs)
import System.FilePath (makeRelative,splitDirectories,(</>))
import Data.List (intercalate)

main :: IO ()
main = do
    [inputPath,outputPath] <- getArgs
    tree <- readDirectory inputPath
    writeDirectoryWith (\path content -> do
        let path' = outputPath </> intercalate "." (splitDirectories (makeRelative inputPath path))
        writeFile path' content) tree
    return ()

