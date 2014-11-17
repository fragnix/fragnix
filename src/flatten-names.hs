module Main where

import Language.Haskell.Names.Interfaces (readInterface,writeInterface)
import System.Directory.Tree (readDirectoryWith,writeDirectoryWith)
import System.Environment (getArgs)
import System.FilePath (makeRelative,splitDirectories,dropExtension,(</>))
import Data.List (intercalate)

main :: IO ()
main = do
    [inputPath,outputPath] <- getArgs
    tree <- readDirectoryWith readInterface inputPath
    writeDirectoryWith (\path symbols -> do
        let path' = outputPath </> intercalate "." (splitDirectories (dropExtension (makeRelative inputPath path)))
        writeInterface path' symbols) tree
    return ()

