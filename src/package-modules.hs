{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Process (rawSystem)
import System.Environment (getArgs)
import System.Directory (
    getCurrentDirectory,getDirectoryContents,
    createDirectoryIfMissing,copyFile)
import System.FilePath ((</>),takeExtension)
import Control.Monad (forM_)

main :: IO ()
main = do
    [packagequalifier] <- getArgs
    currentdirectory <- getCurrentDirectory
    exitCode <- rawSystem "cabal" [
        "install","--force-reinstalls",
        "--gcc-option=-I/usr/lib/ghc/include",
        "--haskell-suite","-w","haskell-modules",
        "--prefix=" ++ (currentdirectory </> "fragnix" </> "temp"),
        "--package-db=" ++ (currentdirectory </> "fragnix" </> "temp" </> "packages.db"),
        packagequalifier]
    copyModuleFiles
    print exitCode

copyModuleFiles :: IO ()
copyModuleFiles = do
    libraryFolderNames <- getDirectoryContents installationPath >>= return . filter (not . (=='.') . head)
    createDirectoryIfMissing True moduleSourcePath
    forM_ libraryFolderNames (\libraryFolderName -> do
        moduleFileNames <- getDirectoryContents (installationPath </> libraryFolderName) >>= return . filter ((==".hs") . takeExtension)
        forM_ moduleFileNames (\moduleFileName -> do
            copyFile
                (installationPath </> libraryFolderName </> moduleFileName)
                (moduleSourcePath </> moduleFileName)))


installationPath :: FilePath
installationPath = "fragnix" </> "temp" </> "lib" </> "x86_64-linux-haskell-modules-0.1"

moduleSourcePath :: FilePath
moduleSourcePath = "fragnix" </> "temp" </> "modulesources"
