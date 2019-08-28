module Main where

import Prelude hiding (writeFile,readFile)

-- import Fragnix.HashLocalSlices (hashLocalSlices)
import Fragnix.Slice (Slice)

import Data.Aeson (eitherDecode,encode)
import Data.Either (rights)
import Data.ByteString.Lazy (writeFile,readFile)
-- import System.FilePath ((<.>))

-- import System.Environment (getArgs)

import System.Directory (getCurrentDirectory, listDirectory)
import Control.Monad (forM)


-- | For a first step, read all slices in a directory and ...dump them to stdout
main :: IO ()
main = do
  slicesDir <- getCurrentDirectory
  sliceFilePaths <- listDirectory slicesDir
  -- _ <- forM sliceFilePaths putStrLn
  rawSlices <- forM sliceFilePaths readFile
  slices <- return (rights (map eitherDecode rawSlices))
  _ <- forM (map showSlice slices) putStrLn
  return ()

-- | Turn a slice into a repl printable string for debugging
showSlice :: Slice -> String
showSlice = show
