module Main where

import Prelude hiding (writeFile,readFile)

import Fragnix.HashLocalSlices (hashLocalSlices)

import Data.Aeson (eitherDecode,encode)
import Data.ByteString.Lazy (writeFile,readFile)
import System.FilePath ((<.>))

import System.Environment (getArgs)


-- | Take a file name on the command line. The file contains a list of local slices
-- in JSON. Write a file that is a JSON encoding of a Map from local slice ID to
-- hashed slice.
main :: IO ()
main = do

    [localSlicesFilePath] <- getArgs

    localSlicesFile <- readFile localSlicesFilePath

    localSlices <- either error return (eitherDecode localSlicesFile)

    let slicesPath = localSlicesFilePath <.> "hashed"
    let slices = hashLocalSlices localSlices

    writeFile slicesPath (encode slices)



