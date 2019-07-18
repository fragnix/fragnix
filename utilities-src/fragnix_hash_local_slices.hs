module Main where

import Prelude hiding (writeFile,readFile)

import Fragnix.HashLocalSlices (hashLocalSlices)

import Data.Aeson (eitherDecode,encode)
import Data.ByteString.Lazy (writeFile,readFile)
import System.FilePath ((<.>))

import System.Environment (getArgs)


-- | Take a file name on the command line. The file contains a list of local slices
-- in JSON. Write two files that are JSON encodings of a Map from local slice ID to
-- sliceID and a list of slices.
main :: IO ()
main = do

    [localSlicesFilePath] <- getArgs

    localSlicesFile <- readFile localSlicesFilePath

    localSlices <- either error return (eitherDecode localSlicesFile)

    let slicesPath = localSlicesFilePath <.> "slices"
    let localSliceIDsPath = localSlicesFilePath <.> "localSliceIDs"
    let (localSliceIDMap, slices) = hashLocalSlices localSlices

    writeFile slicesPath (encode slices)



