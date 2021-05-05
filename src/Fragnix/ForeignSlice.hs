{-# LANGUAGE NamedFieldPuns, StandaloneDeriving #-}
module Fragnix.ForeignSlice
  ( hashForeignSlice
  , writeForeignSlice
  , readForeignSlice
  , extractForeignFile
  , convertFileToForeignSlice
  , fileToForeignSlice
  , writeForeignFiles
  ) where

import Prelude hiding (readFile, writeFile)

import Fragnix.Core.ForeignSlice
import Fragnix.Core.Slice (SliceID)

import Fragnix.Paths (cbitsPath, foreignSlicesPath, includePath, slicesPath)
import Fragnix.Slice
    (loadSlicesTransitive, sliceNestedPath, usedForeignSliceIDs)

import Control.Exception (Exception, throwIO)
import Control.Monad (forM, forM_)
import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BS (readFile, writeFile)
import Data.Hashable (Hashable)
import qualified Data.Hashable as Hashable (hash)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.IO as T (readFile, writeFile)
import Data.Typeable (Typeable)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName, takeExtension, takeFileName, (</>))

data ForeignSliceParseError = ForeignSliceParseError FilePath String

deriving instance Typeable ForeignSliceParseError
deriving instance Show ForeignSliceParseError

instance Exception ForeignSliceParseError

hashForeignSlice :: Hashable a => a -> SliceID
hashForeignSlice a = T.pack (show (abs (fromIntegral (Hashable.hash a) :: Integer)))

writeForeignSlice :: FilePath -> ForeignSlice -> IO ()
writeForeignSlice path slice@ForeignSlice { sliceID } = do
  let slicePath = path </> sliceNestedPath sliceID
  createDirectoryIfMissing True (dropFileName slicePath)
  BS.writeFile slicePath (encodePretty slice)

writeForeignFiles :: [SliceID] -> IO ()
writeForeignFiles sliceIDs = do
  createDirectoryIfMissing True cbitsPath
  createDirectoryIfMissing True includePath
  slices <- loadSlicesTransitive slicesPath sliceIDs
  let foreignSliceIDs = concatMap usedForeignSliceIDs slices
  foreignSlices <- forM foreignSliceIDs $ readForeignSlice foreignSlicesPath
  forM_ foreignSlices extractForeignFile

readForeignSlice :: FilePath -> SliceID -> IO ForeignSlice
readForeignSlice path sliceID = readForeignSliceFile $ path </> sliceNestedPath sliceID

readForeignSliceFile :: FilePath -> IO ForeignSlice
readForeignSliceFile path = do
  sliceFile <- BS.readFile path
  either (throwIO . ForeignSliceParseError path) return (eitherDecode sliceFile)

extractForeignFile :: ForeignSlice -> IO ()
extractForeignFile ForeignSlice {code, fileName} = T.writeFile path code
  where
    name = T.unpack fileName
    path = case takeExtension name of
      ".c" -> cbitsPath </> name
      ".h" -> includePath </> name

fileToForeignSlice :: FilePath -> IO ForeignSlice
fileToForeignSlice path = T.readFile path >>= \file -> return ForeignSlice
  { fileName = T.pack $ takeFileName path
  , code = file
  , sliceID = hashForeignSlice (T.pack $ takeFileName path, file) }

convertFileToForeignSlice :: FilePath -> IO ()
convertFileToForeignSlice path = fileToForeignSlice path >>= writeForeignSlice foreignSlicesPath
