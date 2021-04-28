{-# LANGUAGE NamedFieldPuns, StandaloneDeriving #-}
module Fragnix.ForeignSlice
  ( hashForeignSlice
  , writeForeignSlice
  , readForeignSlice
  , extractForeignFile
  , convertFileToForeignSlice
  ) where

import Prelude hiding (writeFile,readFile)

import Fragnix.Core.ForeignSlice
import Fragnix.Core.Slice (SliceID)

import Fragnix.Paths (foreignSlicesPath, cbitsPath, includePath)
import Fragnix.Slice (sliceNestedPath)

import Control.Exception (Exception,throwIO)
import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BS (writeFile,readFile)
import Data.Hashable (Hashable)
import qualified Data.Hashable as Hashable (hash)
import Data.Typeable(Typeable)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.IO as T (writeFile, readFile)
import System.FilePath ((</>), dropFileName, takeExtension, takeFileName)
import System.Directory (createDirectoryIfMissing)

data ForeignSliceParseError = ForeignSliceParseError FilePath String

deriving instance Typeable ForeignSliceParseError
deriving instance Show ForeignSliceParseError

instance Exception ForeignSliceParseError

hashForeignSlice :: Hashable a => a -> SliceID
hashForeignSlice a = T.pack (show (abs (fromIntegral (Hashable.hash a) :: Integer)))

writeForeignSlice :: FilePath -> ForeignSlice -> IO ()
writeForeignSlice slicesPath slice@ForeignSlice { sliceID } = do
  let slicePath = slicesPath </> sliceNestedPath sliceID
  createDirectoryIfMissing True (dropFileName slicePath)
  BS.writeFile slicePath (encodePretty slice)

readForeignSlice :: FilePath -> SliceID -> IO ForeignSlice
readForeignSlice path sliceID = readForeignSliceFile $ path </> sliceNestedPath sliceID
  
readForeignSliceFile :: FilePath -> IO ForeignSlice
readForeignSliceFile path = do
  sliceFile <- BS.readFile path
  either (throwIO . ForeignSliceParseError path) return (eitherDecode sliceFile)

extractForeignFile :: ForeignSlice -> IO ()
extractForeignFile ForeignSlice {sliceID, code, fileName} = T.writeFile path code
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