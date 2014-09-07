{-# LANGUAGE StandaloneDeriving,DeriveDataTypeable #-}
module Fragnix.Nest where

import Prelude hiding (writeFile,readFile)

import Fragnix.Slice (Slice(Slice),SliceID)

import Control.Exception (Exception,throwIO)
import Data.Typeable(Typeable)

import Data.Aeson (encode,eitherDecode)
import Data.ByteString.Lazy (writeFile,readFile)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)

data SliceParseError = SliceParseError SliceID String

deriving instance Typeable SliceParseError
deriving instance Show SliceParseError

instance Exception SliceParseError

writeSlice :: Slice -> IO ()
writeSlice slice@(Slice sliceID _ _) = do
    createDirectoryIfMissing True sliceDirectory
    writeFile (slicePath sliceID) (encode slice)

readSlice :: SliceID -> IO Slice
readSlice sliceID = do
    sliceFile <- readFile (slicePath sliceID)
    either (throwIO . SliceParseError sliceID) return (eitherDecode sliceFile)

slicePath :: SliceID -> FilePath
slicePath sliceID = sliceDirectory </> show sliceID

sliceDirectory :: FilePath
sliceDirectory = "fragnix" </> "slices"
