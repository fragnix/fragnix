{-# LANGUAGE StandaloneDeriving #-}
module Fragnix.Nest where

import Prelude hiding (writeFile,readFile)

import Fragnix.Slice (Slice(Slice),SliceID)

import Control.Error (EitherT,runEitherT,tryIO,hoistEither,fmapLT)
import Control.Exception (IOException)

import Data.Aeson (encode,eitherDecode)
import Data.ByteString.Lazy (writeFile,readFile)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)

data NestError =
    SliceFileError SliceID IOException |
    SliceParseError SliceID String

deriving instance Show NestError

get :: SliceID -> IO Slice
get sliceID = runEitherT (readSlice sliceID) >>= either (error . show) return

writeSlice :: Slice -> IO ()
writeSlice slice@(Slice sliceID _ _) = do
    createDirectoryIfMissing True sliceDirectory
    writeFile (slicePath sliceID) (encode slice)

readSlice :: SliceID -> EitherT NestError IO Slice
readSlice sliceID = do
    sliceFile <- tryIO (readFile (slicePath sliceID)) `onFailure` SliceFileError sliceID
    hoistEither (eitherDecode sliceFile) `onFailure` SliceParseError sliceID

slicePath :: SliceID -> FilePath
slicePath sliceID = sliceDirectory </> show sliceID

sliceDirectory :: FilePath
sliceDirectory = "fragnix" </> "slices"

onFailure :: (Monad m) => EitherT e m a -> (e -> u) -> EitherT u m a
onFailure = flip fmapLT