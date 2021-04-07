{-# LANGUAGE OverloadedStrings,StandaloneDeriving,DeriveGeneric,DeriveDataTypeable, NamedFieldPuns #-}
module Fragnix.Slice
  ( SliceID
  , Slice(..)
  , Use(..)
  , Reference(..)
  , OriginalModule
  , Language(..)
  , Fragment(..)
  , Qualification
  , Name(..)
  , UsedName(..)
  , Instance(..)
  , InstancePart(..)
  , InstanceID
  , sliceIDModuleName
  , moduleNameReference
  , usedSliceIDs
  , readSlice
  , writeSlice
  , loadSlicesTransitive
  , loadSliceIDsTransitive
  , getSlices
  , sliceNestedPath
  ) where

import Prelude hiding (writeFile,readFile)

import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)

import qualified Data.Text as Text (unpack,pack,length,index)

import Control.Monad.Trans.State.Strict (StateT,execStateT,get,put)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM, forM_, unless)
import Control.Exception (Exception,throwIO)
import Data.Typeable(Typeable)

import Data.ByteString.Lazy (writeFile,readFile)
import System.FilePath ((</>),dropFileName)
import System.Directory (createDirectoryIfMissing)
import Data.Char (isDigit)
import Fragnix.Core.Slice
import Fragnix.Utils (listFilesRecursive)

-- Slice parse errors

data SliceParseError = SliceParseError FilePath String

deriving instance Typeable SliceParseError
deriving instance Show SliceParseError

instance Exception SliceParseError

-- Reading and writing slices to disk

-- | The name we give to the module generated for a slice with the given ID.
sliceIDModuleName :: SliceID -> String
sliceIDModuleName sid = "F" ++ Text.unpack sid

-- | We abuse module names to either refer to builtin modules or to a slice.
-- If the module name refers to a slice it starts with F followed by
-- digits.
moduleNameReference :: String -> Reference
moduleNameReference moduleName =
  case moduleName of
    ('F':rest)
      | all isDigit rest -> OtherSlice (Text.pack rest)
      | otherwise -> Builtin (Text.pack moduleName)
    _ -> Builtin (Text.pack moduleName)


-- | Write the given slice to the given directory
writeSlice :: FilePath -> Slice -> IO ()
writeSlice slicesPath slice@Slice { sliceID } = do
  let slicePath = slicesPath </> sliceNestedPath sliceID
  createDirectoryIfMissing True (dropFileName slicePath)
  writeFile slicePath (encodePretty slice)

-- | Read the slice with the given slice ID from the given directory
readSlice :: FilePath -> SliceID -> IO Slice
readSlice slicesPath sliceID = do
  let slicePath = slicesPath </> sliceNestedPath sliceID
  sliceFile <- readFile slicePath
  either (throwIO . SliceParseError slicePath) return (eitherDecode sliceFile)

-- | Given slice IDs load all slices and all instance slices nedded
-- for compilation.
loadSlicesTransitive :: FilePath -> [SliceID] -> IO [Slice]
loadSlicesTransitive slicesPath sliceIDs = do
    transitiveSliceIDs <- loadSliceIDsTransitive slicesPath sliceIDs
    forM transitiveSliceIDs (readSlice slicesPath)

-- | Given slice IDs find all IDs of all the slices needed
-- for compilation.
loadSliceIDsTransitive :: FilePath -> [SliceID] -> IO [SliceID]
loadSliceIDsTransitive slicesPath sliceIDs = execStateT (forM sliceIDs (loadSliceIDsStateful slicesPath)) []

-- | Given a slice ID load all IDs of all the slices needed for
-- compilation. Keep track of visited slice IDs to avoid loops.
loadSliceIDsStateful :: FilePath -> SliceID -> StateT [SliceID] IO ()
loadSliceIDsStateful slicesPath sliceID = do
    seenSliceIDs <- get
    unless (elem sliceID seenSliceIDs) (do
        put (sliceID : seenSliceIDs)
        slice <- liftIO (readSlice slicesPath sliceID)
        let recursiveSliceIDs = usedSliceIDs slice
            recursiveInstanceSliceIDs = sliceInstanceIDs slice
        forM_ recursiveSliceIDs (loadSliceIDsStateful slicesPath)
        forM_ recursiveInstanceSliceIDs (loadSliceIDsStateful slicesPath))

usedSliceIDs :: Slice -> [SliceID]
usedSliceIDs (Slice _ _ _ uses _) = do
    Use _ _ (OtherSlice sliceID) <- uses
    return sliceID

sliceInstanceIDs :: Slice -> [InstanceID]
sliceInstanceIDs (Slice _ _ _ _ instances) = do
    Instance _ instanceID <- instances
    return instanceID


-- | Return all slices in the given directory
getSlices :: FilePath -> IO [Slice]
getSlices path = do
  slicePaths <- listFilesRecursive path
  forM slicePaths (\slicePath -> do
    sliceFile <- readFile slicePath
    either (throwIO . SliceParseError slicePath) return (eitherDecode sliceFile))

-- | Map the SliceID "12345" to the FilePath "1 </> 2 </> 12345"
sliceNestedPath :: SliceID -> FilePath
sliceNestedPath sliceID
  | Text.length sliceID < 2 = error $ "sliceID \"" <> Text.unpack sliceID <> "\" has less than 2 characters"
  | otherwise = [Text.index sliceID 0] </> [Text.index sliceID 1] </> (Text.unpack sliceID)

