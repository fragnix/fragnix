{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module Get (get, getSlice, getLoaf, fetchForeignSlice) where

import Fragnix.Core.Config (Config(Config, loaves))
import Fragnix.Core.ForeignSlice (ForeignSlice)
import Fragnix.Core.Loaf (Loaf (..), LoafID, Symbol (..))
import Fragnix.Core.Slice (Reference (..), Slice (..), SliceID)

import Fragnix.Config (readConfig, writeConfig)
import Fragnix.ForeignSlice (writeForeignSlice)
import Fragnix.Loaf (readLoafFile, writeLoaf)
import Fragnix.Paths (environmentPath, foreignSlicesPath, slicesPath, configPath)
import Fragnix.Slice
    (loadSliceIDsTransitive, readSlice, sliceNestedPath, usedForeignSliceIDs,
    writeSlice)

import Control.Monad (forM_, when)
import Data.Text (unpack)
import qualified Data.Map as Map (insert)
import Network.HTTP.Req (responseBody)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Utils
    (IDType (..), WithDeps (..), foreignSliceRequest, loafRequest, sliceRequest)

get :: IDType -> WithDeps -> IO ()
get (SliceID sliceID) = getSlice sliceID
get (LoafID loafID)   = getLoaf loafID


getSlice :: SliceID -> WithDeps -> IO ()
getSlice sliceID nodeps = do
  slice <- fetchSlice sliceID
  case nodeps of
    WithDeps -> do
      slicesToDownload <- loadSliceIDsTransitive slicesPath [sliceID]
      forM_ slicesToDownload (\idToDownload -> do
        when (idToDownload /= sliceID) $ getSlice idToDownload WithDeps)
      let usedForeignSlices = usedForeignSliceIDs slice
      forM_ usedForeignSlices fetchForeignSlice
    WithoutDeps -> return ()


fetchSlice :: SliceID -> IO Slice
fetchSlice sliceID = do
  sliceExists <- doesFileExist (slicesPath </> sliceNestedPath sliceID)
  if sliceExists
    then do
      putStrLn $ "Slice " ++ unpack sliceID ++ " already fetched."
      readSlice slicesPath sliceID
    else do
      putStrLn $ "Fetching slice " ++ unpack sliceID
      r <- sliceRequest sliceID
      let slice = responseBody r :: Slice
      writeSlice slicesPath slice
      return slice


fetchForeignSlice :: SliceID -> IO ()
fetchForeignSlice sliceID = do
  sliceExists <- doesFileExist (foreignSlicesPath </> sliceNestedPath sliceID)
  if sliceExists
    then do
      putStrLn $ "Foreign slice " ++ unpack sliceID ++ " already fetched."
    else do
      putStrLn $ "Fetching foreign slice " ++ unpack sliceID
      r <- foreignSliceRequest sliceID
      let slice = responseBody r :: ForeignSlice
      writeForeignSlice foreignSlicesPath slice


getLoaf :: LoafID -> WithDeps -> IO ()
getLoaf loafID nodeps = do
  loaf@Loaf{name} <- fetchLoaf loafID
  config@Config{loaves} <- readConfig configPath
  writeConfig configPath $ config {loaves = Map.insert name [loafID] loaves}
  case nodeps of
    WithDeps -> do
      let slices = referencedSlices loaf
      let foreignSlices = referencedForeignSlices loaf
      forM_ slices (\sliceID -> do
        getSlice sliceID WithDeps)
      forM_ foreignSlices (\sliceID -> do
        fetchForeignSlice sliceID)
    WithoutDeps -> return ()


fetchLoaf :: LoafID -> IO Loaf
fetchLoaf loafID = do
  let loafPath = environmentPath </> unpack loafID
  envExists <- doesFileExist loafPath
  if envExists
    then do
      putStrLn $ "Loaf " ++ unpack loafID ++ " already fetched."
      readLoafFile loafPath
    else do
      putStrLn $ "Fetching loaf " ++ unpack loafID
      r <- loafRequest loafID
      let loaf = (responseBody r :: Loaf)
      createDirectoryIfMissing True environmentPath
      writeLoaf environmentPath loaf
      return loaf

referencedSlices :: Loaf -> [SliceID]
referencedSlices Loaf{symbols} = do
  OtherSlice sliceID <- fmap symbolModule symbols
  return sliceID

referencedForeignSlices :: Loaf -> [SliceID]
referencedForeignSlices Loaf{symbols} = do
  ForeignSlice sliceID <- fmap symbolModule symbols
  return sliceID
