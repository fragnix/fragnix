{-# LANGUAGE OverloadedStrings #-}

module Get (get) where

import Fragnix.Core.ForeignSlice (ForeignSlice)
import Fragnix.Core.Slice (Slice, Reference(..))

import Data.Text (Text, pack, unpack)
import Network.HTTP.Req
import Fragnix.Slice (readSlice, writeSlice, sliceNestedPath, moduleNameReference, loadSliceIDsTransitive, usedForeignSliceIDs)
import Fragnix.ForeignSlice (writeForeignSlice)
import Fragnix.Environment (writeSymbols)
import Fragnix.Paths (foreignSlicesPath, slicesPath, environmentPath)
import Language.Haskell.Names (Symbol, symbolModule, readSymbols)
import Language.Haskell.Exts (ModuleName (..))
import Control.Monad (forM_, when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Utils (IDType(..), WithDeps(..), foreignSliceRequest, sliceRequest, envRequest)

get :: IDType -> WithDeps -> IO ()
get (SliceID sliceId) = getSlice sliceId
get (EnvID envId) = getEnv envId


getSlice :: Text -> WithDeps -> IO ()
getSlice sliceId nodeps = do
  slice <- fetchSlice sliceId
  case nodeps of 
    WithDeps -> do
      slicesToDownload <- loadSliceIDsTransitive slicesPath [sliceId]
      forM_ slicesToDownload (\slice -> do
        when (slice /= sliceId) $ getSlice slice WithDeps)
      let foreignSlicesToDownload = usedForeignSliceIDs slice
      forM_ foreignSlicesToDownload fetchForeignSlice
    WithoutDeps -> return ()


fetchSlice :: Text -> IO Slice
fetchSlice sliceID = do
  sliceExists <- doesSliceExist sliceID
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


fetchForeignSlice :: Text -> IO ()
fetchForeignSlice sliceId = do
  sliceExists <- doesSliceExist sliceId
  if sliceExists
    then do
      putStrLn $ "Slice " ++ unpack sliceId ++ " already fetched."
    else do
      putStrLn $ "Fetching slice " ++ unpack sliceId
      r <- foreignSliceRequest sliceId
      let slice = responseBody r :: ForeignSlice
      writeForeignSlice foreignSlicesPath slice


getEnv :: Text -> WithDeps -> IO ()
getEnv envId nodeps = do
  env <- fetchEnv envId
  case nodeps of
    WithDeps -> do
      let slices = filter isSlice env
      let foreignSlices = filter isForeignSlice env
      forM_ slices (\slice -> do
        let sliceID = pack . tail $ symbolModuleName slice
        getSlice sliceID WithDeps)
      forM_ foreignSlices (\slice -> do
        let sliceID = pack . tail $ symbolModuleName slice
        fetchForeignSlice sliceID)
    WithoutDeps -> return ()
        

fetchEnv :: Text -> IO [Symbol]
fetchEnv envId = do
  let modulePath = environmentPath </> unpack envId
  envExists <- doesFileExist modulePath
  if envExists
    then do
      putStrLn $ "Environment " ++ unpack envId ++ " already fetched."
      readSymbols modulePath
    else do
      putStrLn $ "Fetching environment " ++ unpack envId
      r <- envRequest envId
      let symbols = (responseBody r :: [Symbol])
      createDirectoryIfMissing True environmentPath
      writeSymbols modulePath symbols
      return symbols

isSlice :: Symbol -> Bool
isSlice s = case moduleNameReference (symbolModuleName s) of
  OtherSlice _ -> True
  _ -> False

isForeignSlice :: Symbol -> Bool
isForeignSlice s = case moduleNameReference (symbolModuleName s) of
  ForeignSlice _ -> True
  _ -> False


symbolModuleName :: Symbol -> String
symbolModuleName s = getName (symbolModule s)
  where getName (ModuleName () name) = name


doesSliceExist :: Text -> IO Bool
doesSliceExist slice = doesFileExist (slicesPath </> sliceNestedPath slice)