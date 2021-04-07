{-# LANGUAGE OverloadedStrings #-}

module Get where

import Control.Monad.IO.Class
import Data.Text (Text, pack, unpack)
import Data.Aeson
import Network.HTTP.Req
import Fragnix.Slice (Slice, writeSlice, sliceNestedPath, moduleNameReference, usedSliceIDs, sliceInstanceIDs, loadSliceIDsTransitive)
import Fragnix.Core.Slice (uses, reference, Reference(OtherSlice, Builtin))
import Fragnix.Environment (writeSymbols)
import Fragnix.Paths (slicesPath, environmentPath)
import Language.Haskell.Names (Symbol, symbolModule)
import Language.Haskell.Exts (ModuleName (..), prettyPrint)
import Data.Maybe (isJust)
import Text.Read (readMaybe)
import Control.Monad (forM_, filterM, unless)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Utils (IDType (SliceID, EnvID), sliceRequest, envRequest)

get :: IDType -> String -> Bool -> IO ()
get SliceID = fetchSlice
get EnvID = fetchEnv


fetchSlice :: String -> Bool -> IO ()
fetchSlice id nodeps = do
  sliceExists <- doesSliceExist id
  if sliceExists
    then putStrLn $ "Slice " ++ id ++ " already fetched."
    else do
      putStrLn $ "Fetching slice " ++ id
      r <- sliceRequest id
      let slice = responseBody r :: Slice
      writeSlice slicesPath slice
      unless nodeps $ do
        slicesToDownload <- loadSliceIDsTransitive slicesPath [pack id]
        forM_ slicesToDownload (\slice -> fetchSlice (unpack slice) False)


fetchEnv :: String -> Bool -> IO ()
fetchEnv id nodeps = do
  let modulePath = environmentPath </> id
  envExists <- doesFileExist modulePath
  if envExists
    then putStrLn $ "Environment " ++ id ++ " already fetched."
    else do
      putStrLn $ "Fetching environment " ++ id
      r <- envRequest id
      let symbols = (responseBody r :: [Symbol])
      createDirectoryIfMissing True environmentPath
      writeSymbols modulePath symbols
      unless nodeps $ do
        let slices = filter isSlice symbols
        forM_ slices (\slice -> liftIO $ do
          let sliceID = tail $ symbolModuleName slice
          fetchSlice sliceID False)


isSlice :: Symbol -> Bool
isSlice s = case moduleNameReference (symbolModuleName s) of
  OtherSlice _ -> True
  Builtin _ -> False


symbolModuleName :: Symbol -> String
symbolModuleName s = getName (symbolModule s)
  where getName (ModuleName () s) = s


doesSliceExist :: String -> IO Bool
doesSliceExist slice = doesFileExist (slicesPath </> sliceNestedPath (pack slice))