{-# LANGUAGE OverloadedStrings #-}

module Get (get) where

import Control.Monad.IO.Class
import Data.Text (Text, pack, unpack)
import Data.Aeson
import Network.HTTP.Req
import Fragnix.Slice (Slice, readSlice, writeSlice, sliceNestedPath, moduleNameReference, usedSliceIDs, sliceInstanceIDs, loadSliceIDsTransitive)
import Fragnix.Core.Slice (uses, reference, Reference(OtherSlice, Builtin))
import Fragnix.Environment (writeSymbols)
import Fragnix.Paths (slicesPath, environmentPath)
import Language.Haskell.Names (Symbol, symbolModule, readSymbols)
import Language.Haskell.Exts (ModuleName (..), prettyPrint)
import Data.Maybe (isJust)
import Text.Read (readMaybe)
import Control.Monad (forM_, filterM, unless, when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Utils (IDType(..), WithDeps(..), sliceRequest, envRequest)

get :: IDType -> WithDeps -> IO ()
get (SliceID id) = getSlice id
get (EnvID id) = getEnv id


getSlice :: Text -> WithDeps -> IO ()
getSlice id nodeps = do
  slice <- fetchSlice id
  case nodeps of 
    WithDeps -> do
      slicesToDownload <- loadSliceIDsTransitive slicesPath [id]
      forM_ slicesToDownload (\slice -> do
        when (slice /= id) $ getSlice slice WithDeps)
    WithoutDeps -> return ()


fetchSlice :: Text -> IO Slice
fetchSlice id = do
  sliceExists <- doesSliceExist id
  if sliceExists
    then do
      putStrLn $ "Slice " ++ unpack id ++ " already fetched."
      readSlice slicesPath id
    else do
      putStrLn $ "Fetching slice " ++ unpack id
      r <- sliceRequest id
      let slice = responseBody r :: Slice
      writeSlice slicesPath slice
      return slice


getEnv :: Text -> WithDeps -> IO ()
getEnv id nodeps = do
  env <- fetchEnv id
  case nodeps of
    WithDeps -> do
      let slices = filter isSlice env
      forM_ slices (\slice -> liftIO $ do
        let sliceID = pack . tail $ symbolModuleName slice
        getSlice sliceID WithDeps)
    WithoutDeps -> return ()
        

fetchEnv :: Text -> IO [Symbol]
fetchEnv id = do
  let modulePath = environmentPath </> unpack id
  envExists <- doesFileExist modulePath
  if envExists
    then do
      putStrLn $ "Environment " ++ unpack id ++ " already fetched."
      readSymbols modulePath
    else do
      putStrLn $ "Fetching environment " ++ unpack id
      r <- envRequest id
      let symbols = (responseBody r :: [Symbol])
      createDirectoryIfMissing True environmentPath
      writeSymbols modulePath symbols
      return symbols

isSlice :: Symbol -> Bool
isSlice s = case moduleNameReference (symbolModuleName s) of
  OtherSlice _ -> True
  Builtin _ -> False


symbolModuleName :: Symbol -> String
symbolModuleName s = getName (symbolModule s)
  where getName (ModuleName () s) = s


doesSliceExist :: Text -> IO Bool
doesSliceExist slice = doesFileExist (slicesPath </> sliceNestedPath slice)