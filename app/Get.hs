{-# LANGUAGE OverloadedStrings #-}

module Get (get) where

import Control.Monad.IO.Class
import Data.Text (Text, pack, unpack)
import Network.HTTP.Req
import Fragnix.Slice (Slice, writeSlice, sliceNestedPath, moduleNameReference, loadSliceIDsTransitive)
import Fragnix.Core.Slice (Reference(OtherSlice, Builtin))
import Fragnix.Environment (writeSymbols)
import Fragnix.Paths (slicesPath, environmentPath)
import Language.Haskell.Names (Symbol, symbolModule, readSymbols)
import Language.Haskell.Exts (ModuleName (..))
import Control.Monad (forM_, when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Utils (IDType(..), WithDeps(..), sliceRequest, envRequest)

get :: IDType -> WithDeps -> IO ()
get (SliceID sliceId) = getSlice sliceId
get (EnvID envId) = getEnv envId


getSlice :: Text -> WithDeps -> IO ()
getSlice sliceId nodeps = do
  fetchSlice sliceId
  case nodeps of 
    WithDeps -> do
      slicesToDownload <- loadSliceIDsTransitive slicesPath [sliceId]
      forM_ slicesToDownload (\slice -> do
        when (slice /= sliceId) $ getSlice slice WithDeps)
    WithoutDeps -> return ()


fetchSlice :: Text -> IO ()
fetchSlice sliceId = do
  sliceExists <- doesSliceExist sliceId
  if sliceExists
    then do
      putStrLn $ "Slice " ++ unpack sliceId ++ " already fetched."
    else do
      putStrLn $ "Fetching slice " ++ unpack sliceId
      r <- sliceRequest sliceId
      let slice = responseBody r :: Slice
      writeSlice slicesPath slice


getEnv :: Text -> WithDeps -> IO ()
getEnv envId nodeps = do
  env <- fetchEnv envId
  case nodeps of
    WithDeps -> do
      let slices = filter isSlice env
      forM_ slices (\slice -> liftIO $ do
        let sliceID = pack . tail $ symbolModuleName slice
        getSlice sliceID WithDeps)
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
  Builtin _ -> False


symbolModuleName :: Symbol -> String
symbolModuleName s = getName (symbolModule s)
  where getName (ModuleName () name) = name


doesSliceExist :: Text -> IO Bool
doesSliceExist slice = doesFileExist (slicesPath </> sliceNestedPath slice)