{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (writeFile,readFile)

import Fragnix.Slice (Slice(..), SliceID, sliceDirectory)
import Fragnix.LocalSlice (LocalSlice, LocalSliceID)
import Fragnix.HashLocalSlices (hashLocalSlices)

import Data.Aeson (eitherDecode,encode)
import Data.Either (rights)
import Data.ByteString.Lazy (writeFile,readFile)
import Data.Text (unpack)
import Data.Map (Map)

import System.Directory (getCurrentDirectory, listDirectory, doesFileExist, removeFile)
import System.FilePath ((</>))

import Control.Monad (forM, filterM)
-- to prevent too many open files caused by lazy IO
import Control.Exception (evaluate)
-- | Imports for the Servant Server
import Control.Monad.IO.Class (liftIO)

import Servant -- (Server, Handler, serve, (:<|>))
import Servant.Static.TH (createServerExp)
import Network.Wai.Handler.Warp (run)

import Api

staticServer :: Server StaticAPI
staticServer = $(createServerExp "gui-src/elm/dist")

dynamicServer :: Server DynamicAPI
dynamicServer
  = getSlicesHandler
  :<|> saveSlicesHandler

server :: Server API
server
  -- GET /contents
  = dynamicServer
  :<|> staticServer

-- | Start the API Server
main :: IO ()
main = run 8080 (serve api server)

-- | GET /contents implementation
getSlicesHandler :: Handler [Slice]
getSlicesHandler = liftIO getSlices

getSlices :: IO [Slice]
getSlices = do
  allContents <- listDirectory sliceDirectory
  allPaths <- return (map (sliceDirectory </>) allContents)
  filePaths <- filterM doesFileExist allPaths
  eitherSlices <- forM filePaths decodeSlice
  -- ignore anything that's not a valid slice - let the elm part figure
  -- out if the tree is complete
  return (rights eitherSlices)

decodeSlice :: FilePath -> IO (Either String Slice)
decodeSlice p = do
  file <- readFile p
  evaluate (eitherDecode file)

-- | POST /save implementation
saveSlicesHandler :: ([SliceID], [LocalSlice]) -> Handler (Map LocalSliceID SliceID, [Slice])
saveSlicesHandler localSlices = liftIO (saveSlices localSlices)

saveSlices :: ([SliceID], [LocalSlice]) -> IO (Map LocalSliceID SliceID, [Slice])
saveSlices (obsoletes, localSlices) = do
  -- slicesDir <- getCurrentDirectory
  (localSliceIDMap, newSlices) <- return (hashLocalSlices localSlices)

  deletePaths <- return (map (\sid -> sliceDirectory </> (unpack sid)) obsoletes)
  deletePathsSafe <- filterM doesFileExist deletePaths
  _ <- forM deletePathsSafe removeFile

  _ <- forM newSlices (saveSlice sliceDirectory)

  return (localSliceIDMap, newSlices)

saveSlice :: FilePath -> Slice -> IO ()
saveSlice path slice@(Slice sid _ _ _ _) = do
  saveName <- return (path </> (unpack sid))
  done <- writeFile saveName (encode slice)
  evaluate done
