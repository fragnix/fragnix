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
import Data.Map (Map, toList)

import System.Directory (getCurrentDirectory, listDirectory, doesDirectoryExist, doesFileExist, removeFile)
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
import Helpers (slice, compile)

staticServer :: Server StaticAPI
staticServer = $(createServerExp "gui-src/elm/dist")

dynamicServer :: Server DynamicAPI
dynamicServer
  =    getSlicesHandler
  :<|> saveSlicesHandler
  :<|> compileHandler

server :: Server API
server
  -- GET /contents
  = dynamicServer
  :<|> staticServer

-- | Slice Modules if necessary and start the API Server
main :: IO ()
main = do
  done <- doesDirectoryExist sliceDirectory
  if done then
    putStrLn "A slice directory already exists"
  else
    do
      currentDirectory <- getCurrentDirectory
      directoryContents <- listDirectory currentDirectory
      modulePaths <-
        return (filter (\n -> (take 3 (reverse n)) == "sh.") directoryContents)
      slice modulePaths

  putStrLn "Editor available at: http://localhost:8080/index.html"

  run 8080 (serve api server)

-- | GET /contents implementation
getSlicesHandler :: Handler [Slice]
getSlicesHandler = liftIO getSlices

-- | return all slices in sliceDirectory
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
saveSlicesHandler :: Tuple [SliceID] [LocalSlice] -> Handler (Tuple [Tuple LocalSliceID SliceID] [Slice])
saveSlicesHandler localSlices = liftIO (saveSlices (unwrapTuple localSlices))

-- | delete obsoleted slices, hash and save new slices
saveSlices :: ([SliceID], [LocalSlice]) -> IO (Tuple [Tuple LocalSliceID SliceID] [Slice])
saveSlices (obsoletes, localSlices) = do
  -- slicesDir <- getCurrentDirectory
  (localSliceIDMap, newSlices) <- return (hashLocalSlices localSlices)

  deletePaths <- return (map (\sid -> sliceDirectory </> (unpack sid)) obsoletes)
  deletePathsSafe <- filterM doesFileExist deletePaths
  _ <- forM deletePathsSafe removeFile

  _ <- forM newSlices (saveSlice sliceDirectory)

  return (Tuple (map wrapTuple (toList localSliceIDMap)) newSlices)

saveSlice :: FilePath -> Slice -> IO ()
saveSlice path slice@(Slice sid _ _ _ _) = do
  saveName <- return (path </> (unpack sid))
  done <- writeFile saveName (encode slice)
  evaluate done

-- | POST /compile implementation
compileHandler :: SliceID -> Handler String
compileHandler sliceID = liftIO (compileVerbose sliceID)

-- | stub: in the future, this should give back the command line messages sent
--   by the compiler, so that the frontend can show them directly
compileVerbose :: SliceID -> IO String
compileVerbose sliceID = do
  compile sliceID
  return "Look at command line to see if it worked"
