{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (writeFile,readFile)

import Fragnix.Slice (Slice)

import Data.Aeson (eitherDecode,encode)
import Data.Either (rights)
import Data.ByteString.Lazy (writeFile,readFile)

import System.Directory (getCurrentDirectory, listDirectory, doesFileExist)
import Control.Monad (forM, filterM)

-- | Imports for the Servant Server
import Control.Monad.IO.Class (liftIO)
import Servant -- (Server, Handler, serve, (:<|>))
import Servant.Static.TH (createServerExp)
import Network.Wai.Handler.Warp (run)

import Api

staticServer :: Server StaticAPI
staticServer = $(createServerExp "/home/florian/Documents/Sem6/fragnix-gui/fragnix/fragnix/gui-src/elm/dist")

dynamicServer :: Server DynamicAPI
dynamicServer = getSlicesHandler

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
  slicesDir <- getCurrentDirectory
  allContents <- listDirectory slicesDir
  filePaths <- filterM doesFileExist allContents
  rawFiles <- forM filePaths readFile
  -- ignore anything that's not a valid slice - let the elm part figure
  -- out if the tree is complete
  slices <- return (rights (map eitherDecode rawFiles))
  return slices
