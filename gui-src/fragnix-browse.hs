{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (writeFile,readFile)

-- import Fragnix.HashLocalSlices (hashLocalSlices)
import Fragnix.Slice (Slice)

import Data.Aeson (eitherDecode,encode)
import Data.Either (rights)
import Data.ByteString.Lazy (writeFile,readFile)
-- import System.FilePath ((<.>))

-- import System.Environment (getArgs)

import System.Directory (getCurrentDirectory, listDirectory, doesFileExist)
import Control.Monad (forM, filterM)

-- | Imports for the Servant Server
import Control.Monad.IO.Class (liftIO)

{-
import Servant.API ((:>), Get, JSON)
import Data.Proxy (Proxy(..))
-}

import Servant -- (Server, Handler, serve, (:<|>))
import Servant.Static.TH (createServerExp)

-- import Control.Monad.Trans.Either (EitherT)
import Network.Wai.Handler.Warp (run)

import Api

staticServer :: Server StaticAPI
staticServer = $(createServerExp "/home/florian/Documents/Sem6/fragnix-gui/fragnix/fragnix/gui-src/elm/dist")

-- | Start the API Server
main :: IO ()
main = run 8080 (serve api server)

server :: Server API
server
  -- GET /contents
  = dynamicServer
  :<|> staticServer

dynamicServer :: Server DynamicAPI
dynamicServer = getSlicesHandler



{-
-- | API Definition

type API
  -- GET /contents
  = "contents" :> Get '[JSON] [Slice]

server :: Server API
server
  -- GET /contents
  = getSlicesHandler

api :: Proxy API
api = Proxy
-}

-- | GET /contents implementation

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

getSlicesHandler :: Handler [Slice]
getSlicesHandler = liftIO getSlices
