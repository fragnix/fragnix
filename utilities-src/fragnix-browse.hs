{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude hiding (writeFile,readFile)

-- import Fragnix.HashLocalSlices (hashLocalSlices)
import Fragnix.Slice (Slice)

import Data.Aeson (eitherDecode,encode)
import Data.Either (rights)
import Data.ByteString.Lazy (writeFile,readFile)
-- import System.FilePath ((<.>))

-- import System.Environment (getArgs)

import System.Directory (getCurrentDirectory, listDirectory)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)

-- imports for Servant
import Servant.API
-- import Servant.API.ContentTypes
import Servant
import Data.Proxy
import Control.Monad.Trans.Either (EitherT)
import Network.Wai.Handler.Warp

type API
  -- GET /contents
  = "contents" :> Get '[Servant.API.JSON] [Slice]

server :: Server API
server
  -- GET /contents
  = getSlicesHandler

api :: Proxy API
api = Proxy

-- This is Servant's default handler type.
-- type Handler a = EitherT Servant.ServantErr IO a

getSlices :: IO [Slice]
getSlices = do
  slicesDir <- getCurrentDirectory
  sliceFilePaths <- listDirectory slicesDir
  -- _ <- forM sliceFilePaths putStrLn
  rawSlices <- forM sliceFilePaths readFile
  -- ignore anything that's not a valid slice - let the elm part figure
  -- out if the tree is complete
  slices <- return (rights (map eitherDecode rawSlices))
  return slices

getSlicesHandler :: Handler [Slice]
getSlicesHandler = liftIO getSlices



-- | Second Step: Exposing directory contents to localhost
main :: IO ()
main = run 8080 (serve api server)
