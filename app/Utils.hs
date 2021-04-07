{-# LANGUAGE OverloadedStrings,DataKinds #-}

module Utils
  ( IDType(..)
  , WithDeps(..)
  , sliceRequest
  , envRequest
  , archiveRequest
  , extract
  ) where

import Fragnix.Slice (Slice)
import Fragnix.Paths (builtinEnvironmentPath)
import Network.HTTP.Req
import Language.Haskell.Names (Symbol)
import Data.Text (Text, index, pack)
import Data.ByteString.Lazy (ByteString)

data IDType = EnvID Text | SliceID Text

data WithDeps = WithDeps | WithoutDeps

url :: Url 'Https
url = https "raw.github.com" /: "fragnix" /: "fragnix-store" /: "main" 

sliceRequest :: Text -> IO (JsonResponse Slice)
sliceRequest id = runReq defaultHttpConfig $ do
  req
    GET
    (url /: "slices" /: pack [index id 0] /: pack [index id 1] /: id)
    NoReqBody
    jsonResponse
    mempty


envRequest :: Text -> IO (JsonResponse [Symbol])
envRequest id = runReq defaultHttpConfig $ do
  req
    GET
    (url /: "environments" /: id)
    NoReqBody
    jsonResponse
    mempty


archiveRequest :: Text -> IO LbsResponse
archiveRequest archive = runReq defaultHttpConfig $ do
  req
    GET
    (url /: "fragnix" /: archive)
    NoReqBody
    lbsResponse
    mempty


extract :: FilePath -> ByteString -> IO ()
extract path = Tar.unpack path . Tar.read . GZip.decompress

