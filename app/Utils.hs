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
import Network.HTTP.Req
import Language.Haskell.Names (Symbol)
import Data.Text (Text, index, pack)
import Data.ByteString.Lazy (ByteString)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip

data IDType = EnvID Text | SliceID Text

data WithDeps = WithDeps | WithoutDeps

url :: Url 'Https
url = https "raw.github.com" /: "fragnix" /: "fragnix-store" /: "main" 

sliceRequest :: Text -> IO (JsonResponse Slice)
sliceRequest sliceId = runReq defaultHttpConfig $ do
  req
    GET
    (url /: "slices" /: pack [index sliceId 0] /: pack [index sliceId 1] /: sliceId)
    NoReqBody
    jsonResponse
    mempty


envRequest :: Text -> IO (JsonResponse [Symbol])
envRequest envId = runReq defaultHttpConfig $ do
  req
    GET
    (url /: "environments" /: envId)
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
