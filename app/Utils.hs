{-# LANGUAGE DataKinds, OverloadedStrings #-}

module Utils
  ( IDType(..)
  , WithDeps(..)
  , sliceRequest
  , foreignSliceRequest
  , loafRequest
  , archiveRequest
  , extract
  ) where

import Fragnix.Core.ForeignSlice (ForeignSlice)
import Fragnix.Core.Loaf (Loaf)
import Fragnix.Core.Slice (Slice)

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, index, pack)
import Network.HTTP.Req

data IDType = LoafID Text | SliceID Text

data WithDeps = WithDeps | WithoutDeps

url :: Url 'Https
url = https "raw.github.com" /: "fragnix" /: "fragnix-store" /: "main"

sliceRequest :: Text -> IO (JsonResponse Slice)
sliceRequest sliceID = runReq defaultHttpConfig $ do
  req
    GET
    (url /: "slices" /: pack [index sliceID 0] /: pack [index sliceID 1] /: sliceID)
    NoReqBody
    jsonResponse
    mempty


foreignSliceRequest :: Text -> IO (JsonResponse ForeignSlice)
foreignSliceRequest sliceID = runReq defaultHttpConfig $ do
  req
    GET
    (url /: "foreign" /: pack [index sliceID 0] /: pack [index sliceID 1] /: sliceID)
    NoReqBody
    jsonResponse
    mempty


loafRequest :: Text -> IO (JsonResponse Loaf)
loafRequest loafID = runReq defaultHttpConfig $ do
  req
    GET
    (url /: "loaves" /: pack [index loafID 0] /: pack [index loafID 1] /: loafID)
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
