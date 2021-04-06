{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Fragnix.Slice (Slice)
import Fragnix.Paths (builtinEnvironmentPath)
import Network.HTTP.Req
import Language.Haskell.Names (Symbol)
import Data.Text (Text, index, pack)
import Data.ByteString.Lazy (ByteString)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip

data IDType = EnvID | SliceID
  
url = https "raw.github.com" /: "fragnix" /: "fragnix-store" /: "main" 

sliceRequest :: String -> IO (JsonResponse Slice)
sliceRequest id = runReq defaultHttpConfig $ do
  req
    GET
    (url /: "slices" /: pack [id !! 0] /: pack [id !! 1] /: pack id)
    NoReqBody
    jsonResponse
    mempty


envRequest :: String -> IO (JsonResponse [Symbol])
envRequest id = runReq defaultHttpConfig $ do
  req
    GET
    (url /: "environments" /: pack id)
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

