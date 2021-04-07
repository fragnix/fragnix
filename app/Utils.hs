{-# LANGUAGE OverloadedStrings,DataKinds #-}

module Utils
  ( IDType(..)
  , WithDeps(..)
  , sliceRequest
  , envRequest
  , archiveRequest
  ) where

import Fragnix.Slice (Slice)
import Network.HTTP.Req
import Language.Haskell.Names (Symbol)
import Data.Text (Text, index, pack)

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