{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Fragnix.Slice (Slice)
import Network.HTTP.Req
import Language.Haskell.Names (Symbol)
import Data.Text (Text, index, pack)

data IDType = EnvID | SliceID
  
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


url = https "raw.github.com" /: "fragnix" /: "fragnix-store" /: "main" 