{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Api where

import Fragnix.Slice (Slice)

import Servant.API ((:>), Get, JSON, Raw, (:<|>))
import Servant.Static.TH (createApiType)
import Data.Proxy (Proxy(..))


-- | API Definition

type API
  -- GET /contents
  = DynamicAPI
  :<|> StaticAPI

type DynamicAPI = "contents" :> Get '[JSON] [Slice]

type StaticAPI = $(createApiType "/home/florian/Documents/Sem6/fragnix-gui/fragnix/fragnix/gui-src/elm/dist")

api :: Proxy API
api = Proxy
