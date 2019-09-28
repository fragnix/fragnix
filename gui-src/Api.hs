{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Api where

import Fragnix.Slice (Slice, SliceID)
import Fragnix.LocalSlice (LocalSlice, LocalSliceID)

import Servant.API ((:>), Get, Post, ReqBody, JSON, Raw, (:<|>))
import Servant.Static.TH (createApiType)
import Data.Proxy (Proxy(..))
import Data.Map (Map)


-- | API Definition

type API
  -- GET /contents
  = DynamicAPI
  :<|> StaticAPI

type DynamicAPI
  =    "contents" :> Get '[JSON] [Slice]

  :<|> "save"     :> ReqBody '[JSON] ([SliceID], [LocalSlice])
                  :> Post '[JSON] (Map LocalSliceID SliceID, [Slice])

  :<|> "compile"  :> ReqBody '[JSON] SliceID
                  :> Post '[JSON] String

type StaticAPI = $(createApiType "/home/florian/Documents/Sem6/fragnix-gui/fragnix/fragnix/gui-src/elm/dist")

api :: Proxy API
api = Proxy
