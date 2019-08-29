{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Fragnix.Slice (Slice)

import Servant.API ((:>), Get, JSON)
import Data.Proxy (Proxy(..))


-- | API Definition

type API
  -- GET /contents
  = "contents" :> Get '[JSON] [Slice]

api :: Proxy API
api = Proxy
