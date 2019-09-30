{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import Fragnix.Slice (Slice, SliceID)
import Fragnix.LocalSlice (LocalSlice, LocalSliceID)

import Servant.API ((:>), Get, Post, ReqBody, JSON, Raw, (:<|>))
import Servant.Static.TH (createApiType)
import Data.Proxy (Proxy(..))
import Data.Map (Map)

import Data.Aeson (
    ToJSON(toJSON),object,(.=),
    FromJSON(parseJSON),withObject,(.:),withText,
    encode,eitherDecode)


-- | API Definition

type API
  -- GET /contents
  = DynamicAPI
  :<|> StaticAPI

type DynamicAPI
  =    "contents" :> Get '[JSON] [Slice]

  :<|> "save"     :> ReqBody '[JSON] (Tuple [SliceID] [LocalSlice])
                  :> Post '[JSON] (Tuple [Tuple LocalSliceID SliceID] [Slice])

  :<|> "compile"  :> ReqBody '[JSON] SliceID
                  :> Post '[JSON] String

type StaticAPI = $(createApiType "/home/florian/Documents/Sem6/fragnix-gui/fragnix/fragnix/gui-src/elm/dist")

api :: Proxy API
api = Proxy


-- | Serialization Helper because JSON does not natively expose Tuples
data Tuple a b = Tuple a b

wrapTuple :: (a, b) -> Tuple a b
wrapTuple (x, y) = Tuple x y

unwrapTuple :: Tuple a b -> (a, b)
unwrapTuple (Tuple x y) = (x, y)

instance (ToJSON a, ToJSON b) => ToJSON (Tuple a b) where
    toJSON (Tuple first second) = object
      [ "first" .= first
      , "second" .= second
      ]

instance (FromJSON a, FromJSON b) => FromJSON (Tuple a b) where
    parseJSON = withObject "tuple" (\o ->
        Tuple <$>
            o .: "first" <*>
            o .: "second")
