{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Network/HTTP/Types/Method.hs" #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Network.HTTP.Types.Method
(
  Method
, methodGet
, methodPost
, methodHead
, methodPut
, methodDelete
, methodTrace
, methodConnect
, methodOptions
, methodPatch
, StdMethod(..)
, parseMethod
, renderMethod
, renderStdMethod
)
where

import           Control.Arrow         ((|||))
import           Data.Array
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import           Data.Typeable

-- | HTTP method (flat string type).
type Method = B.ByteString

-- | HTTP Method constants.
methodGet, methodPost, methodHead, methodPut, methodDelete, methodTrace, methodConnect, methodOptions, methodPatch :: Method
methodGet     = renderStdMethod GET
methodPost    = renderStdMethod POST
methodHead    = renderStdMethod HEAD
methodPut     = renderStdMethod PUT
methodDelete  = renderStdMethod DELETE
methodTrace   = renderStdMethod TRACE
methodConnect = renderStdMethod CONNECT
methodOptions = renderStdMethod OPTIONS
methodPatch   = renderStdMethod PATCH

-- | HTTP standard method (as defined by RFC 2616, and PATCH which is defined
--   by RFC 5789).
data StdMethod
    = GET
    | POST
    | HEAD
    | PUT
    | DELETE
    | TRACE
    | CONNECT
    | OPTIONS
    | PATCH
    deriving (Read, Show, Eq, Ord, Enum, Bounded, Ix, Typeable)
-- These are ordered by suspected frequency. More popular methods should go first.
-- The reason is that methodList is used with lookup.
-- lookup is probably faster for these few cases than setting up an elaborate data structure.

methodArray :: Array StdMethod Method
methodArray = listArray (minBound, maxBound) $ map (B8.pack . show) [minBound :: StdMethod .. maxBound]

methodList :: [(Method, StdMethod)]
methodList = map (\(a, b) -> (b, a)) (assocs methodArray)

-- | Convert a method 'ByteString' to a 'StdMethod' if possible.
parseMethod :: Method -> Either B.ByteString StdMethod
parseMethod bs = maybe (Left bs) Right $ lookup bs methodList

-- | Convert an algebraic method to a 'ByteString'.
renderMethod :: Either B.ByteString StdMethod -> Method
renderMethod = id ||| renderStdMethod

-- | Convert a 'StdMethod' to a 'ByteString'.
renderStdMethod :: StdMethod -> Method
renderStdMethod m = methodArray ! m
