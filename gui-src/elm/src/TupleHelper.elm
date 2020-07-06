module TupleHelper exposing (..)

import Json.Encode as E
import Json.Decode as D

decodeTuple : D.Decoder a -> D.Decoder b -> D.Decoder (a, b)
decodeTuple first second =
  D.map2 (\x y -> (x, y))
    (D.field "first" first)
    (D.field "second" second)

encodeTuple : E.Value -> E.Value -> E.Value
encodeTuple first second =
  E.object
    [ ("first", first)
    , ("second", second)
    ]
