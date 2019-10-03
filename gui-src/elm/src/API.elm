module API exposing (getAllSlices, saveSlices, compile, httpErrorToString, UpdateMap)

import Http
import Slice exposing (Slice, SliceID, sliceDecoder)
import LocalSlice exposing (LocalSlice, LocalSliceID, encodeLocalSlice, decodeLocalSliceID)
import TupleHelper exposing (decodeTuple, encodeTuple)
import Json.Encode as E
import Json.Decode as Decode

type alias ApiResult res = Result Http.Error res

-- | API Requests
getAllSlices : (ApiResult (List Slice) -> msg) -> Cmd msg
getAllSlices toMsg =
  Http.get
    { url =
        apiUrl ++ "/contents"
    , expect =
        Http.expectJson toMsg (Decode.list sliceDecoder)
    }

saveSlices : (ApiResult (UpdateMap, List Slice) -> msg) -> List SliceID -> List LocalSlice -> Cmd msg
saveSlices toMsg obsoletes localSlices =
  Http.post
    { url =
        apiUrl ++ "/save"
    , body =
        Http.jsonBody
          (encodeTuple
            (E.list E.string obsoletes)
            (E.list encodeLocalSlice localSlices))
    , expect =
        Http.expectJson toMsg (decodeTuple decodeUpdateMap (Decode.list sliceDecoder))
    }

compile : (ApiResult String -> msg) -> SliceID -> Cmd msg
compile toMsg sid =
  Http.post
    { url =
        apiUrl ++ "/compile"
    , body =
        Http.jsonBody (E.string sid)
    , expect =
        Http.expectJson toMsg Decode.string
    }

httpErrorToString : Http.Error -> String
httpErrorToString err =
  case err of
    Http.Timeout ->
        "Request timeout"

    Http.NetworkError ->
        "Network error"

    Http.BadBody msg ->
        "Bad Body: " ++ msg

    Http.BadStatus s ->
        "Bad Status: " ++ (String.fromInt s)

    Http.BadUrl msg ->
        "Bad url: " ++ msg

apiUrl : String
apiUrl = "http://localhost:8080"

type alias UpdateMap = List (LocalSliceID, SliceID)

decodeUpdateMap : Decode.Decoder UpdateMap
decodeUpdateMap =
  Decode.list (decodeTuple decodeLocalSliceID Decode.string)
