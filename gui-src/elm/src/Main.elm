module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, p, h1, textarea)
import Html.Attributes exposing (class, value, classList)
import Html.Events exposing (onClick, on)
import Json.Encode as E
import Json.Decode as Decode
import Dict exposing (Dict)
import Http


main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none
{-subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ sendBaseDirectory setDirectory
    , sendSlice cacheSlice
    ]


cacheSlice : E.Value -> Msg
cacheSlice s =
  case Decode.decodeValue sliceWrapDecoder s of
    Ok sw -> CacheSlice sw
    Err e -> case Decode.decodeValue errorRecoveryDecoder s of
      Ok sid -> SliceLoadError sid (Decode.errorToString e)
      Err f  -> Error ("Error decoding slice: " ++ (Decode.errorToString e))
-}

-- HTTP
getAllSlices : Cmd Msg
getAllSlices = Http.get
                  { url =
                      "http://localhost:8080/contents"
                  , expect =
                      Http.expectJson ReceivedSlices (Decode.list sliceWrapDecoder)
                  }

httpErrorToString : Http.Error -> String
httpErrorToString err =
  case err of
    Http.BadUrl str -> "Bad Url: " ++ str
    Http.Timeout    -> "Timeout"
    Http.NetworkError -> "NetworkError"
    -- Http.BadStatus nr -> "Error Status " ++ nr
    _    -> "Something else wrong"

-- MODEL

-- Editor State
type alias Model = Editor

type alias Editor =
  { position: List SliceID
  , cache: Dict SliceID SliceWrap
  , baseDirectory: BaseDirectory
  , error: Maybe String
  }

type alias BaseDirectory = String

type alias SliceWrap =
  { slice: Slice
  , tempSlice: Maybe TempSlice
  , focus: Focus
  , editing: Editing
  , occurences: List SliceID
  }

type alias TempSlice = Slice

type Focus = Focused | Unfocused
type Editing = Active | Inactive


-- Slices
type Slice = Slice SliceID Language Fragment (List Use) (List Instance)

type Language = Language (List GHCExtension)

type Fragment = Fragment (List SourceCode)

type Use = Use (Maybe Qualification) UsedName Reference

type Instance = Instance InstancePart InstanceID

type InstancePart =
    OfThisClass |
    OfThisClassForUnknownType |
    ForThisType |
    ForThisTypeOfUnknownClass

type alias InstanceID = SliceID

type Reference = OtherSlice SliceID | Builtin OriginalModule

type UsedName =
    ValueName Name |
    TypeName Name |
    ConstructorName TypeName Name

type Name = Identifier String | Operator String

type alias TypeName = Name

type alias SliceID = String
type alias SourceCode = String
type alias Qualification = String
type alias OriginalModule = String
type alias GHCExtension = String

type alias Flags = E.Value


init : Flags -> (Model, Cmd Msg)
init _ =
  ( { position = []
    , cache = Dict.empty
    , baseDirectory = ""
    , error = Nothing
    }
  , getAllSlices
  )

-- DECODERS

sliceWrapDecoder : Decode.Decoder SliceWrap
sliceWrapDecoder =
  Decode.map5 SliceWrap
    (Decode.field "slice" sliceDecoder)
    (Decode.succeed Nothing)
    (Decode.succeed Unfocused)
    (Decode.succeed Inactive)
    (Decode.field "occurences" (Decode.list Decode.string))


-- Slice Decoder
errorRecoveryDecoder : Decode.Decoder String
errorRecoveryDecoder =
  (Decode.field "slice" (Decode.field "sliceID" Decode.string))

sliceDecoder : Decode.Decoder Slice
sliceDecoder =
  Decode.map5 Slice
    (Decode.field "sliceID" Decode.string)
    (Decode.field "language" languageDecoder)
    (Decode.field "fragment" fragmentDecoder)
    (Decode.field "uses" (Decode.list useDecoder))
    (Decode.field "instances" (Decode.list instanceDecoder))

languageDecoder : Decode.Decoder Language
languageDecoder =
  Decode.map Language (Decode.field "extensions" (Decode.list Decode.string))

fragmentDecoder : Decode.Decoder Fragment
fragmentDecoder =
  Decode.map Fragment (Decode.list Decode.string)

useDecoder : Decode.Decoder Use
useDecoder =
  Decode.map3 Use
    (Decode.field "qualification" (Decode.nullable Decode.string))
    (Decode.field "usedName" usedNameDecoder)
    (Decode.field "reference" referenceDecoder)

usedNameDecoder : Decode.Decoder UsedName
usedNameDecoder =
  Decode.oneOf
    [ (Decode.map ValueName (Decode.field "valueName" nameDecoder))
    , (Decode.map TypeName (Decode.field "typeName" nameDecoder))
    , (Decode.map2 ConstructorName
        (Decode.field "constructorTypeName" nameDecoder)
        (Decode.field "constructorName" nameDecoder))
    ]

referenceDecoder : Decode.Decoder Reference
referenceDecoder =
  Decode.oneOf
    [ (Decode.map OtherSlice (Decode.field "otherSlice" Decode.string))
    , (Decode.map Builtin (Decode.field "builtinModule" Decode.string))
    ]

nameDecoder : Decode.Decoder Name
nameDecoder =
  Decode.oneOf
    [ (Decode.map Identifier (Decode.field "identifier" Decode.string))
    , (Decode.map Operator (Decode.field "operator" Decode.string))
    ]

instanceDecoder : Decode.Decoder Instance
instanceDecoder =
  Decode.map2 Instance
    (Decode.field "instancePart" instancePartDecoder)
    (Decode.field "instanceID" Decode.string)

instancePartDecoder : Decode.Decoder InstancePart
instancePartDecoder =
  Decode.map
    (\s -> case s of
      "OfThisClass" -> OfThisClass
      "OfThisClassForUnknownType" -> OfThisClassForUnknownType
      "ForThisType" -> ForThisType
      "ForThisTypeOfUnknownClass" -> ForThisTypeOfUnknownClass
      _ -> OfThisClassForUnknownType) -- Dirty dirty Hack!!
    Decode.string

-- UPDATE

type Msg
  = CacheSlice SliceWrap
  | Error String
  | CloseError
  | Push SliceID
  | Pop
  | ReceivedSlices (Result Http.Error (List SliceWrap))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceivedSlices result ->
      case result of
        Err e ->
          ( { model | error = Just (httpErrorToString e) }
          , Cmd.none
          )
        Ok sws ->
          ( List.foldl
              (\sw -> (\m -> insertSlice sw m))
              model
              sws,
            Cmd.none
          )
    CacheSlice sw ->
      ( insertSlice sw model
      , Cmd.none
      )

    Error err ->
      ( { model | error = Just err }
      , Cmd.none
      )

    CloseError ->
      ( { model | error = Nothing }
      , Cmd.none
      )

    Push sid2 ->
      ( { model | position = sid2 :: model.position }
      , Cmd.none
      )

    Pop ->
      ( { model | position = tailE model.position}
      , Cmd.none
      )

tailE : List a -> List a
tailE xs =
  case xs of
    (x::r) -> r
    _      -> []

insertSlice : SliceWrap -> Model -> Model
insertSlice sw model =
  case sw.slice of
    (Slice sid _ _ _ _) ->
      { model | cache = Dict.insert sid sw model.cache }

missingSlices : Model -> List SliceID
missingSlices model =
  collectNeededSlices model
  |> List.filter
      (\s ->
        case Dict.get s model.cache of
          Nothing -> True
          _       -> False)

collectNeededSlices : Model -> List SliceID
collectNeededSlices model =
  case model.position of
    [] -> []
    sid :: []        -> [ sid ]
    s1 :: s2 :: rest ->
     dependencies model s1
      ++ dependencies model s2
      ++ List.concatMap (occurences model) (s2 :: rest)

dependencies : Model -> SliceID -> List SliceID
dependencies model sid =
  case Dict.get sid model.cache of
    Just sw -> sid :: (extractDependencies sw)
    _ -> [ sid ]

occurences : Model -> SliceID -> List SliceID
occurences model sid =
  case Dict.get sid model.cache of
    Just sw -> sid :: sw.occurences
    _ -> [ sid ]



-- VIEW

view : Model -> Html Msg
view model =
  div
    [ class "editorContainer" ]
    ((viewError model) ++ (viewSlices model))

viewError : Model -> List (Html Msg)
viewError model =
  case model.error of
    Just err -> [ div [ class "row" ] [ p [] [ text err ] ] ]
    Nothing  -> []

viewSlices : Model -> List (Html Msg)
viewSlices model =
  case model.position of
    []               -> []
    sid :: []        ->
      [ singleSliceRow model sid
      , dependenciesRow model sid Nothing
      ]
    s1 :: s2 :: rest ->
      [ div [ class "row" ] [ button [ onClick Pop ] [ text "Back " ] ]
      , (dependenciesRow model s1 Nothing)
      , (dependenciesRow model s2 (Just s1)) ]
      ++ (occurenceRows model model.position)
      |> List.reverse

occurenceRows : Model -> List SliceID -> List (Html Msg)
occurenceRows model stack =
  case stack of
    s1 :: s2 :: rest ->
      (occurencesRow model s1 s2) :: (occurenceRows model (s2 :: rest))
    _ -> []

singleSliceRow : Model -> SliceID -> Html Msg
singleSliceRow model sid =
  div [ class "row" ] [ viewSlice model sid Focused Active ]

onLoad : msg -> Html.Attribute msg
onLoad message =
  on "load" (Decode.succeed message)

dependenciesRow : Model -> SliceID -> Maybe SliceID -> Html Msg
dependenciesRow model root focus =
  let
    showSlice : SliceID -> Html Msg
    showSlice sid =
      case focus of
        Just sid2 -> if sid == sid2
          then
            viewSlice model sid Focused Active
          else
            viewSlice model sid Unfocused Inactive

        Nothing   ->
          div
            [ class "container"
            , onClick (Push sid)
            ]
            [ viewSlice model sid Unfocused Inactive ]
  in
    case Dict.get root model.cache of
      Just sw ->
        div [ class "row" ] (List.map showSlice (extractDependencies sw))
      Nothing ->
        errorRow root "404 Slice not found"

occurencesRow : Model -> SliceID -> SliceID -> Html Msg
occurencesRow model root focus =
  let
    showSlice : SliceID -> Html Msg
    showSlice sid =
      if sid == focus then
        viewSlice model sid Focused Inactive
      else
        viewSlice model sid Unfocused Inactive
  in
    case Dict.get root model.cache of
      Just sw ->
        div [ class "row" ] (List.map showSlice sw.occurences)
      Nothing ->
        errorRow root "404 Slice not found"

errorRow :  SliceID -> String -> Html Msg
errorRow causing error =
  div
    [ class "row" ]
    [ errorHolder causing error ]

errorHolder :  SliceID -> String -> Html Msg
errorHolder causing error =
  div
    []
    [ p [] [ text error ]
    ]

extractDependencies : SliceWrap -> List SliceID
extractDependencies sw =
  case sw.slice of
    (Slice _ _ _ uses _) ->
      List.foldl
        (\u acc -> case u of
          (Use _ _ ref) -> case ref of
            OtherSlice sid -> sid :: acc
            _              -> acc)
        []
        uses

viewSlice : Model -> SliceID -> Focus -> Editing -> Html Msg
viewSlice model sid focus editing =
  case Dict.get sid model.cache of
    Just sw ->
      div
        []
        [ textarea
            [ value (extractFragment sw.slice)
            , classList
              [ ("focused", focus == Focused)
              , ("active", editing == Active)
              ]
            ]
            []
        ]
    Nothing ->
      errorHolder sid "Somehow we did not infer you need this."

extractFragment : Slice -> String
extractFragment s =
  case s of
    (Slice _ _ (Fragment frag) _ _) ->
      List.foldl (\x xs -> x ++ "\n" ++ xs) "" frag


-- STUBS
