module Main exposing (..)

import Browser

import Json.Encode as E
import Json.Decode as Decode
import Dict exposing (Dict)
import Set exposing (Set)
import Http

import Slice exposing (..)

-- imports for view
import Parser exposing (Parser)
import SyntaxHighlight as SH
import SyntaxHighlight.Line as SHL
import Html exposing (Html, button, div, text, p, h1, textarea)
import Html.Attributes exposing (class, value, classList, spellcheck, readonly)
import Html.Events exposing (onClick, on, onMouseEnter, onMouseLeave)


main =
  Browser.element { init = init, update = update, view = view, subscriptions = (\_ -> Sub.none) }

-- | INIT
type alias Flags = E.Value


init : Flags -> (Model, Cmd Msg)
init _ =
  ( { emptyModel | error = Just "Loading and analyzing slices..." }
  , getAllSlices
  )

-- | API Requests
getAllSlices : Cmd Msg
getAllSlices = Http.get
                  { url =
                      "http://localhost:8080/contents"
                  , expect =
                      Http.expectJson ReceivedSlices (Decode.list sliceDecoder)
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

-- | MODEL

-- | Editor State
type alias Model =
  { main:  Maybe SliceID
  , rows: Maybe Rows
  , slices: List SliceWrap
  , cache: Dict SliceID SliceWrap
  , error: Maybe String
  }

type alias Rows =
  { focus: SliceID
  , trace: List SliceID
  , marked: Maybe SliceID
  }

emptyModel : Model
emptyModel =
  { main = Nothing
  , rows = Nothing
  , slices = []
  , cache = Dict.empty
  , error = Nothing
  }

-- | UPDATE

type Msg
  = ReceivedSlices (Result Http.Error (List Slice))
  | Error String
  | CloseError
  | Focus SliceID (List SliceID)
  | Mark SliceID
  | Unmark

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceivedSlices result ->
      case result of
        Err e ->
          ( { model | error = Just (httpErrorToString e) }
          , Cmd.none
          )
        Ok slices ->
          ( loadSlices slices { model | error = Nothing },
            Cmd.none
          )

    Error err ->
      ( { model | error = Just err }
      , Cmd.none
      )

    CloseError ->
      ( { model | error = Nothing }
      , Cmd.none
      )

    Focus sid occs ->
      ( setFocus sid occs model
      , Cmd.none
      )

    Mark sid ->
      ( mark sid model
      , Cmd.none
      )

    Unmark ->
      ( unmark model
      , Cmd.none
      )


-- | ReceivedSlices

loadSlices : List Slice -> Model -> Model
loadSlices slices model =
  insertSlices slices model
  |> indexSlices
  |> computeOccurences
  |> performIntegrityCheck
  |> findMain
  |> setRows

-- | wrap new slices and add them to model
insertSlices : List Slice -> Model -> Model
insertSlices newSlices model =
  { model | slices = (List.map wrap newSlices) }

-- | add slices to cache
indexSlices : Model -> Model
indexSlices model =
  { model | cache =
      List.foldl
        (\s c -> case s.slice of
          (Slice sid _ _ _ _) -> Dict.insert sid s c)
        Dict.empty
        model.slices
  }

-- | add information about where the loaded slices are used
computeOccurences : Model -> Model
computeOccurences model =
  let
    fullCache =
      List.foldl
        addOccurences
        model.cache
        model.slices
    fullSlices =
      Dict.values fullCache
  in
    { model | cache = fullCache, slices = fullSlices }

addOccurences : SliceWrap -> (Dict SliceID SliceWrap) -> (Dict SliceID SliceWrap)
addOccurences sw dict =
  case sw.slice of
    (Slice occId _ _ uses _) ->
      List.foldl
        (\u acc -> case u of
          (Use _ _ ref) -> case ref of
            OtherSlice sid -> addOccurence sid occId acc
            _              -> acc)
        dict
        uses

addOccurence : SliceID -> SliceID -> (Dict SliceID SliceWrap) -> (Dict SliceID SliceWrap)
addOccurence sid occId dict =
  case Dict.get sid dict of
    Nothing -> dict
    Just sw -> Dict.insert sid { sw | occurences = occId :: sw.occurences } dict

-- | check if any slice references a slice that is not in the cache
performIntegrityCheck : Model -> Model
performIntegrityCheck model =
  case integrityCheck model of
    Ok _        -> model
    Err missing -> { model | error = Just (missingSlicesToString missing) }

missingSlicesToString : Set SliceID -> String
missingSlicesToString missing =
  "Missing Slices: "
  ++ String.concat (List.map (\x -> x ++ " ") (Set.toList missing))

integrityCheck : Model -> Result (Set SliceID) ()
integrityCheck model =
  List.foldl (checkDependencies model.cache) (Ok ()) model.slices

checkDependencies : (Dict SliceID SliceWrap) -> SliceWrap -> Result (Set SliceID) () -> Result (Set SliceID) ()
checkDependencies cache sw res =
  case sw.slice of
    (Slice _ _ _ uses _) ->
      List.foldl
        (\u acc -> case u of
          (Use _ _ ref) -> case ref of
            OtherSlice sid -> checkDependency sid cache acc
            _              -> acc)
        res
        uses

checkDependency : SliceID -> (Dict SliceID SliceWrap) -> Result (Set SliceID) () -> Result (Set SliceID) ()
checkDependency sid cache res =
  case Dict.get sid cache of
    Just _  -> res
    Nothing -> case res of
      Ok _         -> Err (Set.insert sid Set.empty)
      Err missing  -> Err (Set.insert sid missing)

-- | first main function found in the slices list
findMain : Model -> Model
findMain model =
  case List.filter isMain model.slices of
    x :: _ -> { model | main = Just x.id }
    _      -> model

isMain : SliceWrap -> Bool
isMain sw =
  String.startsWith "main " sw.name

-- | set a start state for showing the editor
setRows : Model -> Model
setRows model =
  case model.main of
    -- for testing purposes
    Nothing  ->
      case model.slices of
        [] -> model
        x :: rst ->
          { model | rows = Just { focus = x.id, trace = [], marked = Nothing } }
    Just sid ->
      { model | rows = Just { focus = sid, trace = [], marked = Nothing } }

-- | Focus
-- | Set current open slice and trace
setFocus : SliceID -> List SliceID -> Model -> Model
setFocus sid occs model =
  case model.rows of
    Nothing ->
      { model | rows = Just {focus = sid, trace = [], marked = Nothing } }
    Just {focus, trace} ->
      { model | rows =
          Just { focus = sid
               , trace = pruneUntilIn occs (focus :: trace)
               , marked = Nothing
               }
      }

-- | shorten the trace until it ends with an occurence of the
-- | new focused slice
pruneUntilIn : List SliceID -> List SliceID -> List SliceID
pruneUntilIn set xs =
  case xs of
    []       -> []
    x :: rst ->
      if List.member x set then xs else pruneUntilIn set rst

-- | Mark
-- | Highlight a slice
mark : SliceID -> Model -> Model
mark sid model =
  { model | rows = Maybe.map
      (\r -> { r | marked = Just sid })
      model.rows
  }

-- | Unmark
-- | stop highlighting any slice
unmark : Model -> Model
unmark model =
  { model | rows = Maybe.map
      (\r -> { r | marked = Nothing })
      model.rows
  }


-- | VIEW
view : Model -> Html Msg
view model =
  case model.error of
    Just err -> viewErrMsg err
    Nothing  -> viewEditor model

viewErrMsg : String -> Html Msg
viewErrMsg err =
  div
    [ class "editorContainer" ]
    [ div
        [ class "row" ]
        [ viewError err
        , button [ onClick CloseError ] [ text "Ignore and show editor" ]
        ]
    ]

viewEditor : Model -> Html Msg
viewEditor model =
  div
    [ class "editorContainer" ]
    ( case model.rows of
        Just r   -> viewRowEditor r model
        Nothing  -> [ p [] [ text "No rows generated" ] ]
    )

viewRowEditor : Rows -> Model -> List (Html Msg)
viewRowEditor {focus, trace} model =
  List.reverse
    ([ dependenciesRow focus Nothing model
    , (case trace of
        parent :: _ -> dependenciesRow parent (Just focus) model
        _           -> singleRow focus model)
    ] ++ occurenceRows (focus :: trace) model)

singleRow : SliceID -> Model -> Html Msg
singleRow sid model =
  div [ class "row" ] [ tryViewSlice model References sid ]

occurenceRows : List SliceID -> Model -> List (Html Msg)
occurenceRows trace model =
  case trace of
    x :: y :: rst ->
      (occurencesRow x (Just y) model) :: (occurenceRows (y :: rst) model)
    y :: [] ->
      [ occurencesRow y Nothing model ]
    [] ->
      []

dependenciesRow : SliceID -> Maybe SliceID -> Model -> Html Msg
dependenciesRow parent center model =
  div
    [ class "row" ]
    (case Dict.get parent model.cache of
      Nothing      ->
        [ viewError ("Missing Slice: " ++ parent) ]
      Just {slice} ->
        List.map (tryViewSlice model References) (reorder center (removeDuplicates (extractDependencies slice))))

occurencesRow : SliceID -> Maybe SliceID -> Model -> Html Msg
occurencesRow parent center model =
  div
    [ class "row" ]
    (case Dict.get parent model.cache of
      Nothing           ->
        [ viewError ("Missing Slice: " ++ parent) ]
      Just {occurences} ->
        List.map (tryViewSlice model (Occurences parent)) (reorder center (removeDuplicates occurences)))

removeDuplicates : List comparable -> List comparable
removeDuplicates list =
  Set.toList (Set.fromList list)

-- set foc as first element in list
reorder : Maybe SliceID -> List SliceID -> List SliceID
reorder foc list =
  case foc of
    Nothing -> list
    Just center ->
      center :: (List.filter (\s -> not (String.contains center s)) list)

-- view an error in the style of surrounding elements
viewError : String -> Html Msg
viewError err =
  p
    [ class "error" ]
    [ text err ]

-- | viewing a single Slice
type Highlight
  = Occurences SliceID
  | References
  | NoHighlight

tryViewSlice : Model -> Highlight -> SliceID -> Html Msg
tryViewSlice model highlight sid =
  case Dict.get sid model.cache of
    Nothing -> viewError ("Missing Slice: " ++ sid)
    Just sw ->
      let
        classIndex = case model.rows of
          Nothing -> Dict.empty
          Just r  -> generateClassIndex r
      in
        viewSlice classIndex highlight sw

generateClassIndex : Rows -> Dict SliceID (List String)
generateClassIndex {marked, focus, trace} =
  (focus, ["focus"])
  :: List.map (\sid -> (sid, ["trace"])) trace
  |> (\x -> case marked of
              Nothing -> x
              Just sid -> (sid, ["marked"]) :: x )
  |> Dict.fromList

renderFragment : Slice -> String
renderFragment slice =
  case slice of
    (Slice _ _ (Fragment codes) _ _) ->
      String.concat (List.intersperse "\n" codes)

viewSlice : Dict SliceID (List String) -> Highlight -> SliceWrap -> Html Msg
viewSlice classIndex highlight sw =
  let
    renderedFragment = renderFragment sw.slice
    classes =
      case Dict.get sw.id classIndex of
        Nothing   -> []
        Just strs -> List.map (\x -> (x, True)) strs
    highlightDict =
      case highlight of
        NoHighlight -> Dict.empty
        References ->
          List.map
            (Tuple.mapSecond
              (\mid ->
                [ class "reference"
                , onMouseEnter (Mark mid)
                , onMouseLeave Unmark
                ]
              )
            )
            (extractReferences sw.slice)
          |> Dict.fromList
        Occurences occId ->
          extractReferences sw.slice
          |> List.filter (\(_, a) -> String.contains occId a)
          |> List.map (Tuple.mapSecond (\_ -> [ class "occurence"]))
          |> Dict.fromList
  in
    div
      [ classList
          (( "elmsh", True ) :: classes)
      , onClick (Focus sw.id sw.occurences)
      ]
      [ toHtml
          renderedFragment
          highlightDict
      ]

viewTextarea : String -> Html Msg
viewTextarea codeStr =
    textarea
        [ value codeStr
        , classList
            [ ( "textarea", True )
            , ( "textarea-lc", False )
            ]
        -- , onInput (SetText thisLang)
        , spellcheck False
        , readonly True
        ]
        []

toHtml : String -> Dict String (List (Html.Attribute Msg)) -> Html Msg
toHtml code refs =
  SH.haskell code
    |> Result.map (highlightReferences refs)
    |> Result.map (SH.toBlockHtml Nothing)
    |> Result.mapError Parser.deadEndsToString
    |> (\result ->
            case result of
                Result.Ok a ->
                    a

                Result.Err x ->
                    text x
       )

highlightReferences : Dict String (List (Html.Attribute Msg)) -> SH.HCode Msg -> SH.HCode Msg
highlightReferences dict hcode =
  let
    highlightFragment frag =
      case Dict.get frag.text dict of
        Nothing -> frag
        Just attrs  ->
          { frag | additionalAttributes = attrs ++ frag.additionalAttributes }
  in
    mapHCodeFragments highlightFragment hcode

mapHCodeFragments : (SHL.Fragment msg -> SHL.Fragment msg) -> SH.HCode msg -> SH.HCode msg
mapHCodeFragments f hcode =
  case hcode of
    SH.HCode lines ->
      List.map
        (\l -> { l | fragments =
          List.map
            f
            l.fragments
          })
        lines
      |> SH.HCode
