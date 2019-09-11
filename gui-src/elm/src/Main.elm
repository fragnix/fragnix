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
  , root: Maybe Node
  , slices: List SliceWrap
  , cache: Dict SliceID SliceWrap
  , error: Maybe String
  }

type Node
  = N_Collapsed SliceWrap
  | N_Expanded SliceWrap Occurences Dependencies

type Occurences
  = O_Collapsed SliceID (List SliceWrap)
  | O_Expanded SliceID (List Node)

type Dependencies
  = D_Collapsed SliceID (List SliceWrap)
  | D_Expanded SliceID (List Node)

emptyModel : Model
emptyModel =
  { main = Nothing
  , root = Nothing
  , slices = []
  , cache = Dict.empty
  , error = Nothing
  }

-- | UPDATE

type Msg
  = ReceivedSlices (Result Http.Error (List Slice))
  | Error String
  | CloseError
  | Editor EditorAction

type EditorAction
  = ExpandNode SliceID
  | CollapseNode SliceID
  | ExpandOccurences SliceID
  | CollapseOccurences SliceID
  | ExpandDependencies SliceID
  | CollapseDependencies SliceID

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

    Editor action ->
      case model.root of
        Just r ->
          case nodeUpdate action model.cache r of
            Ok newRoot ->
              ( { model | root = Just newRoot }
              , Cmd.none
              )
            Err err ->
              ( { model | error = Just err }
              , Cmd.none
              )
        Nothing ->
          ( { model | error = Just "Received editor action but no editor" }
          , Cmd.none
          )

nodeUpdate: EditorAction -> Dict SliceID SliceWrap -> Node -> Result String Node
nodeUpdate action cache node =
  case node of
    N_Collapsed sw ->
      case action of
        ExpandNode sid ->
          if sw.id == sid then
            expandNode sw cache
          else
            Ok node
        _ ->
          Ok node
    N_Expanded sw occs deps ->
      case action of
        CollapseNode sid ->
          if sw.id == sid then
            Ok (N_Collapsed sw)
          else
            propagateNode action cache sw occs deps
        _ ->
          propagateNode action cache sw occs deps

propagateNode : EditorAction -> Dict SliceID SliceWrap -> SliceWrap -> Occurences -> Dependencies -> Result String Node
propagateNode action cache sw occs deps =
  case ( occurencesUpdate action cache occs, dependenciesUpdate action cache deps ) of
    (Err e1, Err e2) -> Err (e1 ++ e2)
    (Err e1, _     ) -> Err e1
    (_     , Err e2) -> Err e2
    (Ok occ, Ok dep) -> Ok (N_Expanded sw occ dep)

expandNode : SliceWrap -> Dict SliceID SliceWrap -> Result String Node
expandNode sw cache =
  let
    occs =
      List.map get sw.occurences
    deps =
      List.map get (extractDependencies sw.slice)
    occLength =
      List.length occs
    get sid =
      case Dict.get sid cache of
        Nothing -> Err ("Missing Slice: " ++ sid ++ " ")
        Just s  -> Ok s
  in
    case combineResults (occs ++ deps) of
      Err errs ->
        Err (String.concat (List.intersperse "," errs))
      Ok xs ->
        N_Expanded
          sw
          (O_Collapsed sw.id (List.take occLength xs))
          (D_Collapsed sw.id (List.drop occLength xs))
        |> Ok


occurencesUpdate: EditorAction -> Dict SliceID SliceWrap -> Occurences -> Result String Occurences
occurencesUpdate action cache occs =
  case occs of
    O_Collapsed sid sws ->
      case action of
        ExpandOccurences eid ->
          if eid == sid then
            Ok (O_Expanded sid (List.map N_Collapsed sws))
          else
            Ok occs
        _ ->
          Ok occs
    O_Expanded sid nodes ->
      case action of
        CollapseOccurences cid ->
          if cid == sid then
            Ok (O_Collapsed sid (List.map nodeToSliceWrap nodes))
          else
            propagateOccurences action cache sid nodes
        _ ->
          propagateOccurences action cache sid nodes

propagateOccurences : EditorAction -> Dict SliceID SliceWrap -> SliceID -> List Node -> Result String Occurences
propagateOccurences action cache sid nodes =
  case combineResults (List.map (nodeUpdate action cache) nodes) of
    Ok newNodes ->
      Ok (O_Expanded sid newNodes)
    Err errs    ->
      Err (String.concat (List.intersperse "," errs))

nodeToSliceWrap : Node -> SliceWrap
nodeToSliceWrap node =
  case node of
    N_Collapsed sw    -> sw
    N_Expanded sw _ _ -> sw

dependenciesUpdate: EditorAction -> Dict SliceID SliceWrap -> Dependencies -> Result String Dependencies
dependenciesUpdate action cache deps =
  case deps of
    D_Collapsed sid sws ->
      case action of
        ExpandDependencies eid ->
          if eid == sid then
            Ok (D_Expanded sid (List.map N_Collapsed sws))
          else
            Ok deps
        _ ->
          Ok deps
    D_Expanded sid nodes ->
      case action of
        CollapseDependencies cid ->
          if cid == sid then
            Ok (D_Collapsed sid (List.map nodeToSliceWrap nodes))
          else
            propagateDependencies action cache sid nodes
        _ ->
          propagateDependencies action cache sid nodes

propagateDependencies : EditorAction -> Dict SliceID SliceWrap -> SliceID -> List Node -> Result String Dependencies
propagateDependencies action cache sid nodes =
  case combineResults (List.map (nodeUpdate action cache) nodes) of
    Ok newNodes ->
      Ok (D_Expanded sid newNodes)
    Err errs    ->
      Err (String.concat (List.intersperse "," errs))

combineResults : List (Result a b) -> Result (List a) (List b)
combineResults =
  List.foldl
    (\x acc ->
      case acc of
        Err errs ->
          case x of
            Err err -> Err (err :: errs)
            _       -> acc
        Ok ress ->
          case x of
            Err err -> Err [err]
            Ok res  -> Ok (res :: ress))
      (Ok [])

-- | ReceivedSlices

loadSlices : List Slice -> Model -> Model
loadSlices slices model =
  insertSlices slices model
  |> indexSlices
  |> computeOccurences
  |> performIntegrityCheck
  |> findMain
  |> setRoot

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
setRoot : Model -> Model
setRoot model =
  let
    rsw =
      case model.main of
        Nothing ->
          List.head model.slices
        Just y  ->
          Dict.get y model.cache
  in
    case rsw of
      Nothing ->
        { model | error = Just "No slices found" }
      Just sw ->
        { model | root = Just (N_Collapsed sw) }

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
    ( case model.root of
        Just r   -> [ viewNode r ]
        Nothing  -> [ p [] [ text "No slice chosen." ] ]
    )

viewNode : Node -> Html Msg
viewNode n = text "dummy"

removeDuplicates : List comparable -> List comparable
removeDuplicates list =
  Set.toList (Set.fromList list)

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
      viewSlice highlight sw

renderFragment : Slice -> String
renderFragment slice =
  case slice of
    (Slice _ _ (Fragment codes) _ _) ->
      String.concat (List.intersperse "\n" codes)

viewSlice : Highlight -> SliceWrap -> Html Msg
viewSlice highlight sw =
  let
    renderedFragment = renderFragment sw.slice
    highlightDict =
      case highlight of
        NoHighlight -> Dict.empty
        References ->
          List.map
            (Tuple.mapSecond
              (\mid ->
                [ class "reference" ]
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
          [ ( "elmsh", True ) ]
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
