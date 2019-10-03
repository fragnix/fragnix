module Main exposing (..)

import Browser

import Http
import Json.Encode as E
import Dict exposing (Dict)
import Set exposing (Set)

import Slice exposing (..)
import LocalSlice exposing (..)
import Palette exposing (..)
import Editor
-- because of the css:
import EditorField
import API

-- imports for view
import Html exposing (Html)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Events as Events

import Cmd.Extra exposing (perform)

main =
  Browser.element { init = init, update = update, view = view, subscriptions = (\_ -> Sub.none) }

-- | INIT
type alias Flags = E.Value


init : Flags -> (Model, Cmd Msg)
init _ =
  ( { emptyModel | page = Loading ["Requesting Slices..."] }
  , (API.getAllSlices ReceivedSlices)
  )

-- | MODEL

-- | Global State
type alias Model =
  { main:  Maybe SliceID
  , page: Page
  , slices: List SliceWrap
  , cache: Cache
  , error: Maybe String
  , saving: Bool
  }

type alias Cache = Dict SliceID SliceWrap

emptyModel : Model
emptyModel =
  { main = Nothing
  , page = Loading []
  , slices = []
  , cache = Dict.empty
  , error = Nothing
  , saving = False
  }

type alias SliceData =
  { slices : List Slice
  , cache : Cache
  , main : Maybe SliceID
  }

type Page
  = Loading (List String)
  | TreeView Editor.Node

-- | UPDATE

type Msg
  = ReceivedSlices (Result Http.Error (List Slice))
  | HashedSlices (Result Http.Error (API.UpdateMap, List Slice))
  | CompileMsg (Result Http.Error String)
  | LoadingStep Step
  | Error String
  | CloseError
  | Nop
  | Save
  | Compile SliceID
  | Editor Editor.Msg

type Step
  = IndexSlices (List Slice)
  | ComputeOccurences
  | CheckIntegrity
  | FindMain

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceivedSlices result ->
      case result of
        Err e ->
          ( { model | error = Just (API.httpErrorToString e) }
          , Cmd.none
          )
        Ok slices ->
          case model.page of
            Loading msgs ->
              ( { model | page = Loading (msgs ++ ["Received Slices. Indexing..."]) },
                perform (LoadingStep (IndexSlices slices))
              )
            _ ->
              ( { model | error = Just "Received Slices but was not in a loading state."}
              , Cmd.none
              )

    LoadingStep step ->
      loadingUpdate step model

    Error err ->
      ( { model | error = Just err }
      , Cmd.none
      )

    CloseError ->
      ( { model | error = Nothing }
      , Cmd.none
      )

    Nop ->
      ( model, Cmd.none )

    Editor editorMsg ->
      case editorMsg of
        Editor.Main mainAction ->
          case mainAction of
            Editor.Edit txt sw ->
              ( editUpdate txt sw model
              , Cmd.none
              )
        Editor.Editor editorAction ->
          case model.page of
            TreeView r ->
              case Editor.nodeUpdate editorAction model.cache r of
                Ok newRoot ->
                  ( { model | page = TreeView newRoot }
                  , Cmd.none
                  )
                Err err ->
                  ( { model | error = Just err }
                  , Cmd.none
                  )
            _ ->
              ( { model | error = Just "How the heck did I get here? I was asked to manipulate a TreeView Page but it does not seem to exist." }
              , Cmd.none
              )

    Save ->
      case computeLocalSlices model.slices of
        (obsoletes, localSlices) ->
          ( { model | saving = True }
          , API.saveSlices HashedSlices obsoletes localSlices)

    HashedSlices result ->
      case result of
        Err e ->
          ( { model | error = Just (API.httpErrorToString e) }
          , Cmd.none
          )
        Ok (updateMap, newSlices) ->
          ( integrateHashedSlices
              updateMap
              newSlices
              { model | saving = False }
          , Cmd.none
          )

    _ -> (model, Cmd.none) -- TODO

-- | integrate newly hashed slices
integrateHashedSlices : API.UpdateMap -> List Slice -> Model -> Model
integrateHashedSlices umap slices model =
  let
    sliceWraps = List.map wrap slices

    updateMap =
      List.map (Tuple.mapFirst (\(LocalSliceID sid) -> sid)) umap

    newCache =
      dictRemoveList (List.map Tuple.first updateMap) model.cache
      |> insertSliceWraps sliceWraps
      |> Dict.map
          (\k sw -> { sw | occurences = [] } )

    newModel =
      computeOccurences
        { model | cache = newCache, slices = Dict.values newCache }

    nodeUpdates =
      List.filterMap
        (\(old, sid) ->
            Dict.get sid newModel.cache
            |> Maybe.map (\sw -> (old, sw)))
        updateMap

    newPage =
      case newModel.page of
        TreeView node -> TreeView (Editor.updateNodeContents nodeUpdates node)
        _ -> newModel.page
  in
    { newModel | page = newPage }

dictRemoveList : List comparable -> Dict comparable a -> Dict comparable a
dictRemoveList list dict =
  List.foldl
    (\k acc ->
      Dict.remove k acc)
    dict
    list

insertSliceWraps : List SliceWrap -> Cache -> Cache
insertSliceWraps sws cache =
  List.foldl
    (\sw acc ->
      Dict.insert sw.id sw acc)
    cache
    sws

-- | compute which slices are obsolete and which are to be rehashed
computeLocalSlices : List SliceWrap -> (List SliceID, List LocalSlice)
computeLocalSlices slices =
  ( List.filterMap
      (\sw -> case sw.origin of
          Disk -> Nothing
          _    -> Just sw.id)
      slices
  , List.filterMap toLocalSlice slices
  )

-- | Change the text contained in a slice and update the model accordingly
editUpdate : String -> SliceWrap -> Model -> Model
editUpdate txt sw model =
  changeText txt sw
  |> computeChangeKinds sw
  |> exchangeSliceWrap model sw

-- | Change a SliceWrap and update the model accordingly
exchangeSliceWrap : Model -> SliceWrap -> SliceWrap -> Model
exchangeSliceWrap model target changed =
  let
    (newCache, updates) =
      updateCache target changed model.cache
    newPage =
      case model.page of
        TreeView node -> TreeView (Editor.updateNodeContents updates node)
        _ -> model.page
  in
    { model | cache = newCache, slices = Dict.values newCache, page = newPage }

-- | Update a SliceWrap in the Cache, propagate the changes and give back a
--   List of SliceWraps that have changed
updateCache : SliceWrap -> SliceWrap -> Cache -> (Cache, List (SliceID, SliceWrap))
updateCache old new cache =
  case (old.origin, new.origin) of
    (Disk, Disk)                   ->
      (cache, []) -- should not happen!
    (ChangedFrom _ _, ChangedFrom _ _) ->
      (Dict.insert old.id new cache, [(old.id, new)])
    (Disk, ChangedFrom _ _) ->
      dirtyRecursive
        (List.map (\o -> (o, new.id)) new.occurences)
        (Dict.insert old.id new cache)
        [(old.id, new)]
    (ChangedFrom _ _, Disk) ->
      unDirtyRecursive
        (List.map (\o -> (o, new.id)) new.occurences)
        (Dict.insert old.id new cache)
        [(old.id, new)]

-- | Propagate that a SliceWrap has changed to all its occurences and give back
--   a list of all SliceWraps that have changed
dirtyRecursive : List (SliceID, SliceID) -> Cache -> List (SliceID, SliceWrap) -> (Cache, List (SliceID, SliceWrap))
dirtyRecursive queue cache updates =
  case queue of
    [] ->
      (cache, updates)
    (headQ, changed) :: tailQ ->
      case Dict.get headQ cache of
        Nothing ->
          dirtyRecursive tailQ cache updates -- Silent failure
        Just sw ->
          let
            (changes, oldSw) = case sw.origin of
              Disk ->
                ([], sw)
              ChangedFrom ex cs ->
                (cs, ex)

            alreadyDirty =
              (List.length
                (List.filter
                  (\c ->
                    case c of
                      Reference id -> id == changed
                      _ -> False)
                  changes)) > 0
          in
            if alreadyDirty then
              dirtyRecursive tailQ cache updates
            else
              let
                newSw =
                  { sw | origin = ChangedFrom oldSw
                      ((Reference changed) :: changes)}
                newUpdates =
                  List.filter
                    (\(old, _) -> old /= sw.id)
                    updates
              in
                dirtyRecursive
                  (tailQ ++ (List.map (\o -> (o, sw.id)) sw.occurences))
                  (Dict.insert sw.id newSw cache)
                  ((sw.id, newSw) :: newUpdates)

-- | Propagate that a SliceWrap has reverted back to its initial state
--   to all its occurences and give back
--   a list of all SliceWraps that have changed
unDirtyRecursive : List (SliceID, SliceID) -> Cache -> List (SliceID, SliceWrap) -> (Cache, List (SliceID, SliceWrap))
unDirtyRecursive queue cache updates =
  case queue of
    [] ->
      (cache, updates)
    (headQ, nowClean) :: tailQ ->
      case Dict.get headQ cache of
        Nothing ->
          unDirtyRecursive tailQ cache updates -- Silent failure
        Just sw ->
          let
            (changes, oldSw) = case sw.origin of
              Disk ->
                ([], sw)
              ChangedFrom ex cs ->
                (cs, ex)

            alreadyClean =
              (List.length
                (List.filter
                  isObsoleteChange
                  changes)) == 0

            isObsoleteChange c =
              case c of
                Reference id -> id == nowClean
                _ -> False
          in
            if alreadyClean then
              unDirtyRecursive tailQ cache updates
            else
              let
                newChanges =
                  List.filter (\c -> not (isObsoleteChange c)) changes
                (newSw, queueAdditions) =
                  case newChanges of
                    [] ->
                      ( { sw | origin = Disk }
                      , (List.map (\o -> (o, sw.id)) sw.occurences)
                      )
                    cs ->
                      ( { sw | origin = ChangedFrom oldSw cs }
                      , []
                      )
                newUpdates =
                  List.filter
                    (\(old, _) -> old /= sw.id)
                    updates
              in
                unDirtyRecursive
                  (tailQ ++ queueAdditions)
                  (Dict.insert sw.id newSw cache)
                  ((sw.id, newSw) :: newUpdates)

-- | ReceivedSlices

loadingUpdate : Step -> Model -> (Model, Cmd Msg)
loadingUpdate step model =
  case model.page of
    Loading msgs ->
      case step of
        IndexSlices slices ->
          ( { model | page = Loading (msgs ++ ["Computing Occurences..."]) }
            |> insertSlices slices
            |> indexSlices
          , perform (LoadingStep ComputeOccurences)
          )
        ComputeOccurences ->
          ( { model | page = Loading (msgs ++ ["Performing Integrity Check..."]) }
            |> computeOccurences
          , perform (LoadingStep CheckIntegrity)
          )
        CheckIntegrity ->
          ( { model | page = Loading (msgs ++ ["Looking for main..."]) }
            |> performIntegrityCheck
          , perform (LoadingStep FindMain)
          )
        FindMain ->
          ( model
            |> findMain
            |> setRoot
          , Cmd.none
          )
    _ ->
      ( { model | error = Just "Trying to load slices but not in a loading state"}
      , Cmd.none
      )

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
      insertSliceWraps model.slices model.cache
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

addOccurences : SliceWrap -> (Cache) -> (Cache)
addOccurences { id, slice } dict =
  List.foldl
    (\sid acc -> addOccurence sid id acc)
    dict
    (extractDependencies slice)

addOccurence : SliceID -> SliceID -> (Cache) -> (Cache)
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

checkDependencies : (Cache) -> SliceWrap -> Result (Set SliceID) () -> Result (Set SliceID) ()
checkDependencies cache sw res =
  case sw.slice of
    (Slice _ _ _ uses _) ->
      List.foldl
        (\u acc -> case u of
          (Use _ _ ref) -> case ref of
            Slice.OtherSlice sid -> checkDependency sid cache acc
            _              -> acc)
        res
        uses

checkDependency : SliceID -> (Cache) -> Result (Set SliceID) () -> Result (Set SliceID) ()
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
  "main" == sw.name

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
        { model | page =
          TreeView { defaultNode | content = (Editor.SliceNode sw), id = sw.id}
        }

defaultNode = Editor.defaultNode

-- | VIEW
view : Model -> Html Msg
view model =
  (case model.error of
    Just err -> viewErrMsg err
    Nothing  -> viewPage model)
  |> createHtml

viewPage : Model -> Element Msg
viewPage { page, saving } =
  case page of
    TreeView node ->
      viewEditor node saving
    Loading msgs ->
      viewLoading msgs

-- | Layout element and add the inescapable css
createHtml : Element Msg -> Html Msg
createHtml el =
  Element.row
    [ Element.width Element.fill
    , Element.height Element.fill
    ]
    [ Element.html
        (Html.node
          "style"
          []
          [ (Html.text (EditorField.css)) ])
    , el
    ]
  |> Element.layoutWith { options = options } []

-- | prevent any default focus styling from interfering
options : List Element.Option
options =
  [ Element.focusStyle
      { borderColor = Nothing
      , backgroundColor = Nothing
      , shadow = Nothing
      }
  ]

-- | If something went fatally wrong
viewErrMsg : String -> Element Msg
viewErrMsg err =
  basicLayout
    ( Element.column
        [ Element.padding 10
        , Element.spacing 7
        , Element.centerX
        , Element.centerY
        ]
        [ Element.el
            [ Font.color (Element.rgb255 255 40 40) ]
            (Element.text err)
        , Input.button
            [ Font.color (Element.rgb 1 1 1) ]
            { onPress = Just CloseError, label = Element.text "Ignore" }
        ]
    )

-- | Slightly nicer loading screen
viewLoading : List String -> Element Msg
viewLoading msgs =
  basicLayout
    ( Element.column
        [ Element.padding 10
        , Element.spacing 7
        ]
        (List.map Element.text msgs)
    )

basicLayout : Element Msg -> Element Msg
basicLayout elem =
  Element.el
    [ Background.color monokai_black
    , Font.color monokai_white
    , Element.width Element.fill
    , Element.height Element.fill
    ]
    elem

-- | View the editor
viewEditor : Editor.Node -> Bool -> Element Msg
viewEditor node loading =
  Element.column
    [ Element.padding 10
    , Background.color monokai_black
    , Font.color monokai_white
    , Font.family [ Font.monospace ]
    , Font.size 16
    , Element.width Element.fill
    , Element.height Element.fill
    ]
    [ viewToolbar loading
    , Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.scrollbars
        ]
        (Element.map Editor (Editor.viewNode node))
    ]

viewToolbar : Bool -> Element Msg
viewToolbar saving =
  Element.row
    [ Element.width Element.fill
    ]
    [ Element.el
        [ Element.padding 10
        , Events.onClick Save
        , Element.mouseOver [Background.color monokai_grey ]
        , Element.pointer
        , Element.alignRight
        ]
        (Element.text (if saving then "Saving..." else "Save"))
    ]
