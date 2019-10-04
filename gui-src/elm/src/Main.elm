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
      walkSlices
        dirtyStep
        (insertAll new.occurences new.id Dict.empty)
        (Dict.insert old.id new cache)
        (Dict.insert old.id new Dict.empty)
    (ChangedFrom _ _, Disk) ->
      walkSlices
        unDirtyStep
        (insertAll new.occurences new.id Dict.empty)
        (Dict.insert old.id new cache)
        (Dict.insert old.id new Dict.empty)


-- | walk the slice graph
type alias Queue a = Dict SliceID (Pending a)
type alias Pending a = a
type alias Stepper a =
  (SliceWrap -> Pending a -> Queue a -> (SliceWrap, Queue a))

walkSlices : Stepper a -> Queue a -> Cache -> Cache -> (Cache, List (SliceID, SliceWrap))
walkSlices step queue cache updates =
  case Dict.keys queue of
    -- stop if queue is empty
    [] ->
      (cache, Dict.toList updates)
    -- else apply first step
    x :: _ ->
      case (Dict.get x cache, Dict.get x queue) of
        (Just sw, Just pending) ->
          case step sw pending (Dict.remove x queue) of
            (newSw, newQueue) ->
              walkSlices
                step
                newQueue
                (Dict.insert x newSw cache)
                (Dict.insert x newSw updates)
        -- if a key is not available, ignore pending step and move on
        _ ->
          walkSlices step (Dict.remove x queue) cache updates

-- | step needed for propagating that a slice that was unchanged is now changed
dirtyStep : Stepper (Set SliceID)
dirtyStep sw changedReferences queue =
  ( updateChangedReferences changedReferences sw
  , if sw.origin == Disk then
      insertAll sw.occurences sw.id queue
    else
      queue
  )

insertAll : List SliceID -> SliceID -> Queue (Set SliceID) -> Queue (Set SliceID)
insertAll places sid queue =
  List.foldl
    (\place newQueue ->
      insertOne place sid newQueue)
    queue
    places

insertOne : SliceID -> SliceID -> Queue (Set SliceID) -> Queue (Set SliceID)
insertOne key newValue queue =
  case Dict.get key queue of
    Nothing ->
      Dict.insert key (Set.insert newValue Set.empty) queue
    Just set ->
      Dict.insert key (Set.insert newValue set) queue

updateChangedReferences : (Set SliceID) -> SliceWrap -> SliceWrap
updateChangedReferences changed sw =
  let
    newChanges = (List.map Reference (Set.toList changed))
  in
    case sw.origin of
      Disk ->
        { sw | origin = ChangedFrom sw newChanges }
      ChangedFrom old existingChanges ->
        { sw | origin = ChangedFrom sw (existingChanges ++ newChanges) }

-- | step needed for propagating that a slice that was changed is now unchanged
unDirtyStep : Stepper (Set SliceID)
unDirtyStep sw resetReferences queue =
  case sw.origin of
    Disk ->
      (sw, queue)
    ChangedFrom old changes ->
      let
        newSw = updateResetReferences resetReferences changes old sw
      in
        ( newSw
        , if newSw.origin == Disk then
            insertAll newSw.occurences newSw.id queue
          else
            queue
        )

updateResetReferences : (Set SliceID) -> List Change -> SliceWrap -> SliceWrap -> SliceWrap
updateResetReferences toReset changes old sw =
  case (List.filter
          (\c -> case c of
            Reference id -> not (Set.member id toReset)
            _            -> True)
          changes) of
    [] -> { sw | origin = Disk }
    newChanges -> { sw | origin = ChangedFrom old newChanges}

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
  case integrityCheck model.cache model.slices of
    Ok _    -> model
    Err err -> { model | error = Just err }

missingSlicesToString : Set SliceID -> String
missingSlicesToString missing =
  "Missing Slices: "
  ++ String.concat (List.map (\x -> x ++ " ") (Set.toList missing))

integrityCheck : Cache -> List SliceWrap -> Result String ()
integrityCheck cache slices =
  List.foldl (checkDependencies cache) (Ok ()) slices
  |> Result.mapError missingSlicesToString

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
