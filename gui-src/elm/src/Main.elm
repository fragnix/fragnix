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

import Process
import Task

-- | emit a Command that is handled asynchronously, i.e. the view is
--   updated first
--   (with all other implementations from packages, the view is not
--    updated before the msg is processed)
--   for an example, see https://ellie-app.com/6PHMfMxtFgpa1
perform : (a -> Msg) -> a -> Cmd Msg
perform toMsg content =
  Process.sleep 1
    |> Task.andThen (\_ -> Task.succeed content)
    |> Task.perform toMsg



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
  , changed: Set SliceID
  , transitive: Set SliceID
  , error: Maybe String
  , saving: Bool
  , compiling: Bool
  , compileMsg: Maybe String
  }

type alias Cache = Dict SliceID SliceWrap

emptyModel : Model
emptyModel =
  { main = Nothing
  , page = Loading []
  , slices = []
  , cache = Dict.empty
  , changed = Set.empty
  , transitive = Set.empty
  , error = Nothing
  , saving = False
  , compileMsg = Nothing
  , compiling = False
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
  | Compile
  | Editor Editor.Msg
  | Edit String SliceWrap
  | AddTransitiveChanges SliceID
  | RemoveTransitiveChanges SliceID
  | ClearCompileMsg

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
                perform (\x -> (LoadingStep (IndexSlices x))) slices
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

    Edit txt sw ->
      editUpdate txt sw model

    Editor editorMsg ->
      case editorMsg of
        Editor.Main mainAction ->
          case mainAction of
            Editor.Edit txt sw ->
              editUpdate txt sw model

        Editor.Editor editorAction ->
          case model.page of
            TreeView r ->
              case Editor.nodeUpdate editorAction model.cache model.transitive r of
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
      case computeLocalSlices model.slices model.cache of
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

    AddTransitiveChanges sid ->
      let
        newTransitive =
          Set.union
            model.transitive
            (computeChangedSlicesRec model.cache [sid] Set.empty)
        newPage =
          case model.page of
            TreeView root ->
              TreeView (Editor.updateChanged newTransitive root)
            p -> p
      in
        ({ model | page = newPage, transitive = newTransitive, changed = Set.insert sid model.changed }
        , Cmd.none
        )

    RemoveTransitiveChanges sid ->
      let
        newChanged =
          Set.remove sid model.changed
        newTransitive =
          computeChangedSlicesRec
            model.cache
            (Set.toList newChanged)
            Set.empty
        newPage =
          case model.page of
            TreeView root ->
              TreeView (Editor.updateChanged newTransitive root)
            p -> p
      in
        ({ model | page = newPage, transitive = newTransitive, changed = newChanged }
        , Cmd.none
        )

    Compile ->
      case model.main of
        Just sid ->
          ( { model | compiling = True }
          , API.compile CompileMsg sid
          )
        Nothing ->
          ( { model | compileMsg = Just "No main symbol found" }
          , Cmd.none
          )

    CompileMsg result ->
      case result of
        Err e ->
          ( { model | error = Just (API.httpErrorToString e) }
          , Cmd.none
          )
        Ok compileMsg ->
          ( { model | compileMsg = Just compileMsg, compiling = False }
          , Cmd.none
          )

    ClearCompileMsg ->
      ( { model | compileMsg = Nothing }
      , Cmd.none
      )

-- | integrate newly hashed slices
integrateHashedSlices : API.UpdateMap -> List Slice -> Model -> Model
integrateHashedSlices umap slices model =
  let
    sliceWraps = List.map wrap slices

    updateMap =
      List.map (Tuple.mapFirst (\(LocalSliceID sid) -> sid)) umap

    newMain =
      case model.main of
        Nothing  -> Nothing
        Just sid ->
          case Dict.get sid (Dict.fromList updateMap) of
            Nothing     -> Just sid
            Just newSid -> Just newSid

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
        TreeView node -> TreeView
          (Editor.mapNode
            (\n -> {n | changed = False})
            (Editor.updateNodeContents nodeUpdates node))
        _ -> newModel.page
  in
    performIntegrityCheck
      { newModel | page = newPage, changed = Set.empty, transitive = Set.empty, main = newMain }

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

-- | Change the text contained in a slice and update the model accordingly
editUpdate : String -> SliceWrap -> Model -> (Model, Cmd Msg)
editUpdate txt sw model =
  let
    newSw =
      changeText txt sw
      |> computeChangeKinds sw
  in
    ( exchangeSliceWrap model sw newSw
    , case (sw.origin, newSw.origin) of
        (Disk, ChangedFrom _ _) ->
          perform AddTransitiveChanges newSw.id
        (ChangedFrom _ _, Disk) ->
          perform RemoveTransitiveChanges newSw.id
        _ ->
          Cmd.none
    )

-- | Change a SliceWrap and update the model accordingly
exchangeSliceWrap : Model -> SliceWrap -> SliceWrap -> Model
exchangeSliceWrap model target changed =
  let
    newCache =
      Dict.insert target.id changed model.cache
    updates =
      [(target.id, changed)]
    newPage =
      case model.page of
        TreeView node -> TreeView (Editor.updateNodeContents updates node)
        _ -> model.page
  in
    { model | cache = newCache, slices = Dict.values newCache, page = newPage }

computeChangedSlices : List SliceWrap -> Cache -> Set SliceID
computeChangedSlices slices cache =
  computeChangedSlicesRec
    cache
    (List.filterMap
      (\sw -> if sw.origin /= Disk then Just sw.id else Nothing)
      slices)
    Set.empty

computeChangedSlicesRec : Cache -> List SliceID -> Set SliceID -> Set SliceID
computeChangedSlicesRec cache queue visited =
  case queue of
    [] -> visited
    x :: rst ->
      if Set.member x visited then
        computeChangedSlicesRec cache rst visited
      else
        case Dict.get x cache of
          Nothing ->
            computeChangedSlicesRec cache rst visited
          Just {occurences} ->
            computeChangedSlicesRec
              cache
              (rst ++ occurences)
              (Set.insert x visited)

-- | compute which slices are obsolete and which are to be rehashed
computeLocalSlices : List SliceWrap -> Cache -> (List SliceID, List LocalSlice)
computeLocalSlices slices cache =
  let
    newCache =
      walkSlices
        dirtyStep
        initQueue
        cache

    initQueue =
      List.foldl
        (\x acc ->
          insertAll x.occurences x.id acc)
        Dict.empty
        changedSlices

    changedSlices =
      List.filter (\sw -> sw.origin /= Disk) slices

    newSlices =
      Dict.values newCache
  in
    ( List.filterMap
        (\sw -> if hasChanged sw then Just sw.id else Nothing)
        newSlices
    , List.filterMap toLocalSlice newSlices
    )

-- | walk the slice graph
type alias Queue a = Dict SliceID (Pending a)
type alias Pending a = a
type alias Stepper a =
  (SliceWrap -> Pending a -> Queue a -> (SliceWrap, Queue a))

walkSlices : Stepper a -> Queue a -> Cache -> Cache
walkSlices step queue cache =
  case Dict.keys queue of
    -- stop if queue is empty
    [] ->
      cache
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
        -- if a key is not available, ignore pending step and move on
        _ ->
          walkSlices step (Dict.remove x queue) cache

-- | step needed for propagating that a slice that was unchanged is now changed
dirtyStep : Stepper (Set SliceID)
dirtyStep sw changedReferences queue =
  ( { sw | locals = Set.union changedReferences sw.locals }
  , if (not (hasChanged sw)) then
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
          , perform LoadingStep ComputeOccurences
          )
        ComputeOccurences ->
          ( { model | page = Loading (msgs ++ ["Performing Integrity Check..."]) }
            |> computeOccurences
          , perform LoadingStep CheckIntegrity
          )
        CheckIntegrity ->
          ( { model | page = Loading (msgs ++ ["Looking for main..."]) }
            |> performIntegrityCheck
          , perform LoadingStep FindMain
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
  List.member "main" sw.names

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
viewPage model =
  case model.page of
    TreeView node ->
      viewEditor node model
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
viewEditor : Editor.Node -> Model -> Element Msg
viewEditor node { saving, compiling, compileMsg } =
  Element.column
    [ Element.padding 10
    , Element.spacing 10
    , Background.color monokai_black
    , Font.color monokai_white
    , Font.family [ Font.monospace ]
    , Font.size 16
    , Element.width Element.fill
    , Element.height Element.fill
    ]
    [ Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.scrollbars
        ]
        (Element.map Editor (Editor.viewNode node))
    , viewToolbar saving compiling
    , viewCompileMsg compileMsg
    ]

viewToolbar : Bool -> Bool -> Element Msg
viewToolbar saving compiling =
  Element.row
    [ Element.width Element.fill
    , Border.widthEach { edges | bottom = 1 }
    , Border.color monokai_grey
    ]
    [ Element.el
        [ Element.padding 10
        , Events.onClick Save
        , Element.mouseOver [Background.color monokai_grey ]
        , Element.pointer
        , Element.alignRight
        ]
        (Element.text (if saving then "Saving..." else "Save"))
    , Element.el
        [ Element.padding 10
        , Events.onClick Compile
        , Element.mouseOver [Background.color monokai_grey ]
        , Element.pointer
        , Element.alignRight
        ]
        (Element.text (if compiling then "Compiling..." else "Compile"))
    ]

viewCompileMsg : Maybe String -> Element Msg
viewCompileMsg maybeMsg =
  case maybeMsg of
    Nothing -> Element.none
    Just msg ->
      Element.row
        [ Element.width Element.fill
        , Element.height (Element.maximum 300 Element.shrink)
        , Element.spacing 10
        , Element.scrollbars
        ]
        [ Element.el
            [ Element.padding 10
            , Events.onClick ClearCompileMsg
            , Element.mouseOver [Background.color monokai_grey ]
            , Element.pointer
            , Border.color monokai_grey
            , Border.widthEach { edges | right = 1 }
            , Font.color orange
            ]
            (Element.text "Close")
        , (Element.text msg)
        ]
