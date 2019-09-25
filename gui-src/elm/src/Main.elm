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

-- | Global State
type alias Model =
  { main:  Maybe SliceID
  , page: Page
  , slices: List SliceWrap
  , cache: Cache
  , error: Maybe String
  }

type alias Cache = Dict SliceID SliceWrap

emptyModel : Model
emptyModel =
  { main = Nothing
  , page = Loading []
  , slices = []
  , cache = Dict.empty
  , error = Nothing
  }

type Page
  = Loading (List String)
  | TreeView Node


-- | Editor State
type alias Node =
  { hovered: Bool
  , marked: Bool
  , id: SliceID
  , children: Children
  , content: NodeContent
  , editable: Bool
  , framed: Bool
  }

type Children = Collapsed | Expanded (List Node)

defaultNode : Node
defaultNode =
  { hovered = False
  , marked = False
  , id = ""
  , children = Collapsed
  , content = Occurences []
  , editable = False
  , framed = False
  }

type NodeContent
 = SliceNode SliceWrap
 | Occurences (List SliceWrap)
 | Dependencies (List SliceWrap)

mapNode : (Node -> Node) -> Node -> Node
mapNode f node =
  case node.children of
    Collapsed ->
      f node
    Expanded nodes ->
      f { node | children = Expanded (List.map (mapNode f) nodes) }

foldNode : (Node -> a -> a) -> a -> Node -> a
foldNode f z node =
    case node.children of
      Collapsed ->
        f node z
      Expanded nodes ->
        let
          z2 =
            List.foldl
              (\x acc ->
                foldNode f acc x)
              z
              nodes
        in
          f node z2

-- | UPDATE

type Msg
  = ReceivedSlices (Result Http.Error (List Slice))
  | LoadingStep Step
  | Error String
  | CloseError
  | Editor EditorAction
  | Edit String SliceWrap
  | Nop

type alias EditorAction =
  { target: String
  , action: Action
  }

type Action
  = Expand
  | Collapse
  | Mark
  | Unmark
  | Hover
  | Unhover
  | MakeEditable
  | MakeStatic
  | Frame
  | Unframe

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
          ( { model | error = Just (httpErrorToString e) }
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

    Edit txt sw ->
      ( editUpdate txt sw model
      , Cmd.none
      )

    Editor action ->
      case model.page of
        TreeView r ->
          case nodeUpdate action model.cache r of
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

editUpdate : String -> SliceWrap -> Model -> Model
editUpdate txt sw model =
  changeText txt sw
  |> computeChangeKinds sw
  |> exchangeSliceWrap model sw

exchangeSliceWrap : Model -> SliceWrap -> SliceWrap -> Model
exchangeSliceWrap model target changed =
  let
    (newCache, updates) =
      updateCache target changed model.cache
    newPage =
      case model.page of
        TreeView node -> TreeView (updateNodeContents updates node)
        _ -> model.page
  in
    { model | cache = newCache, slices = Dict.values newCache, page = newPage }

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
              in
                dirtyRecursive
                  (tailQ ++ (List.map (\o -> (o, sw.id)) sw.occurences))
                  (Dict.insert sw.id newSw cache)
                  ((sw.id, newSw) :: updates)


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
              in
                unDirtyRecursive
                  (tailQ ++ queueAdditions)
                  (Dict.insert sw.id newSw cache)
                  ((sw.id, newSw) :: updates)




updateNodeContents : List (SliceID, SliceWrap) -> Node -> Node
updateNodeContents updates node =
  let
    dict = Dict.fromList updates
    updateSw sw =
      case Dict.get sw.id dict of
        Just newSw -> newSw
        Nothing -> sw
  in
    mapNode
      (\n -> case n.content of
        SliceNode sw ->
          { n | content = SliceNode (updateSw sw)}
        Occurences occs ->
          { n | content = Occurences (List.map updateSw occs) }
        Dependencies deps ->
          { n | content = Dependencies (List.map updateSw deps) }
      )
      node



-- | recursively updating the editor model
nodeUpdate: EditorAction -> Cache -> Node -> Result String Node
nodeUpdate action cache node =
  if String.startsWith node.id action.target then
    if node.id == action.target then
      case action.action of
        Mark ->
          Ok { node | marked = True }
        Unmark ->
          Ok { node | marked = False }
        Hover ->
          Ok { node | hovered = True }
        Unhover ->
          Ok { node | hovered = False }
        Expand ->
          expandNode cache node
        Collapse ->
          collapseNode node
        MakeEditable ->
          Ok { node | editable = True }
        MakeStatic ->
          Ok { node | editable = False }
        Frame ->
          Ok { node | framed = True }
        Unframe ->
          Ok { node | framed = False }
    else
      propagateUpdate action cache node
  else
    Ok node

propagateUpdate : EditorAction -> Cache -> Node -> Result String Node
propagateUpdate action cache node =
  case node.children of
    Collapsed ->
      Ok node
    Expanded cs ->
      case combineResults (List.map (nodeUpdate action cache) cs) of
        Ok newNodes ->
          Ok { node | children = Expanded newNodes }
        Err errs    ->
          Err (String.concat (List.intersperse "," errs))

collapseNode : Node -> Result String Node
collapseNode node =
  Ok { node | children = Collapsed }

-- | create the children of a node
expandNode : Cache -> Node -> Result String Node
expandNode cache node =
  if node.children /= Collapsed then
    Ok node
  else
    case node.content of
      SliceNode sw ->
        tupleCombineResults
          ( fetchMap cache sw.occurences
          , fetchMap cache (extractDependencies sw.slice)
          )
        |> Result.map (\(occs, deps) ->
          { node | children = Expanded
            [ { defaultNode |
                id = node.id ++ "occ"
                , content = Occurences occs
              }
            , { defaultNode |
                id = node.id ++ "dep"
                , content = Dependencies deps
              }
            ]
          })
      Occurences occs ->
        { node | children = Expanded
            (List.map
              (\sw ->
                { defaultNode |
                    id = node.id ++ sw.id
                    , content = SliceNode sw
                }
              )
              occs)
        } |> Ok
      Dependencies deps ->
        { node | children = Expanded
            (List.map
              (\sw ->
                { defaultNode |
                    id = node.id ++ sw.id
                    , content = SliceNode sw
                }
              )
              deps)
        } |> Ok

-- Load slicewrap from cache
fetch : Cache -> SliceID -> Result String SliceWrap
fetch cache sid =
  case Dict.get sid cache of
    Nothing -> Err ("Missing slice: " ++ sid)
    Just sw -> Ok sw

-- Load a bunch of slicewraps from cache
fetchMap : Cache -> List SliceID -> Result String (List SliceWrap)
fetchMap cache sids =
  List.map (fetch cache) sids
  |> combineResults
  |> Result.mapError (\x -> String.concat (List.intersperse "," x))


-- HELPERS for working with Results
tupleCombineResults : (Result String a, Result String b) -> Result String (a, b)
tupleCombineResults (x, y) =
  case (x, y) of
    (Err e1, Err e2) -> Err (e1 ++ ", " ++ e2)
    (Err e1, _     ) -> Err e1
    (_     , Err e2) -> Err e2
    (Ok  r1, Ok  r2) -> Ok (r1, r2)


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
            Ok res  -> Ok (ress ++ [res]) )
      (Ok [])

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
            OtherSlice sid -> checkDependency sid cache acc
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
          TreeView { defaultNode | content = SliceNode sw, id = sw.id}
        }

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
      viewEditor node
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
          [ (Html.text (monokai_colors_css ++ show_caret_css ++ reference_css)) ])
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
viewEditor : Node -> Element Msg
viewEditor node =
  Element.el
    [ Element.padding 10
    , Background.color monokai_black
    , Font.color monokai_white
    , Font.family [ Font.monospace ]
    , Font.size 16
    , Element.scrollbars
    , Element.width Element.fill
    , Element.height Element.fill
    ]
    (viewNode node)

-- | Recursively view the editor model
viewNode : Node -> Element Msg
viewNode node =
  case node.children of
    Collapsed ->
      viewCollapsedNode node
    Expanded children ->
      case node.content of
        SliceNode sw ->
          viewSliceNode sw node
        Occurences occs ->
          viewListNode children node
        Dependencies deps ->
          viewListNode children node

-- | Collapsed
viewCollapsedNode : Node -> Element Msg
viewCollapsedNode { marked, id, content } =
  let
    fontColor = case content of
      SliceNode _ ->
        []
      _ ->
        [ Font.color actual_black ]
  in
    Element.el
      ([ Events.onClick (Editor {target = id, action = Expand})
       , Element.pointer
       , Element.mouseOver [ Background.color monokai_grey ]
       ]
      ++ (if marked then [ Background.color monokai_grey ] else [])
      ++ fontColor)
      (viewTeaser content)

viewTeaser : NodeContent -> Element Msg
viewTeaser content =
  case content of
    SliceNode { tagline, origin } ->
      Element.row
        [ Element.spacing 0
        , Element.padding 0
        ]
        [ Element.el
            [ Font.color
                (if origin == Disk then actual_black else orange)
            ]
            (Element.text "⮟ ")
        , inlineSH tagline
        ]
    Occurences occs ->
      case String.fromInt (List.length occs) of
        l ->
          Element.text
            ("⮟ show " ++ l ++ " occurences")
    Dependencies deps ->
      case String.fromInt (List.length deps) of
        l ->
          Element.text
            ("⮟ show " ++ l ++ " dependencies")

-- Expanded - Slice
viewSliceNode : SliceWrap -> Node -> Element Msg
viewSliceNode sw { hovered, marked, id, children, editable, framed } =
  case children of
    Expanded (occs :: deps :: _) ->
      let
        (smallOccs, smallDeps) =
          if hovered || editable then
            ( if occs.children == Collapsed then viewIfNotEmpty occs else []
            , if deps.children == Collapsed then viewIfNotEmpty deps else []
            )
          else
            ( [], [] )

        (bigOccs, bigDeps) =
          ( if occs.children /= Collapsed then viewIfNotEmpty occs else []
          , if deps.children /= Collapsed then viewIfNotEmpty deps else []
          )

        viewIfNotEmpty n =
          if isEmptyNode n then [] else [ viewNode n ]

      in
        Element.el
          (frameIf framed)
          (viewCollapsable
            id
            (Element.column
              [ Element.spacing 8
              ]
              ( bigOccs ++
                [ Element.column
                    ((Element.spacing 8) :: (nodeAttributes hovered marked id))
                    ( smallOccs ++ [(viewSlice sw editable id)] ++ smallDeps )
                ]
                ++ bigDeps)))

    _ -> Element.text "Faulty SliceNode: Expanded but no children"

isEmptyNode : Node -> Bool
isEmptyNode { content } =
  case content of
    Occurences   [] -> True
    Dependencies [] -> True
    _               -> False

nodeAttributes : Bool -> Bool -> SliceID -> List (Element.Attribute Msg)
nodeAttributes hovered marked sid =
  [ Events.onMouseEnter (Editor {target = sid, action = Hover})
  , Events.onMouseLeave (Editor {target = sid, action = Unhover})
  ] ++ (if marked || hovered then [ Background.color monokai_grey ] else [])

viewSlice : SliceWrap -> Bool -> String -> Element Msg
viewSlice sw editable nodeId =
  let
    renderedFragment = renderFragment sw.slice
    highlightDict =
      List.map
        (Tuple.mapSecond
          (\mid ->
            [ onMouseEnter
                (Editor {target = nodeId ++ "dep" ++ mid, action = Mark})
            , onMouseLeave
                (Editor {target = nodeId ++ "dep" ++ mid, action = Unmark})
            , class "reference"
            ]
          )
        )
        (extractReferences sw.slice)
      |> Dict.fromList
    dirtyAttribs =
      if sw.origin == Disk then
        []
      else
        [ Border.widthEach { edges | left = 1 }
        , Border.color orange
        ]
  in
    if editable then
      Element.row
        dirtyAttribs
        [ Element.el
            [ Border.width 1
            , Border.color monokai_grey
            ]
            (editorField renderedFragment (\txt -> Edit txt sw) highlightDict)
        , Element.el
            [ Events.onClick (Editor {target = nodeId, action = MakeStatic})
            , Element.pointer
            , Element.mouseOver [ Background.color monokai_black ]
            , Font.size 32
            , Element.height Element.fill
            , Element.width Element.fill
            , Element.spaceEvenly
            , Element.paddingEach {edges | left = 5}
            ]
            (Element.text "✓")
        ]
    else
      Element.el
        ([ Events.onClick (Editor {target = nodeId, action = MakeEditable})
        ] ++ dirtyAttribs)
        (syntaxHighlight renderedFragment highlightDict)

-- | Expanded - Occurences/Dependencies

viewListNode : List Node -> Node -> Element Msg
viewListNode nodes { hovered, marked, framed, id } =
  Element.el
    (frameIf framed)
    (viewCollapsable
      id
      (Element.column
        [ Element.spacing 16 ]
        (List.map viewNode nodes)))

-- | Common helpers

viewCollapsable : SliceID -> Element Msg -> Element Msg
viewCollapsable sid content =
  Element.row
    [ Element.spacing 5 ]
    [ (Element.column
        [ Element.height Element.fill
        , Events.onClick (Editor {target = sid, action = Collapse})
        , Events.onMouseEnter (Editor {target = sid, action = Frame})
        , Events.onMouseLeave (Editor {target = sid, action = Unframe})
        , Element.pointer
        , Font.color actual_black
        , Element.mouseOver [ Background.color monokai_grey ]
        ]
        [ {- Element.el
            [ Element.alignTop ]
            ( Element.text "⮟" )
        ,-} Element.el
            [ Element.centerX
            , Element.width (Element.px 1)
            , Element.height Element.fill
            , Border.widthEach { bottom = 0, left = 0, right = 1, top = 0 }
            , Border.color actual_black
            ]
            Element.none
        , Element.el
            [ Element.alignBottom ]
            ( Element.text "⮝" )
        ]
      )
    , content
    ]


frameIf : Bool -> List (Element.Attribute Msg)
frameIf framed =
  if framed then
     [ Border.width 1, Border.color monokai_white ]
   else
     [ Border.width 1, Border.color monokai_black ]


-- | COLOR PALETTE
monokai_black = (Element.rgb255 35 36 31)
monokai_grey = (Element.rgb255 51 52 47)
monokai_white = (Element.rgb255 247 247 241)
actual_black = Element.rgb 0 0 0
glass = Element.rgba 0 0 0 0
orange = Element.rgb255 255 133 51
edges =
   { top = 0
   , right = 0
   , bottom = 0
   , left = 0
   }


-- | SYNTAX HIGHLIGHTING
monokai_colors_css =
  ".elmsh {color: #f8f8f2;}.elmsh-hl {background: #343434;}.elmsh-add {background: #003800;}.elmsh-del {background: #380000;}.elmsh-comm {color: #75715e;}.elmsh1 {color: #ae81ff;}.elmsh2 {color: #e6db74;}.elmsh3 {color: #f92672;}.elmsh4 {color: #66d9ef;}.elmsh5 {color: #a6e22e;}.elmsh6 {color: #ae81ff;}.elmsh7 {color: #fd971f;}.elmsh-elm-ts, .elmsh-js-dk, .elmsh-css-p {font-style: italic;color: #66d9ef;}.elmsh-js-ce {font-style: italic;color: #a6e22e;}.elmsh-css-ar-i {font-weight: bold;color: #f92672;}"
show_caret_css =
  "textarea {caret-color: white;}"
reference_css =
  ".reference {text-decoration: underline;}.reference:hover {cursor:pointer;}"


type alias HighlightDict = Dict String (List (Html.Attribute Msg))

-- create a syntaxhighlighted, shrinkwrapped element
syntaxHighlight : String -> HighlightDict -> Element Msg
syntaxHighlight txt dict =
  SH.haskell (if String.endsWith "\n" txt then txt ++ " " else txt)
    |> Result.map (SH.dictAddAttributes dict)
    |> Result.map SH.toInlineHtml
    {- |> Result.map (\h ->
        Html.div []
            [ Html.node "style" [] [ (Html.text monokai_colors_css) ]
            , h
            ] ) -}
    |> Result.map Element.html
    -- padding to the right is needed to prevent overlayed text from breaking too early
    |> Result.map (Element.el [ Element.paddingEach { edges | right = 3 } ])
    |> Result.mapError Parser.deadEndsToString
    |> (\result ->
            case result of
                Result.Ok a ->
                    a

                Result.Err x ->
                    Element.text x
       )

-- create inline HTML of code
inlineSH : String -> Element Msg
inlineSH txt =
  SH.haskell txt
  |> Result.map SH.toInlineHtml
  |> Result.map Element.html
  |> Result.mapError Parser.deadEndsToString
  |> (\result ->
        case result of
          Ok a  -> a
          Err x -> Element.text x)

-- create a syntaxhighlighted, shrinkwrapped textarea
editorField : String -> (String -> Msg) -> HighlightDict -> Element Msg
editorField txt onChange dict =
    Element.el
        [ Element.inFront (invisibleTextarea txt onChange)
        ]
        (syntaxHighlight txt dict)

invisibleTextarea : String -> (String -> Msg) -> Element Msg
invisibleTextarea txt onChange =
  Input.multiline
      [ Element.height Element.shrink
      , Element.width Element.shrink
      , Background.color glass
      , Font.color glass
      , Element.spacing 0
      , Element.padding 0
      , Border.rounded 0
      , Border.width 0
      , Border.glow glass 0.0
      ]
      { onChange = onChange
      , text = txt
      , placeholder = Nothing
      , label = Input.labelHidden (String.left 5 txt)
      , spellcheck = False
      }
