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

type alias Node =
  { hovered: Bool
  , marked: Bool
  , id: SliceID
  , children: Children
  , content: NodeContent
  , editable: Bool
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
  }

type NodeContent
 = SliceNode SliceWrap
 | Occurences (List SliceWrap)
 | Dependencies (List SliceWrap)

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
  | Nop

type EditorAction
  = Expand SliceID
  | Collapse SliceID
  | Mark SliceID
  | Unmark SliceID
  | Hover SliceID
  | Unhover SliceID
  | MakeEditable SliceID
  | MakeStatic SliceID

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

    Nop ->
      ( model, Cmd.none )

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
  case action of
    Mark sid ->
      if node.id == sid then
        Ok { node | marked = True }
      else
        propagateUpdate action cache node
    Unmark sid ->
      if node.id == sid then
        Ok { node | marked = False }
      else
        propagateUpdate action cache node
    Hover sid ->
      if node.id == sid then
        Ok { node | hovered = True }
      else
        propagateUpdate action cache node
    Unhover sid ->
      if node.id == sid then
        Ok { node | hovered = False }
      else
        propagateUpdate action cache node
    Expand sid ->
      if node.id == sid then
        expandNode cache node
      else
        propagateUpdate action cache node
    Collapse sid ->
      if node.id == sid then
        collapseNode node
      else
        propagateUpdate action cache node
    MakeEditable sid ->
      if node.id == sid then
        Ok { node | editable = True }
      else
        propagateUpdate action cache node
    MakeStatic sid ->
      if node.id == sid then
        Ok { node | editable = False }
      else
        propagateUpdate action cache node

-- end nodeUpdate

propagateUpdate : EditorAction -> Dict SliceID SliceWrap -> Node -> Result String Node
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

expandNode : Dict SliceID SliceWrap -> Node -> Result String Node
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
                id = sw.id ++ "occ"
                , content = Occurences occs
              }
            , { defaultNode |
                id = sw.id ++ "dep"
                , content = Dependencies deps
              }
            ]
          })
      Occurences occs ->
        { node | children = Expanded
            (List.map
              (\sw ->
                { defaultNode |
                    id = sw.id
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
                    id = sw.id
                    , content = SliceNode sw
                }
              )
              deps)
        } |> Ok

fetch : Dict SliceID SliceWrap -> SliceID -> Result String SliceWrap
fetch cache sid =
  case Dict.get sid cache of
    Nothing -> Err ("Missing slice: " ++ sid)
    Just sw -> Ok sw

fetchMap : Dict SliceID SliceWrap -> List SliceID -> Result String (List SliceWrap)
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
        { model | root =
          Just { defaultNode | content = SliceNode sw, id = sw.id}
        }

-- | VIEW
view : Model -> Html Msg
view model =
  (case model.error of
    Just err -> viewErrMsg err
    Nothing  -> viewEditor model)
  |> createHtml

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

options : List Element.Option
options =
  [ Element.focusStyle
      { borderColor = Nothing
      , backgroundColor = Nothing
      , shadow = Nothing
      }
  ]

viewErrMsg : String -> Element Msg
viewErrMsg err =
  Element.el
    [ Background.color monokai_black
    , Font.color monokai_white
    , Element.width Element.fill
    , Element.height Element.fill
    ]
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
            { onPress = Just CloseError, label = Element.text "Ignore and show editor"}
        ]
    )

viewEditor : Model -> Element Msg
viewEditor model =
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
    (case model.root of
      Just r -> viewNode r
      Nothing -> Element.text "No slice chosen.")

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

viewCollapsedNode : Node -> Element Msg
viewCollapsedNode { hovered, marked, id, content } =
  let
    dark =
      case content of
        SliceNode _ -> []
        _ ->
          [ Font.color actual_black ]
  in
    Element.el
      ([ Events.onClick (Editor (Expand id)) ]
      ++ (nodeAttributes hovered marked id) ++ dark)
      (viewTeaser content)

viewTeaser : NodeContent -> Element Msg
viewTeaser content =
  case content of
    SliceNode { tagline } ->
      Element.row
        [ Element.spacing 0
        , Element.padding 0
        ]
        [ Element.text "⮟ "
        , inlineSH tagline
        ]
    Occurences occs ->
      Element.text
        ("⮟ show " ++ (String.fromInt (List.length occs)) ++ " occurences")
    Dependencies deps ->
      Element.text
        ("⮟ show " ++ (String.fromInt (List.length deps)) ++ " dependencies")

viewSliceNode : SliceWrap -> Node -> Element Msg
viewSliceNode sw { hovered, marked, id, children, editable } =
  case children of
    Expanded (occs :: deps :: _) ->
      let
        occsOrNothing =
          if occs.children == Collapsed && not hovered && not editable then
            []
          else
            [ viewNode occs ]
        depsOrNothing =
          if deps.children == Collapsed && not hovered && not editable then
            []
          else
            [ viewNode deps ]
        (colAttribs, nodeAttribs) =
          if hovered && occs.children == Collapsed && deps.children == Collapsed then
            ((nodeAttributes hovered marked id), [])
          else
            ([], (nodeAttributes hovered marked id))
      in
        (viewCollapsable
          id
          (Element.column
            ([ Element.width Element.fill
            , Element.spacing 5
            ] ++ colAttribs)
            ( occsOrNothing
            ++ [ Element.el
                  nodeAttribs
                  (viewSlice sw editable) ]
            ++ depsOrNothing)))
    _ -> Element.text "Faulty SliceNode: Expanded but no children"

viewCollapsable : SliceID -> Element Msg -> Element Msg
viewCollapsable sid content =
  Element.row
    []
    [ (Element.column
        [ Element.height Element.fill
        , Events.onClick (Editor (Collapse sid))
        , Font.color actual_black
        , Element.padding 5
        ]
        [ Element.el
            [ Element.alignTop ]
            ( Element.text "⮟" )
        , Element.el
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

viewListNode : List Node -> Node -> Element Msg
viewListNode nodes { hovered, marked, id } =
  Element.el
    []
    (viewCollapsable
      id
      (Element.column
        [ Element.spacing 5 ]
        (List.map viewNode nodes)))


nodeAttributes : Bool -> Bool -> SliceID -> List (Element.Attribute Msg)
nodeAttributes hovered marked sid =
  [ Events.onMouseEnter (Editor (Hover sid))
  , Events.onMouseLeave (Editor (Unhover sid))
  ] ++ (if marked || hovered then [ Background.color monokai_grey ] else [])

renderFragment : Slice -> String
renderFragment slice =
  case slice of
    (Slice _ _ (Fragment codes) _ _) ->
      String.concat (List.intersperse "\n" codes)

viewSlice : SliceWrap -> Bool -> Element Msg
viewSlice sw editable =
  let
    renderedFragment = renderFragment sw.slice
    highlightDict =
      List.map
        (Tuple.mapSecond
          (\mid ->
            [ onMouseEnter (Editor (Mark mid))
            , onMouseLeave (Editor (Unmark mid))
            , onClick (Editor (Expand mid))
            , class "reference"
            ]
          )
        )
        (extractReferences sw.slice)
      |> Dict.fromList
  in
    if editable then
      editorField renderedFragment (\_ -> Nop) highlightDict
    else
      Element.el
        [ Events.onClick (Editor (MakeEditable sw.id)) ]
        (syntaxHighlight renderedFragment highlightDict)

-- monokai background color
monokai_black = (Element.rgb255 35 36 31)
monokai_grey = (Element.rgb255 51 52 47)
monokai_white = (Element.rgb255 247 247 241)
actual_black = Element.rgb 0 0 0
monokai_colors_css =
  ".elmsh {color: #f8f8f2;}.elmsh-hl {background: #343434;}.elmsh-add {background: #003800;}.elmsh-del {background: #380000;}.elmsh-comm {color: #75715e;}.elmsh1 {color: #ae81ff;}.elmsh2 {color: #e6db74;}.elmsh3 {color: #f92672;}.elmsh4 {color: #66d9ef;}.elmsh5 {color: #a6e22e;}.elmsh6 {color: #ae81ff;}.elmsh7 {color: #fd971f;}.elmsh-elm-ts, .elmsh-js-dk, .elmsh-css-p {font-style: italic;color: #66d9ef;}.elmsh-js-ce {font-style: italic;color: #a6e22e;}.elmsh-css-ar-i {font-weight: bold;color: #f92672;}"
{-
monokai_colors_css =
    ".elmsh {color: #f8f8f2;background: #23241f;}.elmsh-hl {background: #343434;}.elmsh-add {background: #003800;}.elmsh-del {background: #380000;}.elmsh-comm {color: #75715e;}.elmsh1 {color: #ae81ff;}.elmsh2 {color: #e6db74;}.elmsh3 {color: #f92672;}.elmsh4 {color: #66d9ef;}.elmsh5 {color: #a6e22e;}.elmsh6 {color: #ae81ff;}.elmsh7 {color: #fd971f;}.elmsh-elm-ts, .elmsh-js-dk, .elmsh-css-p {font-style: italic;color: #66d9ef;}.elmsh-js-ce {font-style: italic;color: #a6e22e;}.elmsh-css-ar-i {font-weight: bold;color: #f92672;}"
-}

show_caret_css =
  "textarea {caret-color: white;}"
reference_css =
  ".reference {text-decoration: underline;}"

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
    |> Result.map (Element.el [] )
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
        [ Element.behindContent (syntaxHighlight txt dict)
        ]
        (Input.multiline
            [ Element.height Element.shrink
            , Element.width Element.shrink
            , Background.color (Element.rgba 0 0 0 0)
            , Font.color (Element.rgba 0 0 0 0)
            , Element.spacing 0
            , Element.padding 0
            , Border.width 0
            , Border.rounded 0
            , Border.glow (Element.rgba 0 0 0 0) 0.0
            ]
            { onChange = onChange
            , text = txt
            , placeholder = Nothing
            , label = Input.labelHidden (String.left 5 txt)
            , spellcheck = False
            })
