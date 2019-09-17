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
  }

type Children = Collapsed | Expanded (List Node)

defaultNode : Node
defaultNode =
  { hovered = False
  , marked = False
  , id = ""
  , children = Collapsed
  , content = Occurences []
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
viewNode _ = text "helo"
{- viewNode node =
  case node of
    N_Collapsed sw parentId ->
      div
        [ classList
            [ ("collapsed", True)
            , ("marked", sw.marked)
            ]
        , onClick (Editor (ExpandNode sw.id))
        ]
        [ p
            [ class "inline-text" ]
            [ text "⮟ " ]
        , toHtml (sw.tagline) Dict.empty
        ]
    N_Expanded sw parentId occs deps ->
      div
        [ classList
            [ ("expanded", True)
            , ("marked", sw.marked)
            ]
         ]
        [ p
            [ class "left-button"
            , onClick (Editor (CollapseNode sw.id))
            ]
            [ text "⮝" ]
        , div
            [ class "right" ]
            [ viewOccurences occs
            , viewSlice References sw parentId
            , viewDependencies deps
            ]
        ]

viewOccurences : Occurences -> Html Msg
viewOccurences occ =
  case occ of
    O_Collapsed sid occs ->
      div
        [ class "collapsed"
        , onClick (Editor (ExpandOccurences sid))
        ]
        [ p
            [ class "inline-text" ]
            [ text
                ("⮟ show " ++ (String.fromInt (List.length occs))
                  ++ " occurences") ]
        ]
    O_Expanded sid occs ->
      div
        [ class "expanded" ]
        [ p
            [ class "left-button"
            , onClick (Editor (CollapseOccurences sid))
            ]
            [ text "⮝" ]
        , div
            [ class "right" ]
            (List.map viewNode occs)
        ]

viewDependencies : Dependencies -> Html Msg
viewDependencies dep =
  case dep of
    D_Collapsed sid deps ->
      div
        [ class "collapsed"
        , onClick ( Editor (ExpandDependencies sid))
        ]
        [ p
            [ class "inline-text" ]
            [ text
                ("⮟ show " ++ (String.fromInt (List.length deps))
                  ++ " dependencies") ]
        ]
    D_Expanded sid deps ->
      div
        [ class "expanded" ]
        [ p
            [ class "left-button"
            , onClick (Editor (CollapseDependencies sid))
            ]
            [ text "⮝" ]
        , div
            [ class "right" ]
            (List.map viewNode deps)
        ] -}

-- view an error in the style of surrounding elements
viewError : String -> Html Msg
viewError err =
  p
    [ class "error" ]
    [ text err ]

-- | viewing a single Slice
type Highlight
  = Occurencing SliceID
  | References
  | NoHighlight

renderFragment : Slice -> String
renderFragment slice =
  case slice of
    (Slice _ _ (Fragment codes) _ _) ->
      String.concat (List.intersperse "\n" codes)

{-
viewSlice : Highlight -> SliceWrap -> SliceID -> Html Msg
viewSlice highlight sw parentId =
  let
    renderedFragment = renderFragment sw.slice
    highlightDict =
      case highlight of
        NoHighlight -> Dict.empty
        References ->
          List.map
            (Tuple.mapSecond
              (\mid ->
                [ class "reference"
                , onMouseEnter (Editor (MarkReferenceP parentId mid))
                , onMouseLeave (Editor (UnmarkReferenceP parentId mid))
                , onClick (Editor (ExpandNode mid))
                ]
              )
            )
            (extractReferences sw.slice)
          |> Dict.fromList
        Occurencing occId ->
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
-}

-- monokai background color
monokai_black = (Element.rgb255 35 36 31)
monokai_colors_css =
  ".elmsh {color: #f8f8f2;background: #23241f;}.elmsh-hl {background: #343434;}.elmsh-add {background: #003800;}.elmsh-del {background: #380000;}.elmsh-comm {color: #75715e;}.elmsh1 {color: #ae81ff;}.elmsh2 {color: #e6db74;}.elmsh3 {color: #f92672;}.elmsh4 {color: #66d9ef;}.elmsh5 {color: #a6e22e;}.elmsh6 {color: #ae81ff;}.elmsh7 {color: #fd971f;}.elmsh-elm-ts, .elmsh-js-dk, .elmsh-css-p {font-style: italic;color: #66d9ef;}.elmsh-js-ce {font-style: italic;color: #a6e22e;}.elmsh-css-ar-i {font-weight: bold;color: #f92672;}"

show_caret_css =
  "textarea {caret-color: white;}"

type alias HighlightDict = Dict String (List (Html.Attribute Msg))

-- create a syntaxhighlighted, shrinkwrapped element
syntaxHighlight : String -> HighlightDict -> Element Msg
syntaxHighlight txt dict =
  SH.elm (if String.endsWith "\n" txt then txt ++ " " else txt)
    |> Result.map (SH.dictAddAttributes dict)
    |> Result.map SH.toInlineHtml
    |> Result.map (\h ->
        Html.div []
            [ Html.node "style" [] [ (Html.text monokai_colors_css) ]
            , h
            ] )
    |> Result.map Element.html
    |> Result.map (Element.el [ Background.color monokai_black ])
    |> Result.mapError Parser.deadEndsToString
    |> (\result ->
            case result of
                Result.Ok a ->
                    a

                Result.Err x ->
                    Element.text x
       )

-- create a syntaxhighlighted, shrinkwrapped textarea
editorField : String -> (String -> Msg) -> HighlightDict -> Element Msg
editorField txt onChange dict =
    Element.el
        [ Element.behindContent (syntaxHighlight txt dict)
        , Font.family [ Font.monospace ]
        , Font.size 16
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
            , label = Input.labelHidden "editing Area"
            , spellcheck = False
            })
