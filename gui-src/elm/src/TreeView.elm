module TreeView exposing (Node, NodeContent(..), EditorAction, nodeUpdate, viewEditor, monokai_colors_css, show_caret_css, reference_css, monokai_white, monokai_black, defaultNode)

import Dict exposing (Dict)

import Slice exposing (..)

-- imports for view
import Parser exposing (Parser)
import SyntaxHighlight as SH
import Html
import Html.Attributes as Attributes_
import Html.Events as Events_

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Events as Events


-- | MODEL
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



-- | UPDATE
type alias Msg = EditorAction

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
 | Nop

nodeUpdate: EditorAction -> Dict SliceID SliceWrap -> Node -> Result String Node
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
       Nop ->
         Ok node
   else
     propagateUpdate action cache node
 else
   Ok node

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

-- | create the children of a node
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
fetch : Dict SliceID SliceWrap -> SliceID -> Result String SliceWrap
fetch cache sid =
 case Dict.get sid cache of
   Nothing -> Err ("Missing slice: " ++ sid)
   Just sw -> Ok sw

-- Load a bunch of slicewraps from cache
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

-- | VIEW

-- | View the editor
viewEditor : Node -> Element Msg
viewEditor node =
  el
    [ padding 10
    , Background.color monokai_black
    , Font.color monokai_white
    , Font.family [ Font.monospace ]
    , Font.size 16
    , scrollbars
    , width fill
    , height fill
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
    el
      ([ Events.onClick ({target = id, action = Expand})
       , pointer
       , mouseOver [ Background.color monokai_grey ]
       ]
      ++ (if marked then [ Background.color monokai_grey ] else [])
      ++ fontColor)
      (viewTeaser content)

viewTeaser : NodeContent -> Element Msg
viewTeaser content =
  case content of
    SliceNode { tagline } ->
      row
        [ spacing 0
        , padding 0
        ]
        [ el
            [ Font.color actual_black ]
            (text "⮟ ")
        , inlineSH tagline
        ]
    Occurences occs ->
      case String.fromInt (List.length occs) of
        l ->
          text
            ("⮟ show " ++ l ++ " occurences")
    Dependencies deps ->
      case String.fromInt (List.length deps) of
        l ->
          text
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
        el
          (frameIf framed)
          (viewCollapsable
            id
            (column
              [ spacing 8
              ]
              ( bigOccs ++
                [ column
                    ((spacing 8) :: (nodeAttributes hovered marked id))
                    ( smallOccs ++ [(viewSlice sw editable id)] ++ smallDeps )
                ]
                ++ bigDeps)))

    _ -> text "Faulty SliceNode: Expanded but no children"

isEmptyNode : Node -> Bool
isEmptyNode { content } =
  case content of
    Occurences   [] -> True
    Dependencies [] -> True
    _               -> False

nodeAttributes : Bool -> Bool -> SliceID -> List (Attribute Msg)
nodeAttributes hovered marked sid =
  [ Events.onMouseEnter ({target = sid, action = Hover})
  , Events.onMouseLeave ({target = sid, action = Unhover})
  ] ++ (if marked || hovered then [ Background.color monokai_grey ] else [])

viewSlice : SliceWrap -> Bool -> String -> Element Msg
viewSlice sw editable nodeId =
  let
    renderedFragment = renderFragment sw.slice
    highlightDict =
      List.map
        (Tuple.mapSecond
          (\mid ->
            [ Events_.onMouseEnter
                ({target = nodeId ++ "dep" ++ mid, action = Mark})
            , Events_.onMouseLeave
                ({target = nodeId ++ "dep" ++ mid, action = Unmark})
            , Attributes_.class "reference"
            ]
          )
        )
        (extractReferences sw.slice)
      |> Dict.fromList
  in
    if editable then
      row
        []
        [ el
            [ Border.width 1
            , Border.color monokai_grey
            ]
            (editorField renderedFragment (\_ -> { target= nodeId, action = Nop}) highlightDict)
        , el
            [ Events.onClick ({target = nodeId, action = MakeStatic})
            , pointer
            , mouseOver [ Background.color monokai_black ]
            , Font.size 32
            , height fill
            , width fill
            , spaceEvenly
            , paddingEach {edges | left = 5}
            ]
            (text "✓")
        ]
    else
      el
        [ Events.onClick ({target = nodeId, action = MakeEditable})
        ]
        (syntaxHighlight renderedFragment highlightDict)

-- | Expanded - Occurences/Dependencies

viewListNode : List Node -> Node -> Element Msg
viewListNode nodes { hovered, marked, framed, id } =
  el
    (frameIf framed)
    (viewCollapsable
      id
      (column
        [ spacing 16 ]
        (List.map viewNode nodes)))

-- | Common helpers

viewCollapsable : SliceID -> Element Msg -> Element Msg
viewCollapsable sid content =
  row
    [ spacing 5 ]
    [ (column
        [ height fill
        , Events.onClick ({target = sid, action = Collapse})
        , Events.onMouseEnter ({target = sid, action = Frame})
        , Events.onMouseLeave ({target = sid, action = Unframe})
        , pointer
        , Font.color actual_black
        , mouseOver [ Background.color monokai_grey ]
        ]
        [ {- el
            [ alignTop ]
            ( text "⮟" )
        ,-} el
            [ centerX
            , width (px 1)
            , height fill
            , Border.widthEach { bottom = 0, left = 0, right = 1, top = 0 }
            , Border.color actual_black
            ]
            none
        , el
            [ alignBottom ]
            ( text "⮝" )
        ]
      )
    , content
    ]


frameIf : Bool -> List (Attribute Msg)
frameIf framed =
  if framed then
     [ Border.width 1, Border.color monokai_white ]
   else
     [ Border.width 1, Border.color monokai_black ]


-- | COLOR PALETTE
monokai_black = (rgb255 35 36 31)
monokai_grey = (rgb255 51 52 47)
monokai_white = (rgb255 247 247 241)
actual_black = rgb 0 0 0
glass = rgba 0 0 0 0
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
    |> Result.map html
    -- padding to the right is needed to prevent overlayed text from breaking too early
    |> Result.map (el [ paddingEach { edges | right = 3 } ])
    |> Result.mapError Parser.deadEndsToString
    |> (\result ->
            case result of
                Result.Ok a ->
                    a

                Result.Err x ->
                    text x
       )

-- create inline HTML of code
inlineSH : String -> Element Msg
inlineSH txt =
  SH.haskell txt
  |> Result.map SH.toInlineHtml
  |> Result.map html
  |> Result.mapError Parser.deadEndsToString
  |> (\result ->
        case result of
          Ok a  -> a
          Err x -> text x)

-- create a syntaxhighlighted, shrinkwrapped textarea
editorField : String -> (String -> Msg) -> HighlightDict -> Element Msg
editorField txt onChange dict =
    el
        [ inFront (invisibleTextarea txt onChange)
        ]
        (syntaxHighlight txt dict)

invisibleTextarea : String -> (String -> Msg) -> Element Msg
invisibleTextarea txt onChange =
  Input.multiline
      [ height shrink
      , width shrink
      , Background.color glass
      , Font.color glass
      , spacing 0
      , padding 0
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
