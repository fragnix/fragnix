module Editor exposing (..)

import Set exposing (Set)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Events as Events

import Html.Events as HtmlEvents
import Html.Attributes as HtmlAttributes

import Slice exposing (..)
import LocalSlice exposing (..)
import Palette exposing (..)
import EditorField

import Dict exposing (Dict)

-- | MODEL: Editor State
type alias Node =
  { hovered: Bool
  , marked: Bool
  , id: SliceID
  , children: Children
  , content: NodeContent
  , editable: Bool
  , framed: Bool
  , changed: Bool
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
  , changed = False
  }

type NodeContent
 = SliceNode SliceWrap
 | Occurences (List SliceWrap)
 | Dependencies (List SliceWrap)

-- | Helpers
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


-- | UPDATE
-- Sometimes the editor needs to send a Msg to its parent, e.g. when slices
-- are to be updated
type Msg
  = Main MainAction
  | Editor EditorAction

type MainAction
  = Edit String SliceWrap

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

type alias Cache = Dict SliceID SliceWrap

-- | recursively updating the editor model
nodeUpdate: EditorAction -> Cache -> Set SliceID -> Node -> Result String Node
nodeUpdate action cache changed node =
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
          expandNode cache node changed
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
      propagateUpdate action cache changed node
  else
    Ok node

propagateUpdate : EditorAction -> Cache -> Set SliceID -> Node -> Result String Node
propagateUpdate action cache changed node =
  case node.children of
    Collapsed ->
      Ok node
    Expanded cs ->
      case combineResults (List.map (nodeUpdate action cache changed) cs) of
        Ok newNodes ->
          Ok { node | children = Expanded newNodes }
        Err errs    ->
          Err (String.concat (List.intersperse "," errs))

collapseNode : Node -> Result String Node
collapseNode node =
  Ok { node | children = Collapsed }

-- | create the children of a node
expandNode : Cache -> Node -> Set SliceID -> Result String Node
expandNode cache node changed =
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
        |> Result.map (updateChanged changed)
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
        }
        |> Ok
        |> Result.map (updateChanged changed)
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
        }
        |> Ok
        |> Result.map (updateChanged changed)

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

updateChanged : Set SliceID -> Node -> Node
updateChanged changed =
  mapNode
    (\n ->
      case n.content of
        SliceNode sw ->
          if Set.member sw.id changed then
            { n | changed = True }
          else
            { n | changed = False }
        Dependencies deps ->
          if (List.any (\{id} -> Set.member id changed) deps) then
            { n | changed = True }
          else
            { n | changed = False }
        Occurences occs ->
          if (List.any (\{id} -> Set.member id changed) occs) then
            { n | changed = True }
          else
            { n | changed = False })

-- VIEW

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
viewCollapsedNode { marked, id, content, changed } =
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
      (viewTeaser content changed)

viewTeaser : NodeContent -> Bool -> Element Msg
viewTeaser content changed =
  Element.row
    [ Element.spacing 0
    , Element.padding 0
    ]
    [ Element.el
        [ Font.color
            (if changed then orange else actual_black)
        ]
        (Element.text "⮟ ")
    , case content of
        SliceNode {tagline} ->
          EditorField.inlineSH tagline
        Occurences occs ->
          case String.fromInt (List.length occs) of
            l ->
              Element.text
                ("show " ++ l ++ " occurences")
        Dependencies deps ->
          case String.fromInt (List.length deps) of
            l ->
              Element.text
                ("show " ++ l ++ " dependencies")
    ]

-- Expanded - Slice
viewSliceNode : SliceWrap -> Node -> Element Msg
viewSliceNode sw { hovered, marked, id, children, editable, framed, changed } =
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

        debugInfo =
          [ Element.row
              [ Element.spacing 8 ] (List.map Element.text sw.names)
          , Element.row
              [ Element.spacing 8 ] (List.map Element.text sw.signatures)
          ]

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
                    ( smallOccs ++ [(viewSlice sw editable changed id)] ++ smallDeps )
                ]
                ++ bigDeps
                ++ debugInfo)))

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

viewSlice : SliceWrap -> Bool -> Bool -> String -> Element Msg
viewSlice sw editable changed nodeId =
  let
    renderedFragment = renderFragment sw.slice
    highlightDict =
      List.map
        (Tuple.mapSecond
          (\mid ->
            [ HtmlEvents.onMouseEnter
                (Editor {target = nodeId ++ "dep" ++ mid, action = Mark})
            , HtmlEvents.onMouseLeave
                (Editor {target = nodeId ++ "dep" ++ mid, action = Unmark})
            , HtmlAttributes.class "reference"
            ]
          )
        )
        (extractReferences sw.slice)
      |> Dict.fromList
    dirtyAttribs =
      if changed then
        [ Border.widthEach { edges | left = 1 }
        , Border.color orange
        ]
      else
        []
  in
    if editable then
      Element.row
        dirtyAttribs
        [ Element.el
            [ Border.width 1
            , Border.color monokai_grey
            ]
            (EditorField.editorField
              renderedFragment
              (\txt -> Main (Edit txt sw))
              highlightDict)
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
        (EditorField.syntaxHighlight renderedFragment highlightDict)

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
