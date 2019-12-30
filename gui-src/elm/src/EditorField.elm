module EditorField exposing (css, syntaxHighlight, inlineSH, editorField)

import SyntaxHighlight as SH
import Parser
import Html
import Dict exposing (Dict)

import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Border as Border
import Element.Input as Input

import Palette exposing (..)

-- | SYNTAX HIGHLIGHTING
css : Bool -> String
css dark =
  if dark then
    onedark_colors_css ++ light_caret_css ++ reference_css
  else
    onedark_colors_inverted_css ++ reference_css

{-monokai_colors_css =
  ".elmsh {color: #f8f8f2;}.elmsh-hl {background: #343434;}.elmsh-add {background: #003800;}.elmsh-del {background: #380000;}.elmsh-comm {color: #75715e;}.elmsh1 {color: #ae81ff;}.elmsh2 {color: #e6db74;}.elmsh3 {color: #f92672;}.elmsh4 {color: #66d9ef;}.elmsh5 {color: #a6e22e;}.elmsh6 {color: #ae81ff;}.elmsh7 {color: #fd971f;}.elmsh-elm-ts, .elmsh-js-dk, .elmsh-css-p {font-style: italic;color: #66d9ef;}.elmsh-js-ce {font-style: italic;color: #a6e22e;}.elmsh-css-ar-i {font-weight: bold;color: #f92672;}" -}

onedark_colors_css =
  ".elmsh {color: #abb2bf;}.elmsh-hl {background: rgba(229, 231,235, 0.1);}.elmsh-add {background: rgba(40, 124,82, 0.4);}.elmsh-del {background: rgba(136, 64,67, 0.4);}.elmsh-comm {font-style: italic;color: #5c6370;}.elmsh1 {color: #d19a66;}.elmsh2 {color: #98c379;}.elmsh3 {color: #c678dd;}.elmsh4 {color: #c678dd;}.elmsh5 {color: #61aeee;}.elmsh6 {color: #d19a66;}.elmsh7 {color: #abb2bf;}.elmsh-elm-ts, .elmsh-js-dk, .elmsh-css-p {font-style: italic;color: #66d9ef;}.elmsh-js-ce {font-style: italic;color: #a6e22e;}.elmsh-css-ar-i {font-weight: bold;color: #f92672;}"
light_caret_css =
  "textarea {caret-color: white;}"
reference_css =
  ".reference {text-decoration: underline;}.reference:hover {cursor:pointer;}"

-- light colors:
-- (mirrored lightness (30% -> 70% etc.))
onedark_colors_inverted_css =
  """.elmsh {
    color: rgb(67, 74, 85);
  }
  .elmsh-hl { background: rgba(229, 231,235, 0.1);}.elmsh-add { background: rgba(40, 124,82, 0.4);  }.elmsh-del { background: rgba(136, 64,67, 0.4);  }
  .elmsh-comm {
    font-style: italic;
    color: hsl(218, 9%, 60%);
  }
  .elmsh1 {
    color: hsl(29, 53%, 40%);
  }
  .elmsh2 {
    color: hsl(94, 38%, 39%);
  }
  .elmsh3 {
    color: hsl(286, 59%, 34%);
  }
  .elmsh4 {
    color: hsl(286, 59%, 34%);
  }
  .elmsh5 {
    color: hsl(207, 80%, 35%);
  }
  .elmsh6 {
    color: hsl(29, 53%, 40%);
  }
  .elmsh7 {
    color: hsl(218, 13%, 30%);
  }
  .elmsh-elm-ts, .elmsh-js-dk, .elmsh-css-p {
    font-style: italic;
    color: hsl(189, 80%, 34%);
  }
  .elmsh-js-ce {
    font-style: italic;
    color: hsl(80, 75%, 47%);
  }
  .elmsh-css-ar-i {
    font-weight: bold;
    color: hsl(338, 94%, 44%);
  }"""


type alias HighlightDict msg = Dict String (List (Html.Attribute msg))

-- create a syntaxhighlighted, shrinkwrapped element
syntaxHighlight : String -> HighlightDict msg -> Element msg
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
inlineSH : String -> Element msg
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
editorField : String -> (String -> msg) -> HighlightDict msg -> Element msg
editorField txt onChange dict =
    Element.el
        [ Element.inFront (invisibleTextarea txt onChange)
        ]
        (syntaxHighlight txt dict)

invisibleTextarea : String -> (String -> msg) -> Element msg
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
