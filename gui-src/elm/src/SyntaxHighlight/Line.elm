module SyntaxHighlight.Line exposing
    ( Line, Fragment, Highlight(..)
    , highlightLines
    )

{-| A parsed highlighted line.

@docs Line, Fragment, Highlight


## Helpers

@docs highlightLines

-}

import SyntaxHighlight.Style as Style
import Html


{-| A line holds information about its fragments and if is highlighted in any way.
-}
type alias Line msg =
    { fragments : List (Fragment msg)
    , highlight : Maybe Highlight
    }


{-| A fragment holds information about the text being styled, the style and additional class to be applied.
-}
type alias Fragment msg =
    { text : String
    , requiredStyle : Style.Required
    , additionalClass : String
    , additionalAttributes : List (Html.Attribute msg)
    }


type Highlight
    = Normal
    | Add
    | Del


highlightLines : Maybe Highlight -> Int -> Int -> List (Line msg) -> List (Line msg)
highlightLines maybeHighlight start end lines =
    let
        length =
            List.length lines

        start_ =
            if start < 0 then
                length + start

            else
                start

        end_ =
            if end < 0 then
                length + end

            else
                end
    in
    List.indexedMap (highlightLinesHelp maybeHighlight start_ end_) lines


highlightLinesHelp : Maybe Highlight -> Int -> Int -> Int -> (Line msg) -> (Line msg)
highlightLinesHelp maybeHighlight start end index line =
    if index >= start && index < end then
        { line | highlight = maybeHighlight }

    else
        line
