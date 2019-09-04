module SyntaxHighlight.Line.Helpers exposing (toLines)

import SyntaxHighlight.Language.Type as T exposing (Syntax(..), Token)
import SyntaxHighlight.Line exposing (Fragment, Line)
import SyntaxHighlight.Style as Style exposing (Required(..))


toLines : (a -> ( Required, String )) -> List (Token a) -> List (Line msg)
toLines toStyle revTokens =
    List.foldl (toLinesHelp toStyle) ( [], [], Nothing ) revTokens
        |> (\( lines, frags, _ ) -> newLine frags :: lines)


toLinesHelp : (a -> ( Required, String )) -> Token a -> ( List (Line msg), List (Fragment msg), Maybe (Syntax a) ) -> ( List (Line msg), List (Fragment msg), Maybe (Syntax a) )
toLinesHelp toStyle ( syntax, text ) ( lines, fragments, maybeLastSyntax ) =
    if syntax == LineBreak then
        ( newLine fragments :: lines
        , [ toFragment toStyle ( syntax, text ) ]
        , Nothing
        )

    else if Just syntax == maybeLastSyntax then
        -- Concat same syntax sequence to reduce html elements.
        case fragments of
            headFrag :: tailFrags ->
                ( lines
                , { headFrag | text = text ++ headFrag.text }
                    :: tailFrags
                , maybeLastSyntax
                )

            _ ->
                ( lines
                , toFragment toStyle ( syntax, text ) :: fragments
                , maybeLastSyntax
                )

    else
        ( lines
        , toFragment toStyle ( syntax, text ) :: fragments
        , Just syntax
        )


toFragment : (a -> ( Required, String )) -> Token a -> (Fragment msg)
toFragment toStyle ( syntax, text ) =
    case syntax of
        Normal ->
            { text = text
            , requiredStyle = Default
            , additionalClass = ""
            , additionalAttributes = []
            }

        T.Comment ->
            { text = text
            , requiredStyle = Style.Comment
            , additionalClass = ""
            , additionalAttributes = []
            }

        LineBreak ->
            { text = text
            , requiredStyle = Default
            , additionalClass = ""
            , additionalAttributes = []
            }

        C c ->
            let
                ( requiredStyle, additionalClass ) =
                    toStyle c
            in
            { text = text
            , requiredStyle = requiredStyle
            , additionalClass = additionalClass
            , additionalAttributes = []
            }


newLine : List (Fragment msg) -> (Line msg)
newLine fragments =
    { fragments = fragments
    , highlight = Nothing
    }
