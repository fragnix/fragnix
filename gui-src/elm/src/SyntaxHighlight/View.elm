module SyntaxHighlight.View exposing (ConsoleOptions, toBlockHtml, toConsole, toInlineHtml, toStaticBlockHtml, toStaticInlineHtml)

import Html exposing (Html, br, code, div, pre, span, text)
import Html.Attributes exposing (attribute, class, classList)
import SyntaxHighlight.Line exposing (..)
import SyntaxHighlight.Style exposing (Required(..))



-- Html


toBlockHtml : Maybe Int -> List (Line msg) -> Html msg
toBlockHtml maybeStart lines =
    case maybeStart of
        Nothing ->
            pre [ class "elmsh" ]
                [ toInlineHtml lines ]

        Just start ->
            lines
                |> List.indexedMap (lineView start)
                |> code []
                |> List.singleton
                |> pre [ class "elmsh" ]


lineView : Int -> Int -> (Line msg) -> Html msg
lineView start index { fragments, highlight } =
    div
        [ classList
            [ ( "elmsh-line", True )
            , ( "elmsh-hl", highlight == Just Normal )
            , ( "elmsh-add", highlight == Just Add )
            , ( "elmsh-del", highlight == Just Del )
            ]
        , attribute "data-elmsh-lc" (String.fromInt (start + index))
        ]
        (List.map fragmentView fragments)


toInlineHtml : List (Line msg) -> Html msg
toInlineHtml lines =
    lines
        |> List.map
            (\{ highlight, fragments } ->
                if highlight == Nothing then
                    List.map fragmentView fragments

                else
                    [ span
                        [ classList
                            [ ( "elmsh-hl", highlight == Just Normal )
                            , ( "elmsh-add", highlight == Just Add )
                            , ( "elmsh-del", highlight == Just Del )
                            ]
                        ]
                        (List.map fragmentView fragments)
                    ]
            )
        |> List.concat
        |> code [ class "elmsh" ]


fragmentView : Fragment msg -> Html msg
fragmentView { text, requiredStyle, additionalClass, additionalAttributes } =
    if requiredStyle == Default && String.isEmpty additionalClass && List.isEmpty additionalAttributes then
        Html.text text

    else
        span
            ([ classList
                [ ( requiredStyleToString requiredStyle
                  , requiredStyle /= Default
                  )
                , ( "elmsh-" ++ additionalClass
                  , additionalClass /= ""
                  )
                ]
            ] ++ additionalAttributes)
            [ Html.text text ]


requiredStyleToString : Required -> String
requiredStyleToString required =
    (++) "elmsh" <|
        case required of
            Default ->
                "0"

            Comment ->
                "-comm"

            Style1 ->
                "1"

            Style2 ->
                "2"

            Style3 ->
                "3"

            Style4 ->
                "4"

            Style5 ->
                "5"

            Style6 ->
                "6"

            Style7 ->
                "7"



-- Static Html


toStaticBlockHtml : Maybe Int -> List (Line msg) -> String
toStaticBlockHtml maybeStart lines =
    case maybeStart of
        Nothing ->
            "<pre class=\"elmsh\">"
                ++ toStaticInlineHtml lines
                ++ "</pre>"

        Just start ->
            String.concat
                [ "<pre class=\"elmsh\"><code>"
                , List.indexedMap (staticLineView start) lines
                    |> String.concat
                , "</code></pre>"
                ]


staticLineView : Int -> Int -> (Line msg) -> String
staticLineView start index { fragments, highlight } =
    String.concat
        [ "<div class=\""
        , "elmsh-line "
        , emptyIfFalse (highlight == Just Normal) "elmsh-hl "
        , emptyIfFalse (highlight == Just Add) "elmsh-add "
        , emptyIfFalse (highlight == Just Del) "elmsh-del "
        , "\" data-elmsh-lc=\""
        , String.fromInt (start + index)
        , "\">"
        , List.map staticFragmentView fragments |> String.concat
        , "</div>"
        ]


toStaticInlineHtml : List (Line msg) -> String
toStaticInlineHtml lines =
    String.concat
        [ "<code class=\"elmsh\">"
        , List.map
            (\{ highlight, fragments } ->
                if highlight == Nothing then
                    List.map staticFragmentView fragments

                else
                    [ "<span class=\""
                    , emptyIfFalse (highlight == Just Normal)
                        "elmsh-hl "
                    , emptyIfFalse (highlight == Just Add)
                        "elmsh-add "
                    , emptyIfFalse (highlight == Just Del)
                        "elmsh-del "
                    , "\">"
                    , List.map staticFragmentView fragments
                        |> String.concat
                    , "</span>"
                    ]
            )
            lines
            |> List.concat
            |> String.concat
        , "</code>"
        ]


staticFragmentView : (Fragment msg) -> String
staticFragmentView { text, requiredStyle, additionalClass } =
    if requiredStyle == Default && String.isEmpty additionalClass then
        text

    else
        String.concat
            [ "<span class=\""
            , emptyIfFalse
                (requiredStyle /= Default)
                (requiredStyleToString requiredStyle)
            , " "
            , emptyIfFalse
                (additionalClass /= "")
                ("elmsh-" ++ additionalClass)
            , "\">"
            , text
            , "</span>"
            ]


emptyIfFalse : Bool -> String -> String
emptyIfFalse bool str =
    if bool then
        str

    else
        ""



-- Console


type alias ConsoleOptions =
    { default : String -> String
    , highlight : String -> String
    , addition : String -> String
    , deletion : String -> String
    , comment : String -> String
    , style1 : String -> String
    , style2 : String -> String
    , style3 : String -> String
    , style4 : String -> String
    , style5 : String -> String
    , style6 : String -> String
    , style7 : String -> String
    }


toConsole : ConsoleOptions -> List (Line msg) -> List String
toConsole options lines =
    List.map
        (\{ highlight, fragments } ->
            if highlight == Nothing then
                List.map (consoleFragmentView options) fragments
                    |> String.concat

            else
                List.map (consoleFragmentView options) fragments
                    |> String.concat
                    |> (\n ->
                            case highlight of
                                Nothing ->
                                    n

                                Just Normal ->
                                    options.highlight n

                                Just Add ->
                                    options.addition n

                                Just Del ->
                                    options.deletion n
                       )
        )
        lines


consoleFragmentView : ConsoleOptions -> Fragment msg -> String
consoleFragmentView options { text, requiredStyle, additionalClass } =
    case requiredStyle of
        Default ->
            options.default text

        Comment ->
            options.comment text

        Style1 ->
            options.style1 text

        Style2 ->
            options.style2 text

        Style3 ->
            options.style3 text

        Style4 ->
            options.style4 text

        Style5 ->
            options.style5 text

        Style6 ->
            options.style6 text

        Style7 ->
            options.style7 text
