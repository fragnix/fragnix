module SyntaxHighlight.Style exposing (Color(..), Required(..), RequiredStyles, Style, backgroundColor, bold, colorToCss, emptyIfFalse, italic, noEmphasis, styleToCss, textColor, toCss, toCssClass)

{-
   The common uses of the styles are the following:

     - **Default**: Default style
     - **Comment**: Comment
     - **Style1**: Number
     - **Style2**: Literal string, attribute value
     - **Style3**: Keyword, tag, operator symbol (=+-*/...)
     - **Style4**: Keyword, group symbol ({}(),)
     - **Style5**: Function, attribute name
     - **Style6**: Literal keyword
     - **Style7**: Argument, parameter
-}


type Required
    = Default
    | Comment
    | Style1
    | Style2
    | Style3
    | Style4
    | Style5
    | Style6
    | Style7


type alias RequiredStyles =
    { default : Style
    , highlight : Style
    , addition : Style
    , deletion : Style
    , comment : Style
    , style1 : Style
    , style2 : Style
    , style3 : Style
    , style4 : Style
    , style5 : Style
    , style6 : Style
    , style7 : Style
    }


type alias Style =
    { isBold : Bool
    , isItalic : Bool
    , isUnderline : Bool
    , text : Color
    , background : Color
    }


type Color
    = DefaultColor
    | Hex String
    | Rgb Int Int Int
    | Rgba Int Int Int Float


noEmphasis : Color -> Color -> Style
noEmphasis text background =
    { isBold = False
    , isItalic = False
    , isUnderline = False
    , text = text
    , background = background
    }


textColor : Color -> Style
textColor text =
    { isBold = False
    , isItalic = False
    , isUnderline = False
    , text = text
    , background = DefaultColor
    }


backgroundColor : Color -> Style
backgroundColor background =
    { isBold = False
    , isItalic = False
    , isUnderline = False
    , text = DefaultColor
    , background = background
    }


italic : Style -> Style
italic style =
    { style | isItalic = True }


bold : Style -> Style
bold style =
    { style | isBold = True }



-- To Css string helpers


toCss : List ( String, Style ) -> String
toCss classes =
    List.map toCssClass classes
        |> String.concat


toCssClass : ( String, Style ) -> String
toCssClass ( selectors, style ) =
    if String.isEmpty selectors then
        ""

    else
        selectors ++ " {" ++ styleToCss style ++ "}"


styleToCss : Style -> String
styleToCss { isBold, isItalic, isUnderline, text, background } =
    String.concat
        [ emptyIfFalse isBold "font-weight: bold;"
        , emptyIfFalse isItalic "font-style: italic;"
        , emptyIfFalse isUnderline "text-decoration: underline;"
        , colorToCss "color: " text
        , colorToCss "background: " background
        ]


emptyIfFalse : Bool -> String -> String
emptyIfFalse bool str =
    if bool then
        str

    else
        ""


colorToCss : String -> Color -> String
colorToCss property color =
    case color of
        DefaultColor ->
            ""

        Hex hex ->
            property ++ hex ++ ";"

        Rgb r g b ->
            String.concat
                [ property
                , "rgb("
                , String.fromInt r
                , ", "
                , String.fromInt g
                , ","
                , String.fromInt b
                , ");"
                ]

        Rgba r g b a ->
            String.concat
                [ property
                , "rgba("
                , String.fromInt r
                , ", "
                , String.fromInt g
                , ","
                , String.fromInt b
                , ", "
                , String.fromFloat a
                , ");"
                ]
