module SyntaxHighlight.Language.Xml exposing
    ( Syntax(..)
    ,  syntaxToStyle
       -- Exposing for tests purpose

    , toLines
    , toRevTokens
    )

import Char
import Parser exposing ((|.), DeadEnd, Parser, Step(..), andThen, chompIf, getChompedString, keyword, loop, map, oneOf, succeed, symbol)
import SyntaxHighlight.Language.Helpers exposing (Delimiter, addThen, chompIfThenWhile, consThen, delimited, isLineBreak, isSpace, isWhitespace, thenChompWhile)
import SyntaxHighlight.Language.Type as T
import SyntaxHighlight.Line exposing (Line)
import SyntaxHighlight.Line.Helpers as Line
import SyntaxHighlight.Style as Style exposing (Required(..))


type alias Token =
    T.Token Syntax


type Syntax
    = Tag
    | Attribute
    | AttributeValue


toLines : String -> Result (List DeadEnd) (List (Line msg))
toLines =
    Parser.run toRevTokens
        >> Result.map (Line.toLines syntaxToStyle)


toRevTokens : Parser (List Token)
toRevTokens =
    loop [] mainLoop


mainLoop : List Token -> Parser (Step (List Token) (List Token))
mainLoop revTokens =
    oneOf
        [ whitespace
            |> map (\n -> Loop (n :: revTokens))
        , comment
            |> map (\n -> Loop (n ++ revTokens))
        , chompIfThenWhile (\c -> c /= '<' && not (isLineBreak c))
            |> getChompedString
            |> map (\n -> Loop (( T.Normal, n ) :: revTokens))
        , openTag revTokens
            |> map Loop
        , succeed (Done revTokens)
        ]


openTag : List Token -> Parser (List Token)
openTag revTokens =
    openTagParser
        |> getChompedString
        |> map (\b -> ( T.Normal, b ) :: revTokens)
        |> andThen tag


openTagParser : Parser ()
openTagParser =
    succeed ()
        |. chompIf (\c -> c == '<')
        |. oneOf
            [ chompIf (\c -> c == '/' || c == '!')
            , succeed ()
            ]


tag : List Token -> Parser (List Token)
tag revTokens =
    oneOf
        [ chompIf isStartTagChar
            |> thenChompWhile isTagChar
            |> getChompedString
            |> map (\b -> ( T.C Tag, b ))
            |> andThen
                (\n -> loop (n :: revTokens) attributeLoop)
        , succeed revTokens
        ]


isStartTagChar : Char -> Bool
isStartTagChar c =
    Char.isUpper c || Char.isLower c || Char.isDigit c


isTagChar : Char -> Bool
isTagChar c =
    isStartTagChar c || c == '-'


attributeLoop : List Token -> Parser (Step (List Token) (List Token))
attributeLoop revTokens =
    oneOf
        [ chompIfThenWhile isAttributeChar
            |> getChompedString
            |> map (\b -> ( T.C Attribute, b ))
            |> consThen attributeConfirm revTokens
            |> map Loop
        , whitespace
            |> map (\n -> Loop (n :: revTokens))
        , chompIfThenWhile (\c -> not (isWhitespace c) && c /= '>')
            |> getChompedString
            |> map (\b -> Loop (( T.Normal, b ) :: revTokens))
        , succeed (Done revTokens)
        ]


isAttributeChar : Char -> Bool
isAttributeChar c =
    isTagChar c || c == '_'


attributeConfirm : List Token -> Parser (List Token)
attributeConfirm revTokens =
    oneOf
        [ whitespace
            |> consThen attributeConfirm revTokens
        , symbol "="
            |> map (\_ -> ( T.Normal, "=" ))
            |> consThen attributeValueLoop revTokens
        , succeed revTokens
        ]


attributeValueLoop : List Token -> Parser (List Token)
attributeValueLoop revTokens =
    oneOf
        [ whitespace
            |> consThen attributeValueLoop revTokens
        , attributeValue
            |> addThen succeed revTokens
        , succeed revTokens
        ]



-- Attribute Value


attributeValue : Parser (List Token)
attributeValue =
    oneOf
        [ doubleQuote
        , quote
        , chompIfThenWhile (\c -> not (isWhitespace c) && c /= '>')
            |> getChompedString
            |> map (\b -> [ ( T.C AttributeValue, b ) ])
        ]


doubleQuote : Parser (List Token)
doubleQuote =
    delimited doubleQuoteDelimiter


doubleQuoteDelimiter : Delimiter Token
doubleQuoteDelimiter =
    { start = "\""
    , end = "\""
    , isNestable = False
    , defaultMap = \b -> ( T.C AttributeValue, b )
    , innerParsers = [ lineBreakList ]
    , isNotRelevant = not << isLineBreak
    }


quote : Parser (List Token)
quote =
    delimited
        { doubleQuoteDelimiter
            | start = "'"
            , end = "'"
        }



-- Comment


comment : Parser (List Token)
comment =
    delimited
        { doubleQuoteDelimiter
            | start = "<!--"
            , end = "-->"
            , defaultMap = \b -> ( T.Comment, b )
        }



-- Helpers


whitespace : Parser Token
whitespace =
    oneOf
        [ chompIfThenWhile isSpace
            |> getChompedString
            |> map (\s -> ( T.Normal, s ))
        , lineBreak
        ]


lineBreak : Parser Token
lineBreak =
    symbol "\n"
        |> map (\_ -> ( T.LineBreak, "\n" ))


lineBreakList : Parser (List Token)
lineBreakList =
    lineBreak
        |> map List.singleton


syntaxToStyle : Syntax -> ( Style.Required, String )
syntaxToStyle syntax =
    case syntax of
        Tag ->
            ( Style3, "xml-t" )

        Attribute ->
            ( Style5, "xml-a" )

        AttributeValue ->
            ( Style2, "xlm-av" )
