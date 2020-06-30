module SyntaxHighlight.Language.Elm exposing
    ( Syntax(..)
    ,  syntaxToStyle
       -- Exposing for tests purpose

    , toLines
    , toRevTokens
    )

import Char
import Parser exposing ((|.), DeadEnd, Parser, Step(..), andThen, backtrackable, chompIf, getChompedString, keyword, loop, map, oneOf, succeed, symbol)
import Set exposing (Set)
import SyntaxHighlight.Language.Helpers exposing (Delimiter, chompIfThenWhile, delimited, escapable, isEscapable, isLineBreak, isSpace, isWhitespace, number, thenChompWhile)
import SyntaxHighlight.Language.Type as T
import SyntaxHighlight.Line exposing (Line)
import SyntaxHighlight.Line.Helpers as Line
import SyntaxHighlight.Style as Style exposing (Required(..))


type alias Token =
    T.Token Syntax


type Syntax
    = String
    | BasicSymbol
    | GroupSymbol
    | Capitalized
    | Keyword
    | Function
    | TypeSignature
    | Number


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
        [ space
            |> map (\n -> Loop (n :: revTokens))
        , lineBreak
            |> map (\n -> Loop (n :: revTokens))
        , comment
            |> map (\n -> Loop (n ++ revTokens))
        , variable
            |> andThen (lineStartVariable revTokens)
            |> map Loop
        , stringLiteral
            |> andThen (\s -> loop (s ++ revTokens) functionBody)
            |> map Loop
        , functionBodyContent
            |> andThen (\s -> loop (s :: revTokens) functionBody)
            |> map Loop
        , succeed (Done revTokens)
        ]


lineStartVariable : List Token -> String -> Parser (List Token)
lineStartVariable revTokens n =
    if n == "module" || n == "import" then
        moduleDeclaration
            |> loop (( T.C Keyword, n ) :: revTokens)

    else if n == "port" then
        portDeclaration
            |> loop (( T.C Keyword, n ) :: revTokens)

    else if isKeyword n then
        functionBody
            |> loop (( T.C Keyword, n ) :: revTokens)

    else
        functionSignature
            |> loop (( T.C Function, n ) :: revTokens)



-- Module Declaration


moduleDeclaration : List Token -> Parser (Step (List Token) (List Token))
moduleDeclaration revTokens =
    oneOf
        [ whitespaceOrCommentStep revTokens
        , symbol "("
            |> map (always (( T.Normal, "(" ) :: revTokens))
            |> andThen (\n -> loop n modDecParentheses)
            |> map Loop
        , oneOf
            [ commentChar
                |> map (\b -> ( T.Normal, b ))
            , keyword "exposing"
                |> map (always ( T.C Keyword, "exposing" ))
            , keyword "as"
                |> map (always ( T.C Keyword, "as" ))
            , chompIfThenWhile modDecIsNotRelevant
                |> getChompedString
                |> map (\b -> ( T.Normal, b ))
            ]
            |> map (\n -> Loop (n :: revTokens))
        , succeed (Done revTokens)
        ]


modDecIsNotRelevant : Char -> Bool
modDecIsNotRelevant c =
    not (isWhitespace c || isCommentChar c || c == '(')


modDecParentheses : List Token -> Parser (Step (List Token) (List Token))
modDecParentheses revTokens =
    oneOf
        [ whitespaceOrCommentStep revTokens
        , symbol ")"
            |> map (always (( T.Normal, ")" ) :: revTokens))
            |> map Done
        , oneOf
            [ infixParser
            , commentChar |> map (\b -> ( T.Normal, b ))
            , chompIfThenWhile (\c -> c == ',' || c == '.')
                |> getChompedString
                |> map (\b -> ( T.Normal, b ))
            , chompIf Char.isUpper
                |> thenChompWhile mdpIsNotRelevant
                |> getChompedString
                |> map (\b -> ( T.C TypeSignature, b ))
            , chompIfThenWhile mdpIsNotRelevant
                |> getChompedString
                |> map (\b -> ( T.C Function, b ))
            ]
            |> map (\n -> Loop (n :: revTokens))
        , symbol "("
            |> map (always (( T.Normal, "(" ) :: revTokens))
            |> andThen (\n -> loop ( 0, n ) modDecParNest)
            |> map Loop
        , succeed (Done revTokens)
        ]


mdpIsNotRelevant : Char -> Bool
mdpIsNotRelevant c =
    not (isWhitespace c || isCommentChar c || c == '(' || c == ')' || c == ',' || c == '.')


modDecParNest : ( Int, List Token ) -> Parser (Step ( Int, List Token ) (List Token))
modDecParNest ( nestLevel, revTokens ) =
    oneOf
        [ whitespaceOrCommentStepNested ( nestLevel, revTokens )
        , symbol "("
            |> map (always (( T.Normal, "(" ) :: revTokens))
            |> map (\ns -> Loop ( nestLevel + 1, ns ))
        , symbol ")"
            |> map (always (( T.Normal, ")" ) :: revTokens))
            |> map
                (\ns ->
                    if nestLevel == 0 then
                        Done ns

                    else
                        Loop ( nestLevel - 1, ns )
                )
        , oneOf
            [ commentChar |> map (\b -> ( T.Normal, b ))
            , chompIfThenWhile (not << mdpnIsSpecialChar)
                |> getChompedString
                |> map (\s -> ( T.Normal, s ))
            ]
            |> map (\n -> Loop ( nestLevel, n :: revTokens ))
        , succeed (Done revTokens)
        ]


mdpnIsSpecialChar : Char -> Bool
mdpnIsSpecialChar c =
    isLineBreak c || isCommentChar c || c == '(' || c == ')'



-- Port Declaration


portDeclaration : List Token -> Parser (Step (List Token) (List Token))
portDeclaration revTokens =
    oneOf
        [ whitespaceOrCommentStep revTokens
        , variable
            |> andThen (portDeclarationHelp revTokens)
            |> map Done
        , loop revTokens functionBody
            |> map Done
        , succeed (Done revTokens)
        ]


portDeclarationHelp : List Token -> String -> Parser (List Token)
portDeclarationHelp revTokens str =
    if str == "module" then
        moduleDeclaration
            |> loop (( T.C Keyword, str ) :: revTokens)

    else
        functionSignature
            |> loop (( T.C Function, str ) :: revTokens)



-- Function Signature


functionSignature : List Token -> Parser (Step (List Token) (List Token))
functionSignature revTokens =
    oneOf
        [ symbol ":"
            |> map (always (( T.C BasicSymbol, ":" ) :: revTokens))
            |> andThen (\ns -> loop ns fnSigContent)
            |> map Done
        , whitespaceOrCommentStep revTokens
        , loop revTokens functionBody
            |> map Done
        , succeed (Done revTokens)
        ]


fnSigContent : List Token -> Parser (Step (List Token) (List Token))
fnSigContent revTokens =
    oneOf
        [ whitespaceOrCommentStep revTokens
        , fnSigContentHelp
            |> map (\n -> Loop (n :: revTokens))
        , succeed (Done revTokens)
        ]


fnSigContentHelp : Parser Token
fnSigContentHelp =
    oneOf
        [ symbol "()" |> map (always ( T.C TypeSignature, "()" ))
        , symbol "->" |> map (always ( T.C BasicSymbol, "->" ))
        , chompIfThenWhile (\c -> c == '(' || c == ')' || c == '-' || c == ',')
            |> getChompedString
            |> map (\b -> ( T.Normal, b ))
        , chompIf Char.isUpper
            |> thenChompWhile fnSigIsNotRelevant
            |> getChompedString
            |> map (\b -> ( T.C TypeSignature, b ))
        , chompIfThenWhile fnSigIsNotRelevant
            |> getChompedString
            |> map (\b -> ( T.Normal, b ))
        ]


fnSigIsNotRelevant : Char -> Bool
fnSigIsNotRelevant c =
    not (isWhitespace c || c == '(' || c == ')' || c == '-' || c == ',')



-- Function Body


functionBody : List Token -> Parser (Step (List Token) (List Token))
functionBody revTokens =
    oneOf
        [ whitespaceOrCommentStep revTokens
        , stringLiteral
            |> map (\ns -> Loop (ns ++ revTokens))
        , functionBodyContent
            |> map (\n -> Loop (n :: revTokens))
        , succeed (Done revTokens)
        ]


functionBodyContent : Parser Token
functionBodyContent =
    oneOf
        [ number
            |> getChompedString
            |> map (\b -> ( T.C Number, b ))
        , symbol "()" |> map (always ( T.C Capitalized, "()" ))
        , infixParser
        , basicSymbol |> map (\b -> ( T.C BasicSymbol, b ))
        , groupSymbol |> map (\b -> ( T.C GroupSymbol, b ))
        , capitalized |> map (\b -> ( T.C Capitalized, b ))
        , variable
            |> map
                (\n ->
                    if isKeyword n then
                        ( T.C Keyword, n )

                    else
                        ( T.Normal, n )
                )
        , weirdText |> map (\b -> ( T.Normal, b ))
        ]


isKeyword : String -> Bool
isKeyword str =
    Set.member str keywordSet


keywordSet : Set String
keywordSet =
    Set.fromList
        [ "as"
        , "where"
        , "let"
        , "in"
        , "if"
        , "else"
        , "then"
        , "case"
        , "of"
        , "type"
        , "alias"
        ]


basicSymbol : Parser String
basicSymbol =
    chompIfThenWhile isBasicSymbol
        |> getChompedString


isBasicSymbol : Char -> Bool
isBasicSymbol c =
    Set.member c basicSymbols


basicSymbols : Set Char
basicSymbols =
    Set.fromList
        [ '|'
        , '.'
        , '='
        , '\\'
        , '/'
        , '('
        , ')'
        , '-'
        , '>'
        , '<'
        , ':'
        , '+'
        , '!'
        , '$'
        , '%'
        , '&'
        , '*'
        ]


groupSymbol : Parser String
groupSymbol =
    chompIfThenWhile isGroupSymbol
        |> getChompedString


isGroupSymbol : Char -> Bool
isGroupSymbol c =
    Set.member c groupSymbols


groupSymbols : Set Char
groupSymbols =
    Set.fromList
        [ ','
        , '['
        , ']'
        , '{'
        , '}'
        ]


capitalized : Parser String
capitalized =
    chompIf Char.isUpper
        |> thenChompWhile isVariableChar
        |> getChompedString


variable : Parser String
variable =
    chompIf Char.isLower
        |> thenChompWhile isVariableChar
        |> getChompedString


isVariableChar : Char -> Bool
isVariableChar c =
    not
        (isWhitespace c
            || isBasicSymbol c
            || isGroupSymbol c
            || isStringLiteralChar c
        )


weirdText : Parser String
weirdText =
    chompIfThenWhile isVariableChar
        |> getChompedString



-- Infix


infixParser : Parser Token
infixParser =
    (getChompedString <|
        succeed ()
            |. backtrackable (symbol "(")
            |. backtrackable (chompIfThenWhile isInfixChar)
            |. backtrackable (symbol ")")
    )
        |> map (\b -> ( T.C Function, b ))


isInfixChar : Char -> Bool
isInfixChar c =
    Set.member c infixSet


infixSet : Set Char
infixSet =
    Set.fromList
        [ '+'
        , '-'
        , '/'
        , '*'
        , '='
        , '.'
        , '$'
        , '<'
        , '>'
        , ':'
        , '&'
        , '|'
        , '^'
        , '?'
        , '%'
        , '#'
        , '@'
        , '~'
        , '!'
        , ','
        ]



-- String/Char literals


stringLiteral : Parser (List Token)
stringLiteral =
    oneOf
        [ tripleDoubleQuote
        , doubleQuote
        , quote
        ]


doubleQuote : Parser (List Token)
doubleQuote =
    delimited stringDelimiter


stringDelimiter : Delimiter Token
stringDelimiter =
    { start = "\""
    , end = "\""
    , isNestable = False
    , defaultMap = \b -> ( T.C String, b )
    , innerParsers = [ lineBreakList, elmEscapable ]
    , isNotRelevant = \c -> not (isLineBreak c || isEscapable c)
    }


tripleDoubleQuote : Parser (List Token)
tripleDoubleQuote =
    delimited
        { stringDelimiter
            | start = "\"\"\""
            , end = "\"\"\""
        }


quote : Parser (List Token)
quote =
    delimited
        { stringDelimiter
            | start = "'"
            , end = "'"
        }


isStringLiteralChar : Char -> Bool
isStringLiteralChar c =
    c == '"' || c == '\''



-- Comments


comment : Parser (List Token)
comment =
    oneOf
        [ inlineComment
        , multilineComment
        ]


inlineComment : Parser (List Token)
inlineComment =
    symbol "--"
        |> thenChompWhile (not << isLineBreak)
        |> getChompedString
        |> map (\b -> [ ( T.Comment, b ) ])


multilineComment : Parser (List Token)
multilineComment =
    delimited
        { start = "{-"
        , end = "-}"
        , isNestable = True
        , defaultMap = \b -> ( T.Comment, b )
        , innerParsers = [ lineBreakList ]
        , isNotRelevant = \c -> not (isLineBreak c)
        }


commentChar : Parser String
commentChar =
    chompIf isCommentChar
        |> getChompedString


isCommentChar : Char -> Bool
isCommentChar c =
    c == '-' || c == '{'



-- Helpers


whitespaceOrCommentStep : List Token -> Parser (Step (List Token) (List Token))
whitespaceOrCommentStep revTokens =
    oneOf
        [ space
            |> map (\n -> Loop (n :: revTokens))
        , lineBreak
            |> map (\n -> n :: revTokens)
            |> andThen checkContext
        , comment
            |> map (\n -> Loop (n ++ revTokens))
        ]


checkContext : List Token -> Parser (Step (List Token) (List Token))
checkContext revTokens =
    oneOf
        [ whitespaceOrCommentStep revTokens
        , succeed (Done revTokens)
        ]


whitespaceOrCommentStepNested : ( Int, List Token ) -> Parser (Step ( Int, List Token ) (List Token))
whitespaceOrCommentStepNested ( nestLevel, revTokens ) =
    oneOf
        [ space
            |> map (\n -> Loop ( nestLevel, n :: revTokens ))
        , lineBreak
            |> map (\n -> ( nestLevel, n :: revTokens ))
            |> andThen checkContextNested
        , comment
            |> map (\n -> Loop ( nestLevel, n ++ revTokens ))
        ]


checkContextNested : ( Int, List Token ) -> Parser (Step ( Int, List Token ) (List Token))
checkContextNested ( nestLevel, revTokens ) =
    oneOf
        [ whitespaceOrCommentStepNested ( nestLevel, revTokens )
        , succeed (Done revTokens)
        ]


space : Parser Token
space =
    chompIfThenWhile isSpace
        |> getChompedString
        |> map (\b -> ( T.Normal, b ))


lineBreak : Parser Token
lineBreak =
    symbol "\n"
        |> map (\_ -> ( T.LineBreak, "\n" ))


lineBreakList : Parser (List Token)
lineBreakList =
    symbol "\n"
        |> map (\_ -> [ ( T.LineBreak, "\n" ) ])


elmEscapable : Parser (List Token)
elmEscapable =
    escapable
        |> getChompedString
        |> map (\b -> [ ( T.C Capitalized, b ) ])


syntaxToStyle : Syntax -> ( Style.Required, String )
syntaxToStyle syntax =
    case syntax of
        String ->
            ( Style2, "elm-s" )

        BasicSymbol ->
            ( Style3, "elm-bs" )

        GroupSymbol ->
            ( Style4, "elm-gs" )

        Capitalized ->
            ( Style6, "elm-c" )

        Keyword ->
            ( Style3, "elm-k" )

        Function ->
            ( Style5, "elm-f" )

        TypeSignature ->
            ( Style4, "elm-ts" )

        Number ->
            ( Style1, "elm-n" )
