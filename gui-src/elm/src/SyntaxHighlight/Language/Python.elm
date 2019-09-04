module SyntaxHighlight.Language.Python exposing
    ( Syntax(..)
    ,  syntaxToStyle
       -- Exposing for tests purpose

    , toLines
    , toRevTokens
    )

import Parser exposing ((|.), DeadEnd, Parser, Step(..), andThen, chompIf, getChompedString, keyword, loop, map, oneOf, succeed, symbol)
import Set exposing (Set)
import SyntaxHighlight.Language.Helpers exposing (Delimiter, addThen, chompIfThenWhile, consThen, delimited, isEscapable, isLineBreak, isSpace, isWhitespace, thenChompWhile)
import SyntaxHighlight.Language.Type as T
import SyntaxHighlight.Line exposing (Line)
import SyntaxHighlight.Line.Helpers as Line
import SyntaxHighlight.Style as Style exposing (Required(..))



-- Author: brandly (https://github.com/brandly)


type alias Token =
    T.Token Syntax


type Syntax
    = Number
    | String
    | Keyword
    | DeclarationKeyword
    | FunctionEval
    | Function
    | LiteralKeyword
    | Param


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
        [ whitespaceOrCommentStep revTokens
        , stringLiteral
            |> map (\s -> Loop (s ++ revTokens))
        , oneOf
            [ operatorChar
            , groupChar
            , number
            ]
            |> map (\s -> Loop (s :: revTokens))
        , chompIfThenWhile isIdentifierNameChar
            |> getChompedString
            |> andThen (keywordParser revTokens)
            |> map Loop
        , succeed (Done revTokens)
        ]


keywordParser : List Token -> String -> Parser (List Token)
keywordParser revTokens n =
    if n == "def" then
        loop (( T.C DeclarationKeyword, n ) :: revTokens) functionDeclarationLoop

    else if n == "class" then
        loop (( T.C DeclarationKeyword, n ) :: revTokens) classDeclarationLoop

    else if isKeyword n then
        succeed (( T.C Keyword, n ) :: revTokens)

    else if isLiteralKeyword n then
        succeed (( T.C LiteralKeyword, n ) :: revTokens)

    else
        loop (( T.C FunctionEval, n ) :: revTokens) functionEvalLoop


functionDeclarationLoop : List Token -> Parser (Step (List Token) (List Token))
functionDeclarationLoop revTokens =
    oneOf
        [ whitespaceOrCommentStep revTokens
        , chompIfThenWhile isIdentifierNameChar
            |> getChompedString
            |> map (\b -> Loop (( T.C Function, b ) :: revTokens))
        , symbol "("
            |> andThen
                (\_ -> loop (( T.Normal, "(" ) :: revTokens) argLoop)
            |> map Loop
        , succeed (Done revTokens)
        ]


argLoop : List Token -> Parser (Step (List Token) (List Token))
argLoop revTokens =
    oneOf
        [ whitespaceOrCommentStep revTokens
        , chompIfThenWhile (\c -> not (isCommentChar c || isWhitespace c || c == ',' || c == ')'))
            |> getChompedString
            |> map (\b -> Loop (( T.C Param, b ) :: revTokens))
        , chompIfThenWhile (\c -> c == '/' || c == ',')
            |> getChompedString
            |> map (\b -> Loop (( T.Normal, b ) :: revTokens))
        , succeed (Done revTokens)
        ]


functionEvalLoop : List Token -> Parser (Step (List Token) (List Token))
functionEvalLoop revTokens =
    oneOf
        [ whitespaceOrCommentStep revTokens
        , symbol "("
            |> map (\_ -> Done (( T.Normal, "(" ) :: revTokens))
        , succeed (Done revTokens)
        ]


classDeclarationLoop : List Token -> Parser (Step (List Token) (List Token))
classDeclarationLoop revTokens =
    -- TODO: handle base classes
    oneOf
        [ whitespaceOrCommentStep revTokens
        , chompIfThenWhile isIdentifierNameChar
            |> getChompedString
            |> map (\b -> Loop (( T.C Function, b ) :: revTokens))
        , succeed (Done revTokens)
        ]


isIdentifierNameChar : Char -> Bool
isIdentifierNameChar c =
    not
        (isPunctuation c
            || isStringLiteralChar c
            || isCommentChar c
            || isWhitespace c
        )



-- Reserved words


isKeyword : String -> Bool
isKeyword str =
    Set.member str keywordSet


keywordSet : Set String
keywordSet =
    Set.fromList
        [ "finally"
        , "is"
        , "return"
        , "continue"
        , "for"
        , "lambda"
        , "try"
        , "from"
        , "nonlocal"
        , "while"
        , "and"
        , "del"
        , "global"
        , "not"
        , "with"
        , "as"
        , "elif"
        , "if"
        , "or"
        , "yield"
        , "assert"
        , "else"
        , "import"
        , "pass"
        , "break"
        , "except"
        , "in"
        , "raise"
        ]


isPunctuation : Char -> Bool
isPunctuation c =
    Set.member c punctuationSet


punctuationSet : Set Char
punctuationSet =
    Set.union operatorSet groupSet


operatorChar : Parser Token
operatorChar =
    chompIfThenWhile isOperatorChar
        |> getChompedString
        |> map (\b -> ( T.C Keyword, b ))


isOperatorChar : Char -> Bool
isOperatorChar c =
    Set.member c operatorSet


operatorSet : Set Char
operatorSet =
    Set.fromList
        [ '+'
        , '-'
        , '*'
        , '/'
        , '='
        , '!'
        , '<'
        , '>'
        , '&'
        , '|'
        , '?'
        , '^'
        , ':'
        , '~'
        , '%'
        , '.'
        ]


groupChar : Parser Token
groupChar =
    chompIfThenWhile isGroupChar
        |> getChompedString
        |> map (\b -> ( T.Normal, b ))


isGroupChar : Char -> Bool
isGroupChar c =
    Set.member c groupSet


groupSet : Set Char
groupSet =
    Set.fromList
        [ '{'
        , '}'
        , '('
        , ')'
        , '['
        , ']'
        , ','
        , ';'
        ]


isLiteralKeyword : String -> Bool
isLiteralKeyword str =
    Set.member str literalKeywordSet


literalKeywordSet : Set String
literalKeywordSet =
    Set.fromList
        [ "True"
        , "False"
        , "None"
        ]



-- String


stringLiteral : Parser (List Token)
stringLiteral =
    -- TODO: shortstring | longstring
    oneOf
        [ quote
        , doubleQuote
        ]


quote : Parser (List Token)
quote =
    delimited quoteDelimiter


quoteDelimiter : Delimiter Token
quoteDelimiter =
    { start = "'"
    , end = "'"
    , isNestable = False
    , defaultMap = \b -> ( T.C String, b )

    -- TODO: escapable chars
    , innerParsers = [ lineBreak ]
    , isNotRelevant = \c -> not (isLineBreak c || isEscapable c)
    }


doubleQuote : Parser (List Token)
doubleQuote =
    delimited
        { quoteDelimiter
            | start = "\""
            , end = "\""
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
    symbol "#"
        |> thenChompWhile (not << isLineBreak)
        |> getChompedString
        |> map (\b -> [ ( T.Comment, b ) ])


multilineComment : Parser (List Token)
multilineComment =
    -- TODO: might not need this at all. just parse as multiline string?
    delimited
        { start = "'''"
        , end = "'''"
        , isNestable = False
        , defaultMap = \b -> ( T.Comment, b )
        , innerParsers = [ lineBreak ]
        , isNotRelevant = \c -> not (isLineBreak c)
        }


isCommentChar : Char -> Bool
isCommentChar c =
    c == '#'



-- Helpers


whitespaceOrCommentStep : List Token -> Parser (Step (List Token) (List Token))
whitespaceOrCommentStep revTokens =
    oneOf
        [ chompIfThenWhile isSpace
            |> getChompedString
            |> map (\s -> Loop (( T.Normal, s ) :: revTokens))
        , lineBreak
            |> map (\ns -> Loop (ns ++ revTokens))
        , comment
            |> map (\ns -> Loop (ns ++ revTokens))
        ]


lineBreak : Parser (List Token)
lineBreak =
    symbol "\n"
        |> map (\_ -> [ ( T.LineBreak, "\n" ) ])


number : Parser Token
number =
    SyntaxHighlight.Language.Helpers.number
        |> getChompedString
        |> map (\b -> ( T.C Number, b ))


syntaxToStyle : Syntax -> ( Style.Required, String )
syntaxToStyle syntax =
    case syntax of
        Number ->
            ( Style1, "py-n" )

        String ->
            ( Style2, "py-s" )

        Keyword ->
            ( Style3, "py-k" )

        DeclarationKeyword ->
            ( Style4, "py-dk" )

        Function ->
            ( Style5, "py-f" )

        LiteralKeyword ->
            ( Style6, "py-lk" )

        Param ->
            ( Style7, "py-p" )

        FunctionEval ->
            ( Default, "py-fe" )
