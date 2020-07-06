module SyntaxHighlight.Language.Javascript exposing
    ( Syntax(..)
    ,  syntaxToStyle
       -- Exposing for tests purpose

    , toLines
    , toRevTokens
    )

import Parser exposing ((|.), DeadEnd, Parser, Step(..), andThen, chompIf, getChompedString, keyword, loop, map, oneOf, succeed, symbol)
import Set exposing (Set)
import SyntaxHighlight.Language.Helpers exposing (Delimiter, chompIfThenWhile, delimited, escapable, isEscapable, isLineBreak, isSpace, isWhitespace, thenChompWhile)
import SyntaxHighlight.Language.Type as T
import SyntaxHighlight.Line exposing (Line)
import SyntaxHighlight.Line.Helpers as Line
import SyntaxHighlight.Style as Style exposing (Required(..))


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
    | ClassExtends


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
    if n == "function" || n == "static" then
        loop (( T.C DeclarationKeyword, n ) :: revTokens) functionDeclarationLoop

    else if n == "class" then
        loop (( T.C DeclarationKeyword, n ) :: revTokens) classDeclarationLoop

    else if n == "this" || n == "super" then
        succeed (( T.C Param, n ) :: revTokens)

    else if n == "constructor" then
        loop (( T.C Function, n ) :: revTokens) functionDeclarationLoop

    else if isKeyword n then
        succeed (( T.C Keyword, n ) :: revTokens)

    else if isDeclarationKeyword n then
        succeed (( T.C DeclarationKeyword, n ) :: revTokens)

    else if isLiteralKeyword n then
        succeed (( T.C LiteralKeyword, n ) :: revTokens)

    else
        loop [] (functionEvalLoop n revTokens)


functionDeclarationLoop : List Token -> Parser (Step (List Token) (List Token))
functionDeclarationLoop revTokens =
    oneOf
        [ whitespaceOrCommentStep revTokens
        , chompIfThenWhile isIdentifierNameChar
            |> getChompedString
            |> map (\b -> Loop (( T.C Function, b ) :: revTokens))
        , symbol "*"
            |> map (\_ -> Loop (( T.C Keyword, "*" ) :: revTokens))
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


functionEvalLoop : String -> List Token -> List Token -> Parser (Step (List Token) (List Token))
functionEvalLoop identifier revTokens thisRevToken =
    oneOf
        [ whitespaceOrCommentStep thisRevToken
        , symbol "("
            |> map
                (\_ -> Done ((( T.Normal, "(" ) :: thisRevToken) ++ (( T.C FunctionEval, identifier ) :: revTokens)))
        , thisRevToken
            ++ (( T.Normal, identifier ) :: revTokens)
            |> Done
            |> succeed
        ]


classDeclarationLoop : List Token -> Parser (Step (List Token) (List Token))
classDeclarationLoop revTokens =
    oneOf
        [ whitespaceOrCommentStep revTokens
        , chompIfThenWhile isIdentifierNameChar
            |> getChompedString
            |> andThen
                (\n ->
                    if n == "extends" then
                        loop (( T.C Keyword, n ) :: revTokens) classExtendsLoop
                            |> map Loop

                    else
                        succeed (Loop (( T.C Function, n ) :: revTokens))
                )
        , succeed (Done revTokens)
        ]


classExtendsLoop : List Token -> Parser (Step (List Token) (List Token))
classExtendsLoop revTokens =
    oneOf
        [ whitespaceOrCommentStep revTokens
        , chompIfThenWhile isIdentifierNameChar
            |> getChompedString
            |> map (\b -> Loop (( T.C ClassExtends, b ) :: revTokens))
        , succeed (Done revTokens)
        ]


isIdentifierNameChar : Char -> Bool
isIdentifierNameChar c =
    not
        (isPunctuaction c
            || isStringLiteralChar c
            || isCommentChar c
            || isWhitespace c
        )



-- Reserved Words


isKeyword : String -> Bool
isKeyword str =
    Set.member str keywordSet


keywordSet : Set String
keywordSet =
    Set.fromList
        [ "break"
        , "do"
        , "instanceof"
        , "typeof"
        , "case"
        , "else"
        , "new"
        , "catch"
        , "finally"
        , "return"
        , "void"
        , "continue"
        , "for"
        , "switch"
        , "while"
        , "debugger"
        , "this"
        , "with"
        , "default"
        , "if"
        , "throw"
        , "delete"
        , "in"
        , "try"
        , "enum"
        , "extends"
        , "export"
        , "import"
        , "implements"
        , "private"
        , "public"
        , "yield"
        , "interface"
        , "package"
        , "protected"
        ]


isDeclarationKeyword : String -> Bool
isDeclarationKeyword str =
    Set.member str declarationKeywordSet


declarationKeywordSet : Set String
declarationKeywordSet =
    Set.fromList
        [ "var"
        , "const"
        , "let"
        ]


isPunctuaction : Char -> Bool
isPunctuaction c =
    Set.member c punctuactorSet


punctuactorSet : Set Char
punctuactorSet =
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
        [ "true"
        , "false"
        , "null"
        , "undefined"
        , "NaN"
        , "Infinity"
        ]



-- String literal


stringLiteral : Parser (List Token)
stringLiteral =
    oneOf
        [ quote
        , doubleQuote
        , templateString
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
    , innerParsers = [ lineBreakList, jsEscapable ]
    , isNotRelevant = \c -> not (isLineBreak c || isEscapable c)
    }


doubleQuote : Parser (List Token)
doubleQuote =
    delimited
        { quoteDelimiter
            | start = "\""
            , end = "\""
        }


templateString : Parser (List Token)
templateString =
    delimited
        { quoteDelimiter
            | start = "`"
            , end = "`"
            , innerParsers = [ lineBreakList, jsEscapable ]
            , isNotRelevant = \c -> not (isLineBreak c || isEscapable c)
        }


isStringLiteralChar : Char -> Bool
isStringLiteralChar c =
    c == '"' || c == '\'' || c == '`'



-- Comments


comment : Parser (List Token)
comment =
    oneOf
        [ inlineComment
        , multilineComment
        ]


inlineComment : Parser (List Token)
inlineComment =
    symbol "//"
        |> thenChompWhile (not << isLineBreak)
        |> getChompedString
        |> map (\b -> [ ( T.Comment, b ) ])


multilineComment : Parser (List Token)
multilineComment =
    delimited
        { start = "/*"
        , end = "*/"
        , isNestable = False
        , defaultMap = \b -> ( T.Comment, b )
        , innerParsers = [ lineBreakList ]
        , isNotRelevant = \c -> not (isLineBreak c)
        }


isCommentChar : Char -> Bool
isCommentChar c =
    c == '/'



-- Helpers


whitespaceOrCommentStep : List Token -> Parser (Step (List Token) (List Token))
whitespaceOrCommentStep revTokens =
    oneOf
        [ chompIfThenWhile isSpace
            |> getChompedString
            |> map (\b -> Loop (( T.Normal, b ) :: revTokens))
        , lineBreakList
            |> map (\ns -> Loop (ns ++ revTokens))
        , comment
            |> map (\ns -> Loop (ns ++ revTokens))
        ]


lineBreakList : Parser (List Token)
lineBreakList =
    symbol "\n"
        |> map (\_ -> [ ( T.LineBreak, "\n" ) ])


number : Parser Token
number =
    SyntaxHighlight.Language.Helpers.number
        |> getChompedString
        |> map (\b -> ( T.C Number, b ))


jsEscapable : Parser (List Token)
jsEscapable =
    escapable
        |> getChompedString
        |> map (\b -> [ ( T.C LiteralKeyword, b ) ])


syntaxToStyle : Syntax -> ( Style.Required, String )
syntaxToStyle syntax =
    case syntax of
        Number ->
            ( Style1, "js-n" )

        String ->
            ( Style2, "js-s" )

        Keyword ->
            ( Style3, "js-k" )

        DeclarationKeyword ->
            ( Style4, "js-dk" )

        FunctionEval ->
            ( Style4, "js-fe" )

        Function ->
            ( Style5, "js-f" )

        LiteralKeyword ->
            ( Style6, "js-lk" )

        Param ->
            ( Style7, "js-p" )

        ClassExtends ->
            ( Style5, "js-ce" )
