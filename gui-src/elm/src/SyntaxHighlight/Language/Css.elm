module SyntaxHighlight.Language.Css exposing
    ( AtRule(..)
    , AttributeSelector(..)
    , Selector(..)
    , Syntax(..)
    ,  syntaxToStyle
       -- Exposing for tests purpose

    , toLines
    , toRevTokens
    )

import Parser exposing ((|.), DeadEnd, Parser, Step(..), andThen, chompIf, getChompedString, loop, map, oneOf, succeed, symbol)
import Set exposing (Set)
import SyntaxHighlight.Language.Helpers exposing (Delimiter, chompIfThenWhile, delimited, escapable, isEscapable, isLineBreak, isSpace, isWhitespace, thenChompWhile, whitespaceCharSet)
import SyntaxHighlight.Language.Type as T
import SyntaxHighlight.Line exposing (Line)
import SyntaxHighlight.Line.Helpers as Line
import SyntaxHighlight.Style as Style exposing (Required(..))


type alias Token =
    T.Token Syntax


type Syntax
    = String
    | AtRule AtRule
    | Selector Selector
    | Property
    | PropertyValue
    | Number
    | Unit


type AtRule
    = Identifier
    | Prefix
    | Keyword
    | AtRuleValue


type Selector
    = Element
    | Id
    | Class
    | Combinator
    | Universal
    | AttributeSelector AttributeSelector
    | PseudoElement
    | PseudoClass


type AttributeSelector
    = AttributeName
    | AttributeValue
    | AttributeOperator


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
        , atRule |> map (\n -> Loop (n ++ revTokens))
        , selector |> map (\n -> Loop (n ++ revTokens))
        , declarationBlock |> map (\n -> Loop (n ++ revTokens))
        , chompIf (always True)
            |> getChompedString
            |> map (\b -> Loop (( T.Normal, b ) :: revTokens))
        , succeed (Done revTokens)
        ]



-- At-Rules


atRule : Parser (List Token)
atRule =
    symbol "@"
        |> thenChompWhile isSelectorNameChar
        |> getChompedString
        |> andThen atRuleHelper


atRuleHelper : String -> Parser (List Token)
atRuleHelper a =
    case a of
        "@import" ->
            (\ns ->
                oneOf
                    [ whitespaceOrCommentStep ns
                    , stringArg "url" ns |> map Loop
                    , stringLiteral ns |> map Loop
                    , atRuleKeywordOrValue ns |> map Loop
                    , chompIf (\c -> c /= ';')
                        |> getChompedString
                        |> map (\b -> Loop (( T.Normal, b ) :: ns))
                    , succeed (Done ns)
                    ]
            )
                |> loop [ ( T.C (AtRule Identifier), a ) ]

        "@namespace" ->
            (\ns ->
                oneOf
                    [ whitespaceOrCommentStep ns
                    , stringArg "url" ns |> map Loop
                    , stringLiteral ns |> map Loop
                    , chompIfThenWhile isSelectorNameChar
                        |> getChompedString
                        |> map (\b -> Loop (( T.C (AtRule Prefix), b ) :: ns))
                    , chompIf (\c -> c /= ';')
                        |> getChompedString
                        |> map (\b -> Loop (( T.Normal, b ) :: ns))
                    , succeed (Done ns)
                    ]
            )
                |> loop [ ( T.C (AtRule Identifier), a ) ]

        "@charset" ->
            (\ns ->
                oneOf
                    [ whitespaceOrCommentStep ns
                    , stringLiteral ns |> map Loop
                    , chompIfThenWhile isSelectorNameChar
                        |> getChompedString
                        |> map (\b -> Loop (( T.C String, b ) :: ns))
                    , chompIf (\c -> c /= ';')
                        |> getChompedString
                        |> map (\b -> Loop (( T.Normal, b ) :: ns))
                    , succeed (Done ns)
                    ]
            )
                |> loop [ ( T.C (AtRule Identifier), a ) ]

        "@media" ->
            mediaOrSupports a

        "@supports" ->
            mediaOrSupports a

        "@keyframes" ->
            keyframesOrCounterStyle a
                |> andThen nestableAtRuleOpener

        "@counter-style" ->
            keyframesOrCounterStyle a

        "@font-feature-values" ->
            (\ns ->
                oneOf
                    [ whitespaceOrCommentStep ns
                    , chompIfThenWhile isSelectorNameChar
                        |> getChompedString
                        |> map (\b -> Loop (( T.C (AtRule Prefix), b ) :: ns))
                    , chompIf (\c -> c /= '{')
                        |> getChompedString
                        |> map (\b -> Loop (( T.Normal, b ) :: ns))
                    , succeed (Done ns)
                    ]
            )
                |> loop [ ( T.C (AtRule Identifier), a ) ]
                |> andThen nestableAtRuleOpener

        _ ->
            if Set.member a atRuleSet then
                succeed [ ( T.C (AtRule Identifier), a ) ]

            else
                succeed [ ( T.Normal, a ) ]


atRuleKeywordOrValue : List Token -> Parser (List Token)
atRuleKeywordOrValue revTokens =
    chompIfThenWhile isSelectorNameChar
        |> getChompedString
        |> map
            (\n ->
                if isAtRuleKeyword n then
                    ( T.C (AtRule Keyword), n ) :: revTokens

                else
                    ( T.C (AtRule AtRuleValue), n ) :: revTokens
            )


mediaOrSupports : String -> Parser (List Token)
mediaOrSupports a =
    (\ns ->
        oneOf
            [ whitespaceOrCommentStep ns
            , stringLiteral ns |> map Loop
            , atRuleKeywordOrValue ns |> map Loop
            , chompIf (\c -> c /= '{')
                |> getChompedString
                |> map (\b -> Loop (( T.Normal, b ) :: ns))
            , succeed (Done ns)
            ]
    )
        |> loop [ ( T.C (AtRule Identifier), a ) ]
        |> andThen nestableAtRuleOpener


keyframesOrCounterStyle : String -> Parser (List Token)
keyframesOrCounterStyle a =
    (\ns ->
        oneOf
            [ whitespaceOrCommentStep ns
            , chompIfThenWhile isSelectorNameChar
                |> getChompedString
                |> map (\b -> Loop (( T.C (AtRule Prefix), b ) :: ns))
            , chompIf (\c -> c /= '{')
                |> getChompedString
                |> map (\b -> Loop (( T.Normal, b ) :: ns))
            , succeed (Done ns)
            ]
    )
        |> loop [ ( T.C (AtRule Identifier), a ) ]


nestableAtRuleOpener : List Token -> Parser (List Token)
nestableAtRuleOpener ns =
    oneOf
        [ symbol "{"
            |> map (always (( T.Normal, "{" ) :: ns))
        , succeed ns
        ]


isAtRuleKeyword : String -> Bool
isAtRuleKeyword n =
    Set.member n atRuleKeywordSet


atRuleKeywordSet : Set String
atRuleKeywordSet =
    Set.fromList
        [ "and"
        , "or"
        , "not"
        , "only"
        ]


atRuleSet : Set String
atRuleSet =
    Set.fromList
        [ "@page"
        , "@font-face"

        -- Font-feature-values at-rules
        , "@swash"
        , "@annotation"
        , "@ornaments"
        , "@stylistic"
        , "@styleset"
        , "@character-variant"
        ]



-- Selectors


selector : Parser (List Token)
selector =
    oneOf
        [ oneOf
            [ id
            , class
            , element
            , universal
            , combinator
            , pseudoElement
            , pseudoClass
            ]
            |> map (\( n, s ) -> [ ( T.C (Selector n), s ) ])
        , attributeSelector
        ]


id : Parser ( Selector, String )
id =
    symbol "#"
        |> thenChompWhile isSelectorNameChar
        |> getChompedString
        |> map (\b -> ( Id, b ))


class : Parser ( Selector, String )
class =
    symbol "."
        |> thenChompWhile isSelectorNameChar
        |> getChompedString
        |> map (\b -> ( Class, b ))


element : Parser ( Selector, String )
element =
    chompIfThenWhile isSelectorNameChar
        |> getChompedString
        |> map (\b -> ( Element, b ))


universal : Parser ( Selector, String )
universal =
    symbol "*"
        |> map (always ( Universal, "*" ))


combinator : Parser ( Selector, String )
combinator =
    oneOf
        [ symbol "+"
        , symbol "~"
        , symbol ">"
        ]
        |> getChompedString
        |> map (\b -> ( Combinator, b ))


pseudoElement : Parser ( Selector, String )
pseudoElement =
    symbol "::"
        |> thenChompWhile isSelectorNameChar
        |> getChompedString
        |> map (\b -> ( PseudoElement, b ))


pseudoClass : Parser ( Selector, String )
pseudoClass =
    symbol ":"
        |> thenChompWhile isSelectorNameChar
        |> getChompedString
        |> map (\b -> ( PseudoClass, b ))


isSelectorNameChar : Char -> Bool
isSelectorNameChar c =
    isWhitespace c
        || isCommentChar c
        || Set.member c selectorNameInvalidCharSet
        |> not


selectorNameInvalidCharSet : Set Char
selectorNameInvalidCharSet =
    Set.fromList [ ':', '{', '}', ',', '.', '#', '>', '+', '~', '*', '[', ']', '|', ';', '(', ')' ]


attributeSelector : Parser (List Token)
attributeSelector =
    symbol "["
        |> map (always ( T.Normal, "[" ))
        |> andThen
            (\opener -> loop [ opener ] attributeSelectorLoop)


attributeSelectorLoop : List Token -> Parser (Step (List Token) (List Token))
attributeSelectorLoop revTokens =
    oneOf
        [ whitespaceOrCommentStep revTokens
        , attributeName |> map (\n -> Loop (n :: revTokens))
        , attributeOperator
            |> andThen
                (\operator ->
                    loop [ operator ] attributeValue
                        |> map (\n -> Loop (n ++ revTokens))
                )
        , succeed (Done revTokens)
        ]


attributeName : Parser Token
attributeName =
    chompIfThenWhile (\c -> not <| Set.member c attSelNameInvalidCharSet)
        |> getChompedString
        |> map (\b -> ( T.C (Selector (AttributeSelector AttributeName)), b ))


attSelNameInvalidCharSet : Set Char
attSelNameInvalidCharSet =
    Set.union attSelOperatorCharSet whitespaceCharSet
        |> Set.insert ']'


attSelOperatorCharSet : Set Char
attSelOperatorCharSet =
    Set.fromList [ '=', '~', '|', '^', '$', '*' ]


attributeOperator : Parser Token
attributeOperator =
    oneOf
        [ symbol "~="
        , symbol "|="
        , symbol "^="
        , symbol "$="
        , symbol "*="
        , symbol "="
        ]
        |> getChompedString
        |> map (\b -> ( T.C (Selector (AttributeSelector AttributeOperator)), b ))


attributeValue : List Token -> Parser (Step (List Token) (List Token))
attributeValue revTokens =
    oneOf
        [ whitespaceOrCommentStep revTokens
        , stringLiteral revTokens |> map Done
        , chompIfThenWhile (\c -> c /= ']' && not (isWhitespace c))
            |> getChompedString
            |> map (\b -> ( T.C (Selector (AttributeSelector AttributeValue)), b ) :: revTokens)
            |> map Done
        , succeed (Done revTokens)
        ]



-- Declaration Block


declarationBlock : Parser (List Token)
declarationBlock =
    chompIfThenWhile (\c -> c == '{')
        |> getChompedString
        |> map (\b -> ( T.Normal, b ))
        |> andThen declarationBlockHelper


declarationBlockHelper : Token -> Parser (List Token)
declarationBlockHelper opener =
    loop [ opener ] declarationLoop


declarationLoop : List Token -> Parser (Step (List Token) (List Token))
declarationLoop revTokens =
    oneOf
        [ whitespaceOrCommentStep revTokens
        , chompIfThenWhile isPropertyChar
            |> getChompedString
            |> map (\b -> Loop (( T.C Property, b ) :: revTokens))
        , chompIfThenWhile (\c -> c == ';' || c == '/')
            |> getChompedString
            |> map (\b -> Loop (( T.Normal, b ) :: revTokens))
        , value |> map (\n -> Loop (n ++ revTokens))
        , succeed (Done revTokens)
        ]


isPropertyChar : Char -> Bool
isPropertyChar c =
    not (isWhitespace c || isCommentChar c || c == ':' || c == ';' || c == '}')


value : Parser (List Token)
value =
    chompIfThenWhile ((==) ':')
        |> getChompedString
        |> map (\b -> ( T.Normal, b ))
        |> andThen valueHelper


valueHelper : Token -> Parser (List Token)
valueHelper opener =
    loop [ opener ] valueLoop


valueLoop : List Token -> Parser (Step (List Token) (List Token))
valueLoop revTokens =
    oneOf
        [ whitespaceOrCommentStep revTokens
        , stringLiteral revTokens |> map Loop
        , number |> map (\n -> Loop (n :: revTokens))
        , hexColor revTokens |> map Loop
        , stringArg "url" revTokens |> map Loop
        , stringArg "format" revTokens |> map Loop
        , stringArg "local" revTokens |> map Loop
        , chompIfThenWhile isPropertyValueChar
            |> getChompedString
            |> map
                (\n ->
                    if isUnit n then
                        Loop (( T.C Unit, n ) :: revTokens)

                    else
                        Loop (( T.C PropertyValue, n ) :: revTokens)
                )
        , chompIfThenWhile isNotPropertyValueChar
            |> getChompedString
            |> map (\b -> Loop (( T.Normal, b ) :: revTokens))
        , chompIfThenWhile isOperatorChar
            |> getChompedString
            |> map (\b -> Loop (( T.C Unit, b ) :: revTokens))
        , succeed (Done revTokens)
        ]


hexColor : List Token -> Parser (List Token)
hexColor revTokens =
    symbol "#"
        |> thenChompWhile isPropertyValueChar
        |> getChompedString
        |> map (\n -> ( T.C Number, n ) :: revTokens)


stringArg : String -> List Token -> Parser (List Token)
stringArg fnStr revTokens =
    symbol (fnStr ++ "(")
        |> map (always (( T.Normal, "(" ) :: ( T.C PropertyValue, fnStr ) :: revTokens))
        |> andThen
            (\revT_ ->
                oneOf
                    [ stringLiteral revT_
                    , chompIfThenWhile (\c -> c /= ')')
                        |> getChompedString
                        |> map (\n -> ( T.C String, n ) :: revT_)
                    , succeed revT_
                    ]
            )


isPropertyValueChar : Char -> Bool
isPropertyValueChar c =
    isPropertyChar c && not (c == '(' || c == ')' || c == ',' || isOperatorChar c)


isNotPropertyValueChar : Char -> Bool
isNotPropertyValueChar c =
    c == '(' || c == ')' || c == ':' || c == ',' || c == '/'


isUnit : String -> Bool
isUnit n =
    Set.member n unitSet


unitSet : Set String
unitSet =
    Set.fromList
        [ "em"
        , "ex"
        , "ch"
        , "rem"
        , "vw"
        , "vh"
        , "vmin"
        , "vmax"
        , "cm"
        , "mm"
        , "q"
        , "in"
        , "pt"
        , "pc"
        , "px"
        , "deg"
        , "grad"
        , "rad"
        , "turn"
        , "s"
        , "ms"
        , "Hz"
        , "kHz"
        , "dpi"
        , "dpcm"
        , "dppx"
        ]


isOperatorChar : Char -> Bool
isOperatorChar c =
    Set.member c operatorCharSet


operatorCharSet : Set Char
operatorCharSet =
    Set.fromList
        [ '+'
        , '-'
        , '%'
        , '*'
        , '/'
        ]



-- String literal


stringLiteral : List Token -> Parser (List Token)
stringLiteral revTokens =
    oneOf
        [ quote
        , doubleQuote
        ]
        |> map (\n -> n ++ revTokens)


quote : Parser (List Token)
quote =
    delimited quoteDelimiter


quoteDelimiter : Delimiter Token
quoteDelimiter =
    { start = "'"
    , end = "'"
    , isNestable = False
    , defaultMap = \b -> ( T.C String, b )
    , innerParsers = [ lineBreak, cssEscapable ]
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



-- T.Comment


comment : Parser (List Token)
comment =
    delimited
        { start = "/*"
        , end = "*/"
        , isNestable = False
        , defaultMap = \b -> ( T.Comment, b )
        , innerParsers = [ lineBreak ]
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


cssEscapable : Parser (List Token)
cssEscapable =
    escapable
        |> getChompedString
        |> map (\b -> [ ( T.C Number, b ) ])


syntaxToStyle : Syntax -> ( Style.Required, String )
syntaxToStyle syntax =
    case syntax of
        String ->
            ( Style2, "css-s" )

        AtRule a ->
            atRuleToFragment a

        Selector s ->
            selectorToFragment s

        Property ->
            ( Style4, "css-p" )

        PropertyValue ->
            ( Style4, "css-pv" )

        Number ->
            ( Style1, "css-n" )

        Unit ->
            ( Style3, "css-u" )


atRuleToFragment : AtRule -> ( Style.Required, String )
atRuleToFragment a =
    case a of
        Identifier ->
            ( Style3, "css-ar-i" )

        Prefix ->
            ( Style5, "css-ar-p" )

        Keyword ->
            ( Style3, "css-ar-k" )

        AtRuleValue ->
            ( Style4, "css-ar-v" )


selectorToFragment : Selector -> ( Style.Required, String )
selectorToFragment s =
    case s of
        Element ->
            ( Style3, "css-s-e" )

        Id ->
            ( Style5, "css-s-i" )

        Class ->
            ( Style5, "css-s-cl" )

        Combinator ->
            ( Style7, "css-s-c" )

        Universal ->
            ( Style3, "css-s-u" )

        AttributeSelector att ->
            attributeSelectorToFragment att

        PseudoElement ->
            ( Default, "css-s-pe" )

        PseudoClass ->
            ( Default, "css-s-pc" )


attributeSelectorToFragment : AttributeSelector -> ( Style.Required, String )
attributeSelectorToFragment att =
    case att of
        AttributeName ->
            ( Style5, "css-s-a-an" )

        AttributeValue ->
            ( Style2, "css-s-a-av" )

        AttributeOperator ->
            ( Style3, "css-s-a-o" )
