module SyntaxHighlight.Language.Sql exposing
    ( Syntax(..)
    ,  syntaxToStyle
       -- Exposing for test purposes

    , toLines
    , toRevTokens
    )

import Parser exposing ((|.), DeadEnd, Parser, Step(..), andThen, backtrackable, getChompedString, loop, map, oneOf, succeed, symbol)
import Regex exposing (Regex)
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
    | Operator
    | Function
    | Punctuation
    | Literal


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
        , punctuationChar
            |> map (\n -> Loop (n :: revTokens))
        , number
            |> map (\n -> Loop (n :: revTokens))
        , comment
            |> map (\n -> Loop (n ++ revTokens))
        , stringLiteral
            |> andThen (\n -> loop (n ++ revTokens) stringBody)
            |> map Loop
        , chompIfThenWhile isIdentifierChar
            |> getChompedString
            |> andThen (keywordParser revTokens)
            |> map Loop
        , succeed (Done revTokens)
        ]


isIdentifierChar : Char -> Bool
isIdentifierChar c =
    not
        (isWhitespace c
            || isPunctuationChar c
        )


stringBody : List Token -> Parser (Step (List Token) (List Token))
stringBody revTokens =
    oneOf
        [ whitespaceOrCommentStep revTokens
        , stringLiteral |> map (\s -> Loop (s ++ revTokens))
        , succeed (Done revTokens)
        ]


punctuationChar : Parser Token
punctuationChar =
    chompIfThenWhile isPunctuationChar
        |> getChompedString
        |> map (\b -> ( T.C Punctuation, b ))


isPunctuationChar : Char -> Bool
isPunctuationChar c =
    Set.member c punctuatorSet


punctuatorSet : Set Char
punctuatorSet =
    Set.fromList [ ';', '[', ']', '(', ')', '`', ',', '.' ]



-- Keywords


keywordParser : List Token -> String -> Parser (List Token)
keywordParser revTokens s =
    if isOperator s then
        succeed (( T.C Operator, s ) :: revTokens)

    else if isFunction s then
        succeed (( T.C Function, s ) :: revTokens)

    else if isKeyword s then
        succeed (( T.C Keyword, s ) :: revTokens)

    else if isLiteral s then
        succeed (( T.C Literal, s ) :: revTokens)

    else
        succeed (( T.Normal, s ) :: revTokens)


isKeyword : String -> Bool
isKeyword =
    Regex.contains keywordPattern


keywordPattern : Regex
keywordPattern =
    "^(ACTION|ADD|AFTER|ALGORITHM|ALL|ALTER|ANALYZE|ANY|APPLY|AS|ASC|AUTHORIZATION|AUTO_INCREMENT|BACKUP|BDB|BEGIN|BERKELEYDB|BIGINT|BINARY|BIT|BLOB|BOOL|BOOLEAN|BREAK|BROWSE|BTREE|BULK|BY|CALL|CASCADED?|CASE|CHAIN|CHAR(?:ACTER|SET)?|CHECK(?:POINT)?|CLOSE|CLUSTERED|COALESCE|COLLATE|COLUMNS?|COMMENT|COMMIT(?:TED)?|COMPUTE|CONNECT|CONSISTENT|CONSTRAINT|CONTAINS(?:TABLE)?|CONTINUE|CONVERT|CREATE|CROSS|CURRENT(?:_DATE|_TIME|_TIMESTAMP|_USER)?|CURSOR|CYCLE|DATA(?:BASES?)?|DATE(?:TIME)?|DAY|DBCC|DEALLOCATE|DEC|DECIMAL|DECLARE|DEFAULT|DEFINER|DELAYED|DELETE|DELIMITERS?|DENY|DESC|DESCRIBE|DETERMINISTIC|DISABLE|DISCARD|DISK|DISTINCT|DISTINCTROW|DISTRIBUTED|DO|DOUBLE|DROP|DUMMY|DUMP(?:FILE)?|DUPLICATE|ELSE(?:IF)?|ENABLE|ENCLOSED|END|ENGINE|ENUM|ERRLVL|ERRORS|ESCAPED?|EXCEPT|EXEC(?:UTE)?|EXISTS|EXIT|EXPLAIN|EXTENDED|FETCH|FIELDS|FILE|FILLFACTOR|FIRST|FIXED|FLOAT|FOLLOWING|FOR(?: EACH ROW)?|FORCE|FOREIGN|FREETEXT(?:TABLE)?|FROM|FULL|FUNCTION|GEOMETRY(?:COLLECTION)?|GLOBAL|GOTO|GRANT|GROUP|HANDLER|HASH|HAVING|HOLDLOCK|HOUR|IDENTITY(?:_INSERT|COL)?|IF|IGNORE|IMPORT|INDEX|INFILE|INNER|INNODB|INOUT|INSERT|INT|INTEGER|INTERSECT|INTERVAL|INTO|INVOKER|ISOLATION|ITERATE|JOIN|KEYS?|KILL|LANGUAGE|LAST|LEAVE|LEFT|LEVEL|LIMIT|LINENO|LINES|LINESTRING|LOAD|LOCAL|LOCK|LONG(?:BLOB|TEXT)|LOOP|MATCH(?:ED)?|MEDIUM(?:BLOB|INT|TEXT)|MERGE|MIDDLEINT|MINUTE|MODE|MODIFIES|MODIFY|MONTH|MULTI(?:LINESTRING|POINT|POLYGON)|NATIONAL|NATURAL|NCHAR|NEXT|NO|NONCLUSTERED|NULLIF|NUMERIC|OFF?|OFFSETS?|ON|OPEN(?:DATASOURCE|QUERY|ROWSET)?|OPTIMIZE|OPTION(?:ALLY)?|ORDER|OUT(?:ER|FILE)?|OVER|PARTIAL|PARTITION|PERCENT|PIVOT|PLAN|POINT|POLYGON|PRECEDING|PRECISION|PREPARE|PREV|PRIMARY|PRINT|PRIVILEGES|PROC(?:EDURE)?|PUBLIC|PURGE|QUICK|RAISERROR|READS?|REAL|RECONFIGURE|REFERENCES|RELEASE|RENAME|REPEAT(?:ABLE)?|REPLACE|REPLICATION|REQUIRE|RESIGNAL|RESTORE|RESTRICT|RETURNS?|REVOKE|RIGHT|ROLLBACK|ROUTINE|ROW(?:COUNT|GUIDCOL|S)?|RTREE|RULE|SAVE(?:POINT)?|SCHEMA|SECOND|SELECT|SERIAL(?:IZABLE)?|SESSION(?:_USER)?|SET(?:USER)?|SHARE|SHOW|SHUTDOWN|SIMPLE|SMALLINT|SNAPSHOT|SOME|SONAME|SQL|START(?:ING)?|STATISTICS|STATUS|STRIPED|SYSTEM_USER|TABLES?|TABLESPACE|TEMP(?:ORARY|TABLE)?|TERMINATED|TEXT(?:SIZE)?|THEN|TIME(?:STAMP)?|TINY(?:BLOB|INT|TEXT)|TOP?|TRAN(?:SACTIONS?)?|TRIGGER|TRUNCATE|TSEQUAL|TYPES?|UNBOUNDED|UNCOMMITTED|UNDEFINED|UNION|UNIQUE|UNLOCK|UNPIVOT|UNSIGNED|UPDATE(?:TEXT)?|USAGE|USE|USER|USING|VALUES?|VAR(?:BINARY|CHAR|CHARACTER|YING)|VIEW|WAITFOR|WARNINGS|WHEN|WHERE|WHILE|WITH(?: ROLLUP|IN)?|WORK|WRITE(?:TEXT)?|YEAR)$"
        |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
        |> Maybe.withDefault Regex.never


isLiteral : String -> Bool
isLiteral str =
    Set.member (String.toUpper str) literalSet


literalSet : Set String
literalSet =
    Set.fromList [ "TRUE", "FALSE", "NULL" ]


isFunction : String -> Bool
isFunction str =
    Set.member (String.toUpper str) functionSet


functionSet : Set String
functionSet =
    Set.fromList [ "AVG", "COUNT", "FIRST", "FORMAT", "LAST", "LCASE", "LEN", "MAX", "MID", "MIN", "MOD", "NOW", "ROUND", "SUM", "UCASE" ]


isOperator : String -> Bool
isOperator =
    Regex.contains operatorPattern


operatorPattern : Regex
operatorPattern =
    "^([-+*\\/=%^~]|&&?|\\|\\|?|!=?|<(?:=>?|<|>)?|>[>=]?|AND|BETWEEN|IN|LIKE|NOT|OR|IS|DIV|REGEXP|RLIKE|SOUNDS LIKE|XOR)$"
        |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
        |> Maybe.withDefault Regex.never



-- Strings


stringLiteral : Parser (List Token)
stringLiteral =
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
    , innerParsers = [ lineBreakList, sqlEscapable ]
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
    c == '\'' || c == '"'



-- Comments


comment : Parser (List Token)
comment =
    oneOf
        [ inlineComment
        , multilineComment
        ]


inlineComment : Parser (List Token)
inlineComment =
    [ "--", "$", "#" ]
        |> List.map (symbol >> thenChompWhile (not << isLineBreak) >> getChompedString >> map (\b -> [ ( T.Comment, b ) ]))
        |> oneOf


multilineComment : Parser (List Token)
multilineComment =
    delimited
        { start = "/*"
        , end = "*/"
        , isNestable = False
        , defaultMap = \b -> ( T.Comment, b )
        , innerParsers = [ lineBreakList ]
        , isNotRelevant = not << isLineBreak
        }



-- Helpers


whitespaceOrCommentStep : List Token -> Parser (Step (List Token) (List Token))
whitespaceOrCommentStep revTokens =
    oneOf
        [ space |> map (\s -> Loop (s :: revTokens))
        , lineBreak
            |> map (\s -> s :: revTokens)
            |> andThen checkContext
        , comment |> map (\s -> Loop (s ++ revTokens))
        ]


checkContext : List Token -> Parser (Step (List Token) (List Token))
checkContext revTokens =
    oneOf
        [ whitespaceOrCommentStep revTokens
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


number : Parser Token
number =
    oneOf
        [ hexNumber
        , SyntaxHighlight.Language.Helpers.number
        ]
        |> getChompedString
        |> map (\b -> ( T.C Number, b ))


hexNumber : Parser ()
hexNumber =
    succeed ()
        |. backtrackable (symbol "0x")
        |. chompIfThenWhile Char.isHexDigit


sqlEscapable : Parser (List Token)
sqlEscapable =
    escapable
        |> getChompedString
        |> map (\b -> [ ( T.C Function, b ) ])


syntaxToStyle : Syntax -> ( Style.Required, String )
syntaxToStyle syntax =
    case syntax of
        Number ->
            ( Style1, "sql-n" )

        String ->
            ( Style2, "sql-s" )

        Keyword ->
            ( Style3, "sql-k" )

        Operator ->
            ( Style4, "sql-o" )

        Function ->
            ( Style5, "sql-f" )

        Punctuation ->
            ( Style6, "sql-p" )

        Literal ->
            ( Style7, "sql-l" )
