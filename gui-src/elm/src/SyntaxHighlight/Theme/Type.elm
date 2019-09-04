module SyntaxHighlight.Theme.Type exposing (Syntax(..), Theme, syntaxToSelector, syntaxesToSelectors, toCss)

import SyntaxHighlight.Language.Css as Css
import SyntaxHighlight.Language.Elm as Elm
import SyntaxHighlight.Language.Haskell as Haskell
import SyntaxHighlight.Language.Javascript as Javascript
import SyntaxHighlight.Language.Python as Python
import SyntaxHighlight.Language.Sql as Sql
import SyntaxHighlight.Language.Xml as Xml
import SyntaxHighlight.Style as Style exposing (RequiredStyles, Style)


type alias Theme =
    { requiredStyles : RequiredStyles
    , customStyles : List ( List Syntax, Style )
    }


type Syntax
    = Elm Elm.Syntax
    | Xml Xml.Syntax
    | Javascript Javascript.Syntax
    | Css Css.Syntax
    | Python Python.Syntax
    | Sql Sql.Syntax
    | Haskell Haskell.Syntax


toCss : Theme -> String
toCss { requiredStyles, customStyles } =
    [ ( ".elmsh", requiredStyles.default )
    , ( ".elmsh-hl", requiredStyles.highlight )
    , ( ".elmsh-add", requiredStyles.addition )
    , ( ".elmsh-del", requiredStyles.deletion )
    , ( ".elmsh-comm", requiredStyles.comment )
    , ( ".elmsh1", requiredStyles.style1 )
    , ( ".elmsh2", requiredStyles.style2 )
    , ( ".elmsh3", requiredStyles.style3 )
    , ( ".elmsh4", requiredStyles.style4 )
    , ( ".elmsh5", requiredStyles.style5 )
    , ( ".elmsh6", requiredStyles.style6 )
    , ( ".elmsh7", requiredStyles.style7 )
    ]
        ++ List.map (Tuple.mapFirst syntaxesToSelectors) customStyles
        |> Style.toCss


syntaxesToSelectors : List Syntax -> String
syntaxesToSelectors syntaxes =
    List.map syntaxToSelector syntaxes
        |> List.map ((++) ".elmsh-")
        |> List.intersperse ", "
        |> String.concat


syntaxToSelector : Syntax -> String
syntaxToSelector syntax =
    case syntax of
        Elm elmSyntax ->
            Elm.syntaxToStyle elmSyntax
                |> Tuple.second

        Xml xmlSyntax ->
            Xml.syntaxToStyle xmlSyntax
                |> Tuple.second

        Javascript jsSyntax ->
            Javascript.syntaxToStyle jsSyntax
                |> Tuple.second

        Css cssSyntax ->
            Css.syntaxToStyle cssSyntax
                |> Tuple.second

        Python pythonSyntax ->
            Python.syntaxToStyle pythonSyntax
                |> Tuple.second

        Sql sqlSyntax ->
            Sql.syntaxToStyle sqlSyntax
                |> Tuple.second

        Haskell haskellSyntax ->
            Haskell.syntaxToStyle haskellSyntax
                |> Tuple.second
