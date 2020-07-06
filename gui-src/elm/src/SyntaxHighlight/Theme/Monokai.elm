module SyntaxHighlight.Theme.Monokai exposing (css, theme)

import SyntaxHighlight.Language.Css as Css
import SyntaxHighlight.Language.Elm as Elm
import SyntaxHighlight.Language.Javascript as JS
import SyntaxHighlight.Style exposing (Color(..), RequiredStyles, backgroundColor, bold, italic, noEmphasis, textColor)
import SyntaxHighlight.Theme.Type as Type exposing (Syntax(..), Theme, toCss)



-- Monokai inspired theme


css : String
css =
    toCss theme


theme : Theme
theme =
    { requiredStyles = requiredStyles
    , customStyles =
        [ ( [ Elm Elm.TypeSignature
            , Javascript JS.DeclarationKeyword
            , Css Css.Property
            ]
          , textColor (Hex "#66d9ef") |> italic
          )
        , ( [ Javascript JS.ClassExtends ]
          , textColor (Hex "#a6e22e") |> italic
          )
        , ( [ Css (Css.AtRule Css.Identifier) ]
          , textColor (Hex "#f92672") |> bold
          )
        ]
    }


requiredStyles : RequiredStyles
requiredStyles =
    { default = noEmphasis (Hex "#f8f8f2") (Hex "#23241f")
    , highlight = backgroundColor (Hex "#343434")
    , addition = backgroundColor (Hex "#003800")
    , deletion = backgroundColor (Hex "#380000")
    , comment = textColor (Hex "#75715e")
    , style1 = textColor (Hex "#ae81ff")
    , style2 = textColor (Hex "#e6db74")
    , style3 = textColor (Hex "#f92672")
    , style4 = textColor (Hex "#66d9ef")
    , style5 = textColor (Hex "#a6e22e")
    , style6 = textColor (Hex "#ae81ff")
    , style7 = textColor (Hex "#fd971f")
    }
