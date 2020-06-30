module SyntaxHighlight.Theme.GitHub exposing (css, theme)

import SyntaxHighlight.Style exposing (Color(..), RequiredStyles, backgroundColor, noEmphasis, textColor)
import SyntaxHighlight.Theme.Type exposing (Theme, toCss)



-- GitHub inspired theme


css : String
css =
    toCss theme


theme : Theme
theme =
    { requiredStyles = requiredStyles
    , customStyles = []
    }


requiredStyles : RequiredStyles
requiredStyles =
    { default = noEmphasis (Hex "#24292e") (Hex "#ffffff")
    , highlight = backgroundColor (Hex "#fffbdd")
    , addition = backgroundColor (Hex "#eaffea")
    , deletion = backgroundColor (Hex "#ffecec")
    , comment = textColor (Hex "#969896")
    , style1 = textColor (Hex "#005cc5")
    , style2 = textColor (Hex "#df5000")
    , style3 = textColor (Hex "#d73a49")
    , style4 = textColor (Hex "#0086b3")
    , style5 = textColor (Hex "#63a35c")
    , style6 = textColor (Hex "#005cc5")
    , style7 = textColor (Hex "#795da3")
    }
