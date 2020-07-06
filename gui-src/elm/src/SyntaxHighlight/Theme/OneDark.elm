module SyntaxHighlight.Theme.OneDark exposing (css, theme)

import SyntaxHighlight.Style exposing (Color(..), RequiredStyles, backgroundColor, italic, noEmphasis, textColor)
import SyntaxHighlight.Theme.Type exposing (Theme, toCss)



{-
   Author: Baransu (https://github.com/Baransu)
   Atom One Dark inspired theme
   https://github.com/atom/one-dark-syntax

   base:    #282c34
   mono-1:  #abb2bf
   mono-2:  #818896
   mono-3:  #5c6370
   hue-1:   #56b6c2
   hue-2:   #61aeee
   hue-3:   #c678dd
   hue-4:   #98c379
   hue-5:   #e06c75
   hue-5-2: #be5046
   hue-6:   #d19a66
   hue-6-2: #e6c07b
-}


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
    { default = noEmphasis (Hex "#abb2bf") (Hex "#282c34")
    , highlight = backgroundColor (Rgba 229 231 235 0.1)
    , addition = backgroundColor (Rgba 40 124 82 0.4)
    , deletion = backgroundColor (Rgba 136 64 67 0.4)
    , comment = textColor (Hex "#5c6370") |> italic
    , style1 = textColor (Hex "#d19a66")
    , style2 = textColor (Hex "#98c379")
    , style3 = textColor (Hex "#c678dd")
    , style4 = textColor (Hex "#c678dd")
    , style5 = textColor (Hex "#61aeee")
    , style6 = textColor (Hex "#d19a66")
    , style7 = textColor (Hex "#abb2bf")
    }
