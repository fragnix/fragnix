module Palette exposing (black, grey, white, real_black, glass, signal_color, edges)

import Element

black dark = if dark then monokai_black else monokai_black_light
grey dark = if dark then monokai_grey else monokai_grey_light
white dark = if dark then monokai_white else monokai_white_light
real_black dark = if dark then actual_black else actual_black_light
signal_color dark = if dark then orange else orange_light

-- | COLOR PALETTE
monokai_black = Element.rgb255 40 44 52-- (Element.rgb255 35 36 31)
monokai_grey = Element.rgb255 56 60 68 -- (Element.rgb255 51 52 47)
monokai_white = Element.rgb255 171 178 191-- (Element.rgb255 247 247 241)
actual_black = Element.rgb 0 0 0
glass = Element.rgba 0 0 0 0
orange = Element.rgb255 255 133 51
edges =
   { top = 0
   , right = 0
   , bottom = 0
   , left = 0
   }

-- | COLOR PALETTE - LIGHT
monokai_black_light = Element.rgb255 204 208 215 -- (Element.rgb255 35 36 31)rgb(204, 208, 215)
monokai_grey_light = Element.rgb255 188 192 199 -- (Element.rgb255 51 52 47)rgb(188, 192, 199)
monokai_white_light = Element.rgb255 67 74 85-- (Element.rgb255 247 247 241)
actual_black_light = Element.rgb 1 1 1
orange_light = Element.rgb255 204 82 0-- rgb(204, 82, 0)
