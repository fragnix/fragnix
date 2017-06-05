{-# LANGUAGE Haskell98, CPP, ForeignFunctionInterface #-}
{-# LINE 1 "System/Console/ANSI/Types.hs" #-}














































-- | Types used to represent SELECT GRAPHIC RENDITION (SGR) aspects.
module System.Console.ANSI.Types
    (
      SGR (..)
    , ConsoleLayer (..)
    , Color (..)
    , ColorIntensity (..)
    , ConsoleIntensity (..)
    , Underlining (..)
    , BlinkSpeed (..)
    ) where

import Data.Ix

-- | ANSI colors: come in various intensities, which are controlled by 'ColorIntensity'
data Color = Black
           | Red
           | Green
           | Yellow
           | Blue
           | Magenta
           | Cyan
           | White
           deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)

-- | ANSI colors come in two intensities
data ColorIntensity = Dull
                    | Vivid
                    deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)

-- | ANSI colors can be set on two different layers
data ConsoleLayer = Foreground
                  | Background
                  deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)

-- | ANSI blink speeds: values other than 'NoBlink' are not widely supported
data BlinkSpeed = SlowBlink -- ^ Less than 150 blinks per minute
                | RapidBlink -- ^ More than 150 blinks per minute
                | NoBlink
                deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)

-- | ANSI text underlining
data Underlining = SingleUnderline
                 | DoubleUnderline -- ^ Not widely supported
                 | NoUnderline
                 deriving (Eq, Ord, Bounded ,Enum, Show, Read, Ix)

-- | ANSI general console intensity: usually treated as setting the font style (e.g. 'BoldIntensity' causes text to be bold)
data ConsoleIntensity = BoldIntensity
                      | FaintIntensity -- ^ Not widely supported: sometimes treated as concealing text
                      | NormalIntensity
                      deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)

-- | ANSI Select Graphic Rendition command
data SGR = Reset
         | SetConsoleIntensity ConsoleIntensity
         | SetItalicized Bool -- ^ Not widely supported: sometimes treated as swapping foreground and background
         | SetUnderlining Underlining
         | SetBlinkSpeed BlinkSpeed
         | SetVisible Bool -- ^ Not widely supported
         | SetSwapForegroundBackground Bool
         | SetColor ConsoleLayer ColorIntensity Color
         deriving (Eq, Ord, Show, Read)
