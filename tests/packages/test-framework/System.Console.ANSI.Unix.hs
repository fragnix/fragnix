{-# LINE 1 "./System/Console/ANSI/Unix.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                          






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./System/Console/ANSI/Unix.hs" #-}
{-# LINE 1 "./System/Console/ANSI/Unix.hs" #-}
{-# OPTIONS_HADDOCK hide #-}
module System.Console.ANSI.Unix (
{-# LINE 1 "includes/Exports-Include.hs" #-}
-- * Basic data types
module System.Console.ANSI.Common,

-- * Cursor movement by character
cursorUp, cursorDown, cursorForward, cursorBackward,
hCursorUp, hCursorDown, hCursorForward, hCursorBackward,
cursorUpCode, cursorDownCode, cursorForwardCode, cursorBackwardCode,

-- * Cursor movement by line
cursorUpLine, cursorDownLine,
hCursorUpLine, hCursorDownLine,
cursorUpLineCode, cursorDownLineCode,

-- * Directly changing cursor position
setCursorColumn,
hSetCursorColumn,
setCursorColumnCode,

setCursorPosition,
hSetCursorPosition,
setCursorPositionCode,

-- * Clearing parts of the screen
clearFromCursorToScreenEnd, clearFromCursorToScreenBeginning, clearScreen,
hClearFromCursorToScreenEnd, hClearFromCursorToScreenBeginning, hClearScreen,
clearFromCursorToScreenEndCode, clearFromCursorToScreenBeginningCode, clearScreenCode,

clearFromCursorToLineEnd, clearFromCursorToLineBeginning, clearLine,
hClearFromCursorToLineEnd, hClearFromCursorToLineBeginning, hClearLine,
clearFromCursorToLineEndCode, clearFromCursorToLineBeginningCode, clearLineCode,

-- * Scrolling the screen
scrollPageUp, scrollPageDown,
hScrollPageUp, hScrollPageDown,
scrollPageUpCode, scrollPageDownCode,

-- * Select Graphic Rendition mode: colors and other whizzy stuff
setSGR,
hSetSGR,
setSGRCode,

-- * Cursor visibilty changes
hideCursor, showCursor,
hHideCursor, hShowCursor,
hideCursorCode, showCursorCode,

-- * Changing the title
setTitle,
hSetTitle,
setTitleCode,

-- * Checking if handle supports ANSI
hSupportsANSI

{-# LINE 4 "./System/Console/ANSI/Unix.hs" #-}
    ) where

import System.Console.ANSI.Common

import System.IO

import Data.List


{-# LINE 1 "includes/Common-Include.hs" #-}
import System.Environment
import Control.Applicative

hCursorUp, hCursorDown, hCursorForward, hCursorBackward :: Handle
                                                        -> Int -- ^ Number of lines or characters to move
                                                        -> IO ()
cursorUp, cursorDown, cursorForward, cursorBackward :: Int -- ^ Number of lines or characters to move
                                                    -> IO ()
cursorUpCode, cursorDownCode, cursorForwardCode, cursorBackwardCode :: Int -- ^ Number of lines or characters to move
                                                                    -> String

cursorUp = hCursorUp stdout
cursorDown = hCursorDown stdout
cursorForward = hCursorForward stdout
cursorBackward = hCursorBackward stdout


hCursorDownLine, hCursorUpLine :: Handle
                               -> Int -- ^ Number of lines to move
                               -> IO ()
cursorDownLine, cursorUpLine :: Int -- ^ Number of lines to move
                             -> IO ()
cursorDownLineCode, cursorUpLineCode :: Int -- ^ Number of lines to move
                                     -> String

cursorDownLine = hCursorDownLine stdout
cursorUpLine = hCursorUpLine stdout


hSetCursorColumn :: Handle
                 -> Int -- ^ 0-based column to move to
                 -> IO ()
setCursorColumn :: Int -- ^ 0-based column to move to
                -> IO ()
setCursorColumnCode :: Int -- ^ 0-based column to move to
                    -> String

setCursorColumn = hSetCursorColumn stdout


hSetCursorPosition :: Handle
                   -> Int -- ^ 0-based row to move to
                   -> Int -- ^ 0-based column to move to
                   -> IO ()
setCursorPosition :: Int -- ^ 0-based row to move to
                  -> Int -- ^ 0-based column to move to
                  -> IO ()
setCursorPositionCode :: Int -- ^ 0-based row to move to
                      -> Int -- ^ 0-based column to move to
                      -> String

setCursorPosition = hSetCursorPosition stdout


hClearFromCursorToScreenEnd, hClearFromCursorToScreenBeginning, hClearScreen :: Handle
                                                                             -> IO ()
clearFromCursorToScreenEnd, clearFromCursorToScreenBeginning, clearScreen :: IO ()
clearFromCursorToScreenEndCode, clearFromCursorToScreenBeginningCode, clearScreenCode :: String

clearFromCursorToScreenEnd = hClearFromCursorToScreenEnd stdout
clearFromCursorToScreenBeginning = hClearFromCursorToScreenBeginning stdout
clearScreen = hClearScreen stdout


hClearFromCursorToLineEnd, hClearFromCursorToLineBeginning, hClearLine :: Handle
                                                                       -> IO ()
clearFromCursorToLineEnd, clearFromCursorToLineBeginning, clearLine :: IO ()
clearFromCursorToLineEndCode, clearFromCursorToLineBeginningCode, clearLineCode :: String

clearFromCursorToLineEnd = hClearFromCursorToLineEnd stdout
clearFromCursorToLineBeginning = hClearFromCursorToLineBeginning stdout
clearLine = hClearLine stdout


-- | Scroll the displayed information up or down the terminal: not widely supported
hScrollPageUp, hScrollPageDown :: Handle
                               -> Int -- ^ Number of lines to scroll by
                               -> IO ()
-- | Scroll the displayed information up or down the terminal: not widely supported
scrollPageUp, scrollPageDown :: Int -- ^ Number of lines to scroll by
                             -> IO ()
-- | Scroll the displayed information up or down the terminal: not widely supported
scrollPageUpCode, scrollPageDownCode :: Int -- ^ Number of lines to scroll by
                                     -> String

scrollPageUp = hScrollPageUp stdout
scrollPageDown = hScrollPageDown stdout


-- | Set the Select Graphic Rendition mode
hSetSGR :: Handle
        -> [SGR] -- ^ Commands: these will typically be applied on top of the current console SGR mode.
                 -- An empty list of commands is equivalent to the list @[Reset]@. Commands are applied
                 -- left to right.
        -> IO ()
-- | Set the Select Graphic Rendition mode
setSGR :: [SGR] -- ^ Commands: these will typically be applied on top of the current console SGR mode.
                -- An empty list of commands is equivalent to the list @[Reset]@. Commands are applied
                -- left to right.
       -> IO ()
-- | Set the Select Graphic Rendition mode
setSGRCode :: [SGR] -- ^ Commands: these will typically be applied on top of the current console SGR mode.
                    -- An empty list of commands is equivalent to the list @[Reset]@. Commands are applied
                    -- left to right.
           -> String

setSGR = hSetSGR stdout


hHideCursor, hShowCursor :: Handle
                         -> IO ()
hideCursor, showCursor :: IO ()
hideCursorCode, showCursorCode :: String

hideCursor = hHideCursor stdout
showCursor = hShowCursor stdout


-- | Set the terminal window title
hSetTitle :: Handle
          -> String -- ^ New title
          -> IO ()
-- | Set the terminal window title
setTitle :: String -- ^ New title
         -> IO ()
-- | Set the terminal window title
setTitleCode :: String -- ^ New title
             -> String

setTitle = hSetTitle stdout

-- | Use heuristics to determine whether the functions defined in this
-- package will work with a given handle.
--
-- The current implementation checks that the handle is a terminal, and
-- that the @TERM@ environment variable doesn't say @dumb@ (whcih is what
-- Emacs sets for its own terminal).
hSupportsANSI :: Handle -> IO Bool
-- Borrowed from an HSpec patch by Simon Hengel
-- (https://github.com/hspec/hspec/commit/d932f03317e0e2bd08c85b23903fb8616ae642bd)
hSupportsANSI h = (&&) <$> hIsTerminalDevice h <*> (not <$> isDumb)
  where
    -- cannot use lookupEnv since it only appeared in GHC 7.6
    isDumb = maybe False (== "dumb") . lookup "TERM" <$> getEnvironment

{-# LINE 14 "./System/Console/ANSI/Unix.hs" #-}


-- | The reference I used for the ANSI escape characters in this module was <http://en.wikipedia.org/wiki/ANSI_escape_sequences>.
csi :: [Int] -> String -> String
csi args code = "\ESC[" ++ concat (intersperse ";" (map show args)) ++ code

colorToCode :: Color -> Int
colorToCode color = case color of
    Black   -> 0
    Red     -> 1
    Green   -> 2
    Yellow  -> 3
    Blue    -> 4
    Magenta -> 5
    Cyan    -> 6
    White   -> 7

sgrToCode :: SGR -> Int
sgrToCode sgr = case sgr of
    Reset -> 0
    SetConsoleIntensity intensity -> case intensity of
        BoldIntensity   -> 1
        FaintIntensity  -> 2
        NormalIntensity -> 22
    SetItalicized True  -> 3
    SetItalicized False -> 23
    SetUnderlining underlining -> case underlining of
        SingleUnderline -> 4
        DoubleUnderline -> 21
        NoUnderline     -> 24
    SetBlinkSpeed blink_speed -> case blink_speed of
        SlowBlink   -> 5
        RapidBlink  -> 6
        NoBlink     -> 25
    SetVisible False -> 8
    SetVisible True  -> 28
    SetSwapForegroundBackground True  -> 7
    SetSwapForegroundBackground False -> 27
    SetColor Foreground Dull color  -> 30 + colorToCode color
    SetColor Foreground Vivid color -> 90 + colorToCode color
    SetColor Background Dull color  -> 40 + colorToCode color
    SetColor Background Vivid color -> 100 + colorToCode color


cursorUpCode n = csi [n] "A"
cursorDownCode n = csi [n] "B"
cursorForwardCode n = csi [n] "C"
cursorBackwardCode n = csi [n] "D"

hCursorUp h n = hPutStr h $ cursorUpCode n
hCursorDown h n = hPutStr h $ cursorDownCode n
hCursorForward h n = hPutStr h $ cursorForwardCode n
hCursorBackward h n = hPutStr h $ cursorBackwardCode n


cursorDownLineCode n = csi [n] "E"
cursorUpLineCode n = csi [n] "F"

hCursorDownLine h n = hPutStr h $ cursorDownLineCode n
hCursorUpLine h n = hPutStr h $ cursorUpLineCode n


setCursorColumnCode n = csi [n + 1] "G"
setCursorPositionCode n m = csi [n + 1, m + 1] "H"

hSetCursorColumn h n = hPutStr h $ setCursorColumnCode n
hSetCursorPosition h n m = hPutStr h $ setCursorPositionCode n m


clearFromCursorToScreenEndCode = csi [0] "J"
clearFromCursorToScreenBeginningCode = csi [1] "J"
clearScreenCode = csi [2] "J"

hClearFromCursorToScreenEnd h = hPutStr h clearFromCursorToScreenEndCode
hClearFromCursorToScreenBeginning h = hPutStr h clearFromCursorToScreenBeginningCode
hClearScreen h = hPutStr h clearScreenCode


clearFromCursorToLineEndCode = csi [0] "K"
clearFromCursorToLineBeginningCode = csi [1] "K"
clearLineCode = csi [2] "K"

hClearFromCursorToLineEnd h = hPutStr h clearFromCursorToLineEndCode
hClearFromCursorToLineBeginning h = hPutStr h clearFromCursorToLineBeginningCode
hClearLine h = hPutStr h clearLineCode


scrollPageUpCode n = csi [n] "S"
scrollPageDownCode n = csi [n] "T"

hScrollPageUp h n = hPutStr h $ scrollPageUpCode n
hScrollPageDown h n = hPutStr h $ scrollPageDownCode n


setSGRCode sgrs = csi (map sgrToCode sgrs) "m"

hSetSGR h sgrs = hPutStr h $ setSGRCode sgrs


hideCursorCode = csi [] "?25l"
showCursorCode = csi [] "?25h"

hHideCursor h = hPutStr h hideCursorCode
hShowCursor h = hPutStr h showCursorCode


-- | Thanks to Brandon S. Allbery and Curt Sampson for pointing me in the right direction on xterm title setting on haskell-cafe.
-- The "0" signifies that both the title and "icon" text should be set: i.e. the text for the window in the Start bar (or similar)
-- as well as that in the actual window title.  This is chosen for consistent behaviour between Unixes and Windows.
setTitleCode title = "\ESC]0;" ++ filter (/= '\007') title ++ "\007"

hSetTitle h title = hPutStr h $ setTitleCode title
