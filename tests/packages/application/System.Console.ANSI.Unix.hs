{-# LANGUAGE Haskell98, CPP, ForeignFunctionInterface #-}
{-# LINE 1 "System/Console/ANSI/Unix.hs" #-}














































{-# OPTIONS_HADDOCK hide #-}
module System.Console.ANSI.Unix (
-- * Basic data types
module System.Console.ANSI.Types,

-- * Cursor movement by character
cursorUp, cursorDown, cursorForward, cursorBackward,
hCursorUp, hCursorDown, hCursorForward, hCursorBackward,
cursorUpCode, cursorDownCode, cursorForwardCode, cursorBackwardCode,

-- * Cursor movement by line
-- | The difference between movements \"by character\" and \"by line\" is
-- that @*Line@ functions additionally move the cursor to the start of the
-- line, while functions like @cursorUp@ and @cursorDown@ keep the column
-- the same.
--
-- Also keep in mind that @*Line@ functions are not as portable. See
-- <https://github.com/feuerbach/ansi-terminal/issues/10> for the details.
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
-- | Note that these functions only clear parts of the screen. They do not move the
-- cursor.
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
    ) where

import System.Console.ANSI.Codes
import System.Console.ANSI.Types
import System.IO (Handle, hIsTerminalDevice, hPutStr, stdout)

import System.Environment

hCursorUp, hCursorDown, hCursorForward, hCursorBackward :: Handle
                                                        -> Int -- ^ Number of lines or characters to move
                                                        -> IO ()
cursorUp, cursorDown, cursorForward, cursorBackward :: Int -- ^ Number of lines or characters to move
                                                    -> IO ()
cursorUp = hCursorUp stdout
cursorDown = hCursorDown stdout
cursorForward = hCursorForward stdout
cursorBackward = hCursorBackward stdout

hCursorDownLine, hCursorUpLine :: Handle
                               -> Int -- ^ Number of lines to move
                               -> IO ()
cursorDownLine, cursorUpLine :: Int -- ^ Number of lines to move
                             -> IO ()
cursorDownLine = hCursorDownLine stdout
cursorUpLine = hCursorUpLine stdout

hSetCursorColumn :: Handle
                 -> Int -- ^ 0-based column to move to
                 -> IO ()
setCursorColumn :: Int -- ^ 0-based column to move to
                -> IO ()
setCursorColumn = hSetCursorColumn stdout

hSetCursorPosition :: Handle
                   -> Int -- ^ 0-based row to move to
                   -> Int -- ^ 0-based column to move to
                   -> IO ()
setCursorPosition :: Int -- ^ 0-based row to move to
                  -> Int -- ^ 0-based column to move to
                  -> IO ()
setCursorPosition = hSetCursorPosition stdout

hClearFromCursorToScreenEnd, hClearFromCursorToScreenBeginning, hClearScreen :: Handle
                                                                             -> IO ()
clearFromCursorToScreenEnd, clearFromCursorToScreenBeginning, clearScreen :: IO ()
clearFromCursorToScreenEnd = hClearFromCursorToScreenEnd stdout
clearFromCursorToScreenBeginning = hClearFromCursorToScreenBeginning stdout
clearScreen = hClearScreen stdout

hClearFromCursorToLineEnd, hClearFromCursorToLineBeginning, hClearLine :: Handle
                                                                       -> IO ()
clearFromCursorToLineEnd, clearFromCursorToLineBeginning, clearLine :: IO ()

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
setSGR = hSetSGR stdout

hHideCursor, hShowCursor :: Handle
                         -> IO ()
hideCursor, showCursor :: IO ()
hideCursor = hHideCursor stdout
showCursor = hShowCursor stdout

-- | Set the terminal window title
hSetTitle :: Handle
          -> String -- ^ New title
          -> IO ()
-- | Set the terminal window title
setTitle :: String -- ^ New title
         -> IO ()
setTitle = hSetTitle stdout

-- | Use heuristics to determine whether the functions defined in this
-- package will work with a given handle.
--
-- The current implementation checks that the handle is a terminal, and
-- that the @TERM@ environment variable doesn't say @dumb@ (which is what
-- Emacs sets for its own terminal).
hSupportsANSI :: Handle -> IO Bool
-- Borrowed from an HSpec patch by Simon Hengel
-- (https://github.com/hspec/hspec/commit/d932f03317e0e2bd08c85b23903fb8616ae642bd)
hSupportsANSI h = (&&) <$> hIsTerminalDevice h <*> (not <$> isDumb)
  where
    -- cannot use lookupEnv since it only appeared in GHC 7.6
    isDumb = maybe False (== "dumb") . lookup "TERM" <$> getEnvironment

hCursorUp h n = hPutStr h $ cursorUpCode n
hCursorDown h n = hPutStr h $ cursorDownCode n
hCursorForward h n = hPutStr h $ cursorForwardCode n
hCursorBackward h n = hPutStr h $ cursorBackwardCode n

hCursorDownLine h n = hPutStr h $ cursorDownLineCode n
hCursorUpLine h n = hPutStr h $ cursorUpLineCode n

hSetCursorColumn h n = hPutStr h $ setCursorColumnCode n
hSetCursorPosition h n m = hPutStr h $ setCursorPositionCode n m

hClearFromCursorToScreenEnd h = hPutStr h clearFromCursorToScreenEndCode
hClearFromCursorToScreenBeginning h = hPutStr h clearFromCursorToScreenBeginningCode
hClearScreen h = hPutStr h clearScreenCode

hClearFromCursorToLineEnd h = hPutStr h clearFromCursorToLineEndCode
hClearFromCursorToLineBeginning h = hPutStr h clearFromCursorToLineBeginningCode
hClearLine h = hPutStr h clearLineCode

hScrollPageUp h n = hPutStr h $ scrollPageUpCode n
hScrollPageDown h n = hPutStr h $ scrollPageDownCode n

hSetSGR h sgrs = hPutStr h $ setSGRCode sgrs

hHideCursor h = hPutStr h hideCursorCode
hShowCursor h = hPutStr h showCursorCode

hSetTitle h title = hPutStr h $ setTitleCode title
