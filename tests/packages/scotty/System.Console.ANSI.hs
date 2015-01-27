{-# LINE 1 "System/Console/ANSI.hs" #-}
# 1 "System/Console/ANSI.hs"
# 1 "<command-line>"
# 9 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 9 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1






















































































# 9 "<command-line>" 2
# 1 "System/Console/ANSI.hs"
-- | Provides ANSI terminal support for Windows and ANSI terminal software running on a Unix-like operating system.
--
-- The ANSI escape codes are described at <http://en.wikipedia.org/wiki/ANSI_escape_code> and provide a rich range of
-- functionality for terminal control, which includes:
--
--  * Colored text output, with control over both foreground and background colors
--
--  * Hiding or showing the cursor
--
--  * Moving the cursor around
--
--  * Clearing parts of the screen
--
-- The most frequently used parts of this ANSI command set are exposed with a platform independent interface by
-- this module.  Every function exported comes in three flavours:
--
--  * Vanilla: has an @IO ()@ type and doesn't take a @Handle@.  This just outputs the ANSI command directly on
--    to the terminal corresponding to stdout.  Commands issued like this should work as you expect on both Windows
--    and Unix.
--
--  * Chocolate: has an @IO ()@ type but takes a @Handle@.  This outputs the ANSI command on the terminal corresponding
--    to the supplied handle.  Commands issued like this should also work as your expect on both Windows and Unix.
--
--  * Strawberry: has a @String@ type and just consists of an escape code which can be added to any other bit of text
--    before being output.  This version of the API is often convenient to use, but due to fundamental limitations in
--    Windows ANSI terminal support will only work on Unix.  On Windows these codes will always be the empty string,
--    so it is possible to use them portably for e.g. coloring console output on the understanding that you will only
--    see colors if you are running on a Unix-like operating system.
# 37 "System/Console/ANSI.hs"

module System.Console.ANSI (
        module System.Console.ANSI.Unix
    ) where

import System.Console.ANSI.Unix

