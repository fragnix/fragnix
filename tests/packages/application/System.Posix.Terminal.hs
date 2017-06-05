{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-261cd265/build/System/Posix/Terminal.hs" #-}
{-# LINE 1 "System/Posix/Terminal.hsc" #-}
{-# LANGUAGE CApiFFI #-}
{-# LINE 2 "System/Posix/Terminal.hsc" #-}

{-# LINE 3 "System/Posix/Terminal.hsc" #-}
{-# LANGUAGE Safe #-}

{-# LINE 7 "System/Posix/Terminal.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Terminal
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX Terminal support
--
-----------------------------------------------------------------------------

module System.Posix.Terminal (
  -- * Terminal support

  -- ** Terminal attributes
  TerminalAttributes,
  getTerminalAttributes,
  TerminalState(..),
  setTerminalAttributes,

  TerminalMode(..),
  withoutMode,
  withMode,
  terminalMode,
  bitsPerByte,
  withBits,

  ControlCharacter(..),
  controlChar,
  withCC,
  withoutCC,

  inputTime,
  withTime,
  minInput,
  withMinInput,

  BaudRate(..),
  inputSpeed,
  withInputSpeed,
  outputSpeed,
  withOutputSpeed,

  -- ** Terminal operations
  sendBreak,
  drainOutput,
  QueueSelector(..),
  discardData,
  FlowAction(..),
  controlFlow,

  -- ** Process groups
  getTerminalProcessGroupID,
  setTerminalProcessGroupID,

  -- ** Testing a file descriptor
  queryTerminal,
  getTerminalName,
  getControllingTerminalName,

  -- ** Pseudoterminal operations
  openPseudoTerminal,
  getSlaveTerminalName
  ) where


{-# LINE 76 "System/Posix/Terminal.hsc" #-}

import Foreign
import Foreign.C
import System.Posix.Terminal.Common
import System.Posix.Types

{-# LINE 84 "System/Posix/Terminal.hsc" #-}

import System.Posix.Internals (peekFilePath)


{-# LINE 91 "System/Posix/Terminal.hsc" #-}

-- | @getTerminalName fd@ calls @ttyname@ to obtain a name associated
--   with the terminal for @Fd@ @fd@. If @fd@ is associated
--   with a terminal, @getTerminalName@ returns the name of the
--   terminal.
getTerminalName :: Fd -> IO FilePath
getTerminalName (Fd fd) = do
  s <- throwErrnoIfNull "getTerminalName" (c_ttyname fd)
  peekFilePath s

foreign import ccall unsafe "ttyname"
  c_ttyname :: CInt -> IO CString

-- | @getControllingTerminalName@ calls @ctermid@ to obtain
--   a name associated with the controlling terminal for the process.  If a
--   controlling terminal exists,
--   @getControllingTerminalName@ returns the name of the
--   controlling terminal.
--
-- Throws 'IOError' (\"unsupported operation\") if platform does not
-- provide @ctermid(3)@ (use @#if HAVE_CTERMID@ CPP guard to
-- detect availability).
getControllingTerminalName :: IO FilePath

{-# LINE 115 "System/Posix/Terminal.hsc" #-}
getControllingTerminalName = do
  s <- throwErrnoIfNull "getControllingTerminalName" (c_ctermid nullPtr)
  peekFilePath s

foreign import capi unsafe "termios.h ctermid"
  c_ctermid :: CString -> IO CString

{-# LINE 126 "System/Posix/Terminal.hsc" #-}

-- | @getSlaveTerminalName@ calls @ptsname@ to obtain the name of the
-- slave terminal associated with a pseudoterminal pair.  The file
-- descriptor to pass in must be that of the master.
getSlaveTerminalName :: Fd -> IO FilePath


{-# LINE 133 "System/Posix/Terminal.hsc" #-}
getSlaveTerminalName (Fd fd) = do
  s <- throwErrnoIfNull "getSlaveTerminalName" (c_ptsname fd)
  peekFilePath s


{-# LINE 142 "System/Posix/Terminal.hsc" #-}
foreign import capi unsafe "HsUnix.h ptsname"
  c_ptsname :: CInt -> IO CString

{-# LINE 145 "System/Posix/Terminal.hsc" #-}

{-# LINE 149 "System/Posix/Terminal.hsc" #-}

-- -----------------------------------------------------------------------------
-- openPseudoTerminal needs to be here because it depends on
-- getSlaveTerminalName.

-- | @openPseudoTerminal@ creates a pseudoterminal (pty) pair, and
-- returns the newly created pair as a (@master@, @slave@) tuple.
openPseudoTerminal :: IO (Fd, Fd)


{-# LINE 159 "System/Posix/Terminal.hsc" #-}
openPseudoTerminal =
  alloca $ \p_master ->
    alloca $ \p_slave -> do
      throwErrnoIfMinus1_ "openPty"
          (c_openpty p_master p_slave nullPtr nullPtr nullPtr)
      master <- peek p_master
      slave <- peek p_slave
      return (Fd master, Fd slave)

foreign import ccall unsafe "openpty"
  c_openpty :: Ptr CInt -> Ptr CInt -> CString -> Ptr CTermios -> Ptr a
            -> IO CInt

{-# LINE 219 "System/Posix/Terminal.hsc" #-}

