{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-261cd265/build/System/Posix/Terminal/Common.hs" #-}
{-# LINE 1 "System/Posix/Terminal/Common.hsc" #-}
{-# LANGUAGE CApiFFI #-}
{-# LINE 2 "System/Posix/Terminal/Common.hsc" #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Terminal.Common
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

module System.Posix.Terminal.Common (
  -- * Terminal support

  -- ** Terminal attributes
  TerminalAttributes,
  getTerminalAttributes,
  TerminalState(..),
  setTerminalAttributes,

  CTermios,
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
  ) where


{-# LINE 68 "System/Posix/Terminal/Common.hsc" #-}

import Data.Bits
import Data.Char
import Foreign.C.Error ( throwErrnoIfMinus1, throwErrnoIfMinus1_ )
import Foreign.C.Types
import Foreign.ForeignPtr ( ForeignPtr, withForeignPtr, mallocForeignPtrBytes )
import Foreign.Marshal.Utils ( copyBytes )
import Foreign.Ptr ( Ptr, plusPtr )
import Foreign.Storable ( Storable(..) )
import System.IO.Unsafe ( unsafePerformIO )
import System.Posix.Types
import System.Posix.Internals ( CTermios )


{-# LINE 85 "System/Posix/Terminal/Common.hsc" #-}

-- -----------------------------------------------------------------------------
-- Terminal attributes

newtype TerminalAttributes = TerminalAttributes (ForeignPtr CTermios)

makeTerminalAttributes :: ForeignPtr CTermios -> TerminalAttributes
makeTerminalAttributes = TerminalAttributes

withTerminalAttributes :: TerminalAttributes -> (Ptr CTermios -> IO a) -> IO a
withTerminalAttributes (TerminalAttributes termios) = withForeignPtr termios


data TerminalMode
        -- input flags
   = InterruptOnBreak           -- BRKINT
   | MapCRtoLF                  -- ICRNL
   | IgnoreBreak                -- IGNBRK
   | IgnoreCR                   -- IGNCR
   | IgnoreParityErrors         -- IGNPAR
   | MapLFtoCR                  -- INLCR
   | CheckParity                -- INPCK
   | StripHighBit               -- ISTRIP
   | StartStopInput             -- IXOFF
   | StartStopOutput            -- IXON
   | MarkParityErrors           -- PARMRK

        -- output flags
   | ProcessOutput              -- OPOST
        -- ToDo: ONLCR, OCRNL, ONOCR, ONLRET, OFILL,
        --       NLDLY(NL0,NL1), CRDLY(CR0,CR1,CR2,CR2)
        --       TABDLY(TAB0,TAB1,TAB2,TAB3)
        --       BSDLY(BS0,BS1), VTDLY(VT0,VT1), FFDLY(FF0,FF1)

        -- control flags
   | LocalMode                  -- CLOCAL
   | ReadEnable                 -- CREAD
   | TwoStopBits                -- CSTOPB
   | HangupOnClose              -- HUPCL
   | EnableParity               -- PARENB
   | OddParity                  -- PARODD

        -- local modes
   | EnableEcho                 -- ECHO
   | EchoErase                  -- ECHOE
   | EchoKill                   -- ECHOK
   | EchoLF                     -- ECHONL
   | ProcessInput               -- ICANON
   | ExtendedFunctions          -- IEXTEN
   | KeyboardInterrupts         -- ISIG
   | NoFlushOnInterrupt         -- NOFLSH
   | BackgroundWriteInterrupt   -- TOSTOP

withoutMode :: TerminalAttributes -> TerminalMode -> TerminalAttributes
withoutMode termios InterruptOnBreak = clearInputFlag (2) termios
{-# LINE 140 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios MapCRtoLF = clearInputFlag (256) termios
{-# LINE 141 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios IgnoreBreak = clearInputFlag (1) termios
{-# LINE 142 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios IgnoreCR = clearInputFlag (128) termios
{-# LINE 143 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios IgnoreParityErrors = clearInputFlag (4) termios
{-# LINE 144 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios MapLFtoCR = clearInputFlag (64) termios
{-# LINE 145 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios CheckParity = clearInputFlag (16) termios
{-# LINE 146 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios StripHighBit = clearInputFlag (32) termios
{-# LINE 147 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios StartStopInput = clearInputFlag (4096) termios
{-# LINE 148 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios StartStopOutput = clearInputFlag (1024) termios
{-# LINE 149 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios MarkParityErrors = clearInputFlag (8) termios
{-# LINE 150 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios ProcessOutput = clearOutputFlag (1) termios
{-# LINE 151 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios LocalMode = clearControlFlag (2048) termios
{-# LINE 152 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios ReadEnable = clearControlFlag (128) termios
{-# LINE 153 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios TwoStopBits = clearControlFlag (64) termios
{-# LINE 154 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios HangupOnClose = clearControlFlag (1024) termios
{-# LINE 155 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios EnableParity = clearControlFlag (256) termios
{-# LINE 156 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios OddParity = clearControlFlag (512) termios
{-# LINE 157 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios EnableEcho = clearLocalFlag (8) termios
{-# LINE 158 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios EchoErase = clearLocalFlag (16) termios
{-# LINE 159 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios EchoKill = clearLocalFlag (32) termios
{-# LINE 160 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios EchoLF = clearLocalFlag (64) termios
{-# LINE 161 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios ProcessInput = clearLocalFlag (2) termios
{-# LINE 162 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios ExtendedFunctions = clearLocalFlag (32768) termios
{-# LINE 163 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios KeyboardInterrupts = clearLocalFlag (1) termios
{-# LINE 164 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios NoFlushOnInterrupt = setLocalFlag (128) termios
{-# LINE 165 "System/Posix/Terminal/Common.hsc" #-}
withoutMode termios BackgroundWriteInterrupt = clearLocalFlag (256) termios
{-# LINE 166 "System/Posix/Terminal/Common.hsc" #-}

withMode :: TerminalAttributes -> TerminalMode -> TerminalAttributes
withMode termios InterruptOnBreak = setInputFlag (2) termios
{-# LINE 169 "System/Posix/Terminal/Common.hsc" #-}
withMode termios MapCRtoLF = setInputFlag (256) termios
{-# LINE 170 "System/Posix/Terminal/Common.hsc" #-}
withMode termios IgnoreBreak = setInputFlag (1) termios
{-# LINE 171 "System/Posix/Terminal/Common.hsc" #-}
withMode termios IgnoreCR = setInputFlag (128) termios
{-# LINE 172 "System/Posix/Terminal/Common.hsc" #-}
withMode termios IgnoreParityErrors = setInputFlag (4) termios
{-# LINE 173 "System/Posix/Terminal/Common.hsc" #-}
withMode termios MapLFtoCR = setInputFlag (64) termios
{-# LINE 174 "System/Posix/Terminal/Common.hsc" #-}
withMode termios CheckParity = setInputFlag (16) termios
{-# LINE 175 "System/Posix/Terminal/Common.hsc" #-}
withMode termios StripHighBit = setInputFlag (32) termios
{-# LINE 176 "System/Posix/Terminal/Common.hsc" #-}
withMode termios StartStopInput = setInputFlag (4096) termios
{-# LINE 177 "System/Posix/Terminal/Common.hsc" #-}
withMode termios StartStopOutput = setInputFlag (1024) termios
{-# LINE 178 "System/Posix/Terminal/Common.hsc" #-}
withMode termios MarkParityErrors = setInputFlag (8) termios
{-# LINE 179 "System/Posix/Terminal/Common.hsc" #-}
withMode termios ProcessOutput = setOutputFlag (1) termios
{-# LINE 180 "System/Posix/Terminal/Common.hsc" #-}
withMode termios LocalMode = setControlFlag (2048) termios
{-# LINE 181 "System/Posix/Terminal/Common.hsc" #-}
withMode termios ReadEnable = setControlFlag (128) termios
{-# LINE 182 "System/Posix/Terminal/Common.hsc" #-}
withMode termios TwoStopBits = setControlFlag (64) termios
{-# LINE 183 "System/Posix/Terminal/Common.hsc" #-}
withMode termios HangupOnClose = setControlFlag (1024) termios
{-# LINE 184 "System/Posix/Terminal/Common.hsc" #-}
withMode termios EnableParity = setControlFlag (256) termios
{-# LINE 185 "System/Posix/Terminal/Common.hsc" #-}
withMode termios OddParity = setControlFlag (512) termios
{-# LINE 186 "System/Posix/Terminal/Common.hsc" #-}
withMode termios EnableEcho = setLocalFlag (8) termios
{-# LINE 187 "System/Posix/Terminal/Common.hsc" #-}
withMode termios EchoErase = setLocalFlag (16) termios
{-# LINE 188 "System/Posix/Terminal/Common.hsc" #-}
withMode termios EchoKill = setLocalFlag (32) termios
{-# LINE 189 "System/Posix/Terminal/Common.hsc" #-}
withMode termios EchoLF = setLocalFlag (64) termios
{-# LINE 190 "System/Posix/Terminal/Common.hsc" #-}
withMode termios ProcessInput = setLocalFlag (2) termios
{-# LINE 191 "System/Posix/Terminal/Common.hsc" #-}
withMode termios ExtendedFunctions = setLocalFlag (32768) termios
{-# LINE 192 "System/Posix/Terminal/Common.hsc" #-}
withMode termios KeyboardInterrupts = setLocalFlag (1) termios
{-# LINE 193 "System/Posix/Terminal/Common.hsc" #-}
withMode termios NoFlushOnInterrupt = clearLocalFlag (128) termios
{-# LINE 194 "System/Posix/Terminal/Common.hsc" #-}
withMode termios BackgroundWriteInterrupt = setLocalFlag (256) termios
{-# LINE 195 "System/Posix/Terminal/Common.hsc" #-}

terminalMode :: TerminalMode -> TerminalAttributes -> Bool
terminalMode InterruptOnBreak = testInputFlag (2)
{-# LINE 198 "System/Posix/Terminal/Common.hsc" #-}
terminalMode MapCRtoLF = testInputFlag (256)
{-# LINE 199 "System/Posix/Terminal/Common.hsc" #-}
terminalMode IgnoreBreak = testInputFlag (1)
{-# LINE 200 "System/Posix/Terminal/Common.hsc" #-}
terminalMode IgnoreCR = testInputFlag (128)
{-# LINE 201 "System/Posix/Terminal/Common.hsc" #-}
terminalMode IgnoreParityErrors = testInputFlag (4)
{-# LINE 202 "System/Posix/Terminal/Common.hsc" #-}
terminalMode MapLFtoCR = testInputFlag (64)
{-# LINE 203 "System/Posix/Terminal/Common.hsc" #-}
terminalMode CheckParity = testInputFlag (16)
{-# LINE 204 "System/Posix/Terminal/Common.hsc" #-}
terminalMode StripHighBit = testInputFlag (32)
{-# LINE 205 "System/Posix/Terminal/Common.hsc" #-}
terminalMode StartStopInput = testInputFlag (4096)
{-# LINE 206 "System/Posix/Terminal/Common.hsc" #-}
terminalMode StartStopOutput = testInputFlag (1024)
{-# LINE 207 "System/Posix/Terminal/Common.hsc" #-}
terminalMode MarkParityErrors = testInputFlag (8)
{-# LINE 208 "System/Posix/Terminal/Common.hsc" #-}
terminalMode ProcessOutput = testOutputFlag (1)
{-# LINE 209 "System/Posix/Terminal/Common.hsc" #-}
terminalMode LocalMode = testControlFlag (2048)
{-# LINE 210 "System/Posix/Terminal/Common.hsc" #-}
terminalMode ReadEnable = testControlFlag (128)
{-# LINE 211 "System/Posix/Terminal/Common.hsc" #-}
terminalMode TwoStopBits = testControlFlag (64)
{-# LINE 212 "System/Posix/Terminal/Common.hsc" #-}
terminalMode HangupOnClose = testControlFlag (1024)
{-# LINE 213 "System/Posix/Terminal/Common.hsc" #-}
terminalMode EnableParity = testControlFlag (256)
{-# LINE 214 "System/Posix/Terminal/Common.hsc" #-}
terminalMode OddParity = testControlFlag (512)
{-# LINE 215 "System/Posix/Terminal/Common.hsc" #-}
terminalMode EnableEcho = testLocalFlag (8)
{-# LINE 216 "System/Posix/Terminal/Common.hsc" #-}
terminalMode EchoErase = testLocalFlag (16)
{-# LINE 217 "System/Posix/Terminal/Common.hsc" #-}
terminalMode EchoKill = testLocalFlag (32)
{-# LINE 218 "System/Posix/Terminal/Common.hsc" #-}
terminalMode EchoLF = testLocalFlag (64)
{-# LINE 219 "System/Posix/Terminal/Common.hsc" #-}
terminalMode ProcessInput = testLocalFlag (2)
{-# LINE 220 "System/Posix/Terminal/Common.hsc" #-}
terminalMode ExtendedFunctions = testLocalFlag (32768)
{-# LINE 221 "System/Posix/Terminal/Common.hsc" #-}
terminalMode KeyboardInterrupts = testLocalFlag (1)
{-# LINE 222 "System/Posix/Terminal/Common.hsc" #-}
terminalMode NoFlushOnInterrupt = not . testLocalFlag (128)
{-# LINE 223 "System/Posix/Terminal/Common.hsc" #-}
terminalMode BackgroundWriteInterrupt = testLocalFlag (256)
{-# LINE 224 "System/Posix/Terminal/Common.hsc" #-}

bitsPerByte :: TerminalAttributes -> Int
bitsPerByte termios = unsafePerformIO $ do
  withTerminalAttributes termios $ \p -> do
    cflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 229 "System/Posix/Terminal/Common.hsc" #-}
    return $! (word2Bits (cflag .&. (48)))
{-# LINE 230 "System/Posix/Terminal/Common.hsc" #-}
  where
    word2Bits :: CTcflag -> Int
    word2Bits x =
        if x == (0) then 5
{-# LINE 234 "System/Posix/Terminal/Common.hsc" #-}
        else if x == (16) then 6
{-# LINE 235 "System/Posix/Terminal/Common.hsc" #-}
        else if x == (32) then 7
{-# LINE 236 "System/Posix/Terminal/Common.hsc" #-}
        else if x == (48) then 8
{-# LINE 237 "System/Posix/Terminal/Common.hsc" #-}
        else 0

withBits :: TerminalAttributes -> Int -> TerminalAttributes
withBits termios bits = unsafePerformIO $ do
  withNewTermios termios $ \p -> do
    cflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 243 "System/Posix/Terminal/Common.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p
{-# LINE 244 "System/Posix/Terminal/Common.hsc" #-}
       ((cflag .&. complement (48)) .|. mask bits)
{-# LINE 245 "System/Posix/Terminal/Common.hsc" #-}
  where
    mask :: Int -> CTcflag
    mask 5 = (0)
{-# LINE 248 "System/Posix/Terminal/Common.hsc" #-}
    mask 6 = (16)
{-# LINE 249 "System/Posix/Terminal/Common.hsc" #-}
    mask 7 = (32)
{-# LINE 250 "System/Posix/Terminal/Common.hsc" #-}
    mask 8 = (48)
{-# LINE 251 "System/Posix/Terminal/Common.hsc" #-}
    mask _ = error "withBits bit value out of range [5..8]"

data ControlCharacter
  = EndOfFile           -- VEOF
  | EndOfLine           -- VEOL
  | Erase               -- VERASE
  | Interrupt           -- VINTR
  | Kill                -- VKILL
  | Quit                -- VQUIT
  | Start               -- VSTART
  | Stop                -- VSTOP
  | Suspend             -- VSUSP

controlChar :: TerminalAttributes -> ControlCharacter -> Maybe Char
controlChar termios cc = unsafePerformIO $ do
  withTerminalAttributes termios $ \p -> do
    let c_cc = ((\hsc_ptr -> hsc_ptr `plusPtr` 17)) p
{-# LINE 268 "System/Posix/Terminal/Common.hsc" #-}
    val <- peekElemOff c_cc (cc2Word cc)
    if val == ((0)::CCc)
{-# LINE 270 "System/Posix/Terminal/Common.hsc" #-}
       then return Nothing
       else return (Just (chr (fromEnum val)))

withCC :: TerminalAttributes
       -> (ControlCharacter, Char)
       -> TerminalAttributes
withCC termios (cc, c) = unsafePerformIO $ do
  withNewTermios termios $ \p -> do
    let c_cc = ((\hsc_ptr -> hsc_ptr `plusPtr` 17)) p
{-# LINE 279 "System/Posix/Terminal/Common.hsc" #-}
    pokeElemOff c_cc (cc2Word cc) (fromIntegral (ord c) :: CCc)

withoutCC :: TerminalAttributes
          -> ControlCharacter
          -> TerminalAttributes
withoutCC termios cc = unsafePerformIO $ do
  withNewTermios termios $ \p -> do
    let c_cc = ((\hsc_ptr -> hsc_ptr `plusPtr` 17)) p
{-# LINE 287 "System/Posix/Terminal/Common.hsc" #-}
    pokeElemOff c_cc (cc2Word cc) ((0) :: CCc)
{-# LINE 288 "System/Posix/Terminal/Common.hsc" #-}

inputTime :: TerminalAttributes -> Int
inputTime termios = unsafePerformIO $ do
  withTerminalAttributes termios $ \p -> do
    c <- peekElemOff (((\hsc_ptr -> hsc_ptr `plusPtr` 17)) p) (5)
{-# LINE 293 "System/Posix/Terminal/Common.hsc" #-}
    return (fromEnum (c :: CCc))

withTime :: TerminalAttributes -> Int -> TerminalAttributes
withTime termios time = unsafePerformIO $ do
  withNewTermios termios $ \p -> do
    let c_cc = ((\hsc_ptr -> hsc_ptr `plusPtr` 17)) p
{-# LINE 299 "System/Posix/Terminal/Common.hsc" #-}
    pokeElemOff c_cc (5) (fromIntegral time :: CCc)
{-# LINE 300 "System/Posix/Terminal/Common.hsc" #-}

minInput :: TerminalAttributes -> Int
minInput termios = unsafePerformIO $ do
  withTerminalAttributes termios $ \p -> do
    c <- peekElemOff (((\hsc_ptr -> hsc_ptr `plusPtr` 17)) p) (6)
{-# LINE 305 "System/Posix/Terminal/Common.hsc" #-}
    return (fromEnum (c :: CCc))

withMinInput :: TerminalAttributes -> Int -> TerminalAttributes
withMinInput termios count = unsafePerformIO $ do
  withNewTermios termios $ \p -> do
    let c_cc = ((\hsc_ptr -> hsc_ptr `plusPtr` 17)) p
{-# LINE 311 "System/Posix/Terminal/Common.hsc" #-}
    pokeElemOff c_cc (6) (fromIntegral count :: CCc)
{-# LINE 312 "System/Posix/Terminal/Common.hsc" #-}

data BaudRate
  = B0
  | B50
  | B75
  | B110
  | B134
  | B150
  | B200
  | B300
  | B600
  | B1200
  | B1800
  | B2400
  | B4800
  | B9600
  | B19200
  | B38400
  | B57600
  | B115200

inputSpeed :: TerminalAttributes -> BaudRate
inputSpeed termios = unsafePerformIO $ do
  withTerminalAttributes termios $ \p -> do
    w <- c_cfgetispeed p
    return (word2Baud w)

foreign import capi unsafe "termios.h cfgetispeed"
  c_cfgetispeed :: Ptr CTermios -> IO CSpeed

withInputSpeed :: TerminalAttributes -> BaudRate -> TerminalAttributes
withInputSpeed termios br = unsafePerformIO $ do
  withNewTermios termios $ \p -> c_cfsetispeed p (baud2Word br)

foreign import capi unsafe "termios.h cfsetispeed"
  c_cfsetispeed :: Ptr CTermios -> CSpeed -> IO CInt


outputSpeed :: TerminalAttributes -> BaudRate
outputSpeed termios = unsafePerformIO $ do
  withTerminalAttributes termios $ \p ->  do
    w <- c_cfgetospeed p
    return (word2Baud w)

foreign import capi unsafe "termios.h cfgetospeed"
  c_cfgetospeed :: Ptr CTermios -> IO CSpeed

withOutputSpeed :: TerminalAttributes -> BaudRate -> TerminalAttributes
withOutputSpeed termios br = unsafePerformIO $ do
  withNewTermios termios $ \p -> c_cfsetospeed p (baud2Word br)

foreign import capi unsafe "termios.h cfsetospeed"
  c_cfsetospeed :: Ptr CTermios -> CSpeed -> IO CInt

-- | @getTerminalAttributes fd@ calls @tcgetattr@ to obtain
--   the @TerminalAttributes@ associated with @Fd@ @fd@.
getTerminalAttributes :: Fd -> IO TerminalAttributes
getTerminalAttributes (Fd fd) = do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 371 "System/Posix/Terminal/Common.hsc" #-}
  withForeignPtr fp $ \p ->
      throwErrnoIfMinus1_ "getTerminalAttributes" (c_tcgetattr fd p)
  return $ makeTerminalAttributes fp

foreign import capi unsafe "termios.h tcgetattr"
  c_tcgetattr :: CInt -> Ptr CTermios -> IO CInt

data TerminalState
  = Immediately
  | WhenDrained
  | WhenFlushed

-- | @setTerminalAttributes fd attr ts@ calls @tcsetattr@ to change
--   the @TerminalAttributes@ associated with @Fd@ @fd@ to
--   @attr@, when the terminal is in the state indicated by @ts@.
setTerminalAttributes :: Fd
                      -> TerminalAttributes
                      -> TerminalState
                      -> IO ()
setTerminalAttributes (Fd fd) termios state = do
  withTerminalAttributes termios $ \p ->
    throwErrnoIfMinus1_ "setTerminalAttributes"
      (c_tcsetattr fd (state2Int state) p)
  where
    state2Int :: TerminalState -> CInt
    state2Int Immediately = (0)
{-# LINE 397 "System/Posix/Terminal/Common.hsc" #-}
    state2Int WhenDrained = (1)
{-# LINE 398 "System/Posix/Terminal/Common.hsc" #-}
    state2Int WhenFlushed = (2)
{-# LINE 399 "System/Posix/Terminal/Common.hsc" #-}

foreign import capi unsafe "termios.h tcsetattr"
   c_tcsetattr :: CInt -> CInt -> Ptr CTermios -> IO CInt

-- | @sendBreak fd duration@ calls @tcsendbreak@ to transmit a
--   continuous stream of zero-valued bits on @Fd@ @fd@ for the
--   specified implementation-dependent @duration@.
sendBreak :: Fd -> Int -> IO ()
sendBreak (Fd fd) duration
  = throwErrnoIfMinus1_ "sendBreak" (c_tcsendbreak fd (fromIntegral duration))

foreign import capi unsafe "termios.h tcsendbreak"
  c_tcsendbreak :: CInt -> CInt -> IO CInt

-- | @drainOutput fd@ calls @tcdrain@ to block until all output
--   written to @Fd@ @fd@ has been transmitted.
--
-- Throws 'IOError' (\"unsupported operation\") if platform does not
-- provide @tcdrain(3)@ (use @#if HAVE_TCDRAIN@ CPP guard to
-- detect availability).
drainOutput :: Fd -> IO ()

{-# LINE 421 "System/Posix/Terminal/Common.hsc" #-}
drainOutput (Fd fd) = throwErrnoIfMinus1_ "drainOutput" (c_tcdrain fd)

foreign import capi safe "termios.h tcdrain"
  c_tcdrain :: CInt -> IO CInt

{-# LINE 430 "System/Posix/Terminal/Common.hsc" #-}

data QueueSelector
  = InputQueue          -- TCIFLUSH
  | OutputQueue         -- TCOFLUSH
  | BothQueues          -- TCIOFLUSH

-- | @discardData fd queues@ calls @tcflush@ to discard
--   pending input and\/or output for @Fd@ @fd@,
--   as indicated by the @QueueSelector@ @queues@.
discardData :: Fd -> QueueSelector -> IO ()
discardData (Fd fd) queue =
  throwErrnoIfMinus1_ "discardData" (c_tcflush fd (queue2Int queue))
  where
    queue2Int :: QueueSelector -> CInt
    queue2Int InputQueue  = (0)
{-# LINE 445 "System/Posix/Terminal/Common.hsc" #-}
    queue2Int OutputQueue = (1)
{-# LINE 446 "System/Posix/Terminal/Common.hsc" #-}
    queue2Int BothQueues  = (2)
{-# LINE 447 "System/Posix/Terminal/Common.hsc" #-}

foreign import capi unsafe "termios.h tcflush"
  c_tcflush :: CInt -> CInt -> IO CInt

data FlowAction
  = SuspendOutput       -- ^ TCOOFF
  | RestartOutput       -- ^ TCOON
  | TransmitStop        -- ^ TCIOFF
  | TransmitStart       -- ^ TCION

-- | @controlFlow fd action@ calls @tcflow@ to control the
--   flow of data on @Fd@ @fd@, as indicated by
--   @action@.
controlFlow :: Fd -> FlowAction -> IO ()
controlFlow (Fd fd) action =
  throwErrnoIfMinus1_ "controlFlow" (c_tcflow fd (action2Int action))
  where
    action2Int :: FlowAction -> CInt
    action2Int SuspendOutput = (0)
{-# LINE 466 "System/Posix/Terminal/Common.hsc" #-}
    action2Int RestartOutput = (1)
{-# LINE 467 "System/Posix/Terminal/Common.hsc" #-}
    action2Int TransmitStop  = (2)
{-# LINE 468 "System/Posix/Terminal/Common.hsc" #-}
    action2Int TransmitStart = (3)
{-# LINE 469 "System/Posix/Terminal/Common.hsc" #-}

foreign import capi unsafe "termios.h tcflow"
  c_tcflow :: CInt -> CInt -> IO CInt

-- | @getTerminalProcessGroupID fd@ calls @tcgetpgrp@ to
--   obtain the @ProcessGroupID@ of the foreground process group
--   associated with the terminal attached to @Fd@ @fd@.
getTerminalProcessGroupID :: Fd -> IO ProcessGroupID
getTerminalProcessGroupID (Fd fd) = do
  throwErrnoIfMinus1 "getTerminalProcessGroupID" (c_tcgetpgrp fd)

foreign import ccall unsafe "tcgetpgrp"
  c_tcgetpgrp :: CInt -> IO CPid

-- | @setTerminalProcessGroupID fd pgid@ calls @tcsetpgrp@ to
--   set the @ProcessGroupID@ of the foreground process group
--   associated with the terminal attached to @Fd@
--   @fd@ to @pgid@.
setTerminalProcessGroupID :: Fd -> ProcessGroupID -> IO ()
setTerminalProcessGroupID (Fd fd) pgid =
  throwErrnoIfMinus1_ "setTerminalProcessGroupID" (c_tcsetpgrp fd pgid)

foreign import ccall unsafe "tcsetpgrp"
  c_tcsetpgrp :: CInt -> CPid -> IO CInt

-- -----------------------------------------------------------------------------
-- file descriptor queries

-- | @queryTerminal fd@ calls @isatty@ to determine whether or
--   not @Fd@ @fd@ is associated with a terminal.
queryTerminal :: Fd -> IO Bool
queryTerminal (Fd fd) = do
  r <- c_isatty fd
  return (r == 1)
  -- ToDo: the spec says that it can set errno to EBADF if the result is zero

foreign import ccall unsafe "isatty"
  c_isatty :: CInt -> IO CInt

-- -----------------------------------------------------------------------------
-- Local utility functions

-- Convert Haskell ControlCharacter to Int

cc2Word :: ControlCharacter -> Int
cc2Word EndOfFile = (4)
{-# LINE 515 "System/Posix/Terminal/Common.hsc" #-}
cc2Word EndOfLine = (11)
{-# LINE 516 "System/Posix/Terminal/Common.hsc" #-}
cc2Word Erase     = (2)
{-# LINE 517 "System/Posix/Terminal/Common.hsc" #-}
cc2Word Interrupt = (0)
{-# LINE 518 "System/Posix/Terminal/Common.hsc" #-}
cc2Word Kill      = (3)
{-# LINE 519 "System/Posix/Terminal/Common.hsc" #-}
cc2Word Quit      = (1)
{-# LINE 520 "System/Posix/Terminal/Common.hsc" #-}
cc2Word Suspend   = (10)
{-# LINE 521 "System/Posix/Terminal/Common.hsc" #-}
cc2Word Start     = (8)
{-# LINE 522 "System/Posix/Terminal/Common.hsc" #-}
cc2Word Stop      = (9)
{-# LINE 523 "System/Posix/Terminal/Common.hsc" #-}

-- Convert Haskell BaudRate to unsigned integral type (Word)

baud2Word :: BaudRate -> CSpeed
baud2Word B0 = (0)
{-# LINE 528 "System/Posix/Terminal/Common.hsc" #-}
baud2Word B50 = (1)
{-# LINE 529 "System/Posix/Terminal/Common.hsc" #-}
baud2Word B75 = (2)
{-# LINE 530 "System/Posix/Terminal/Common.hsc" #-}
baud2Word B110 = (3)
{-# LINE 531 "System/Posix/Terminal/Common.hsc" #-}
baud2Word B134 = (4)
{-# LINE 532 "System/Posix/Terminal/Common.hsc" #-}
baud2Word B150 = (5)
{-# LINE 533 "System/Posix/Terminal/Common.hsc" #-}
baud2Word B200 = (6)
{-# LINE 534 "System/Posix/Terminal/Common.hsc" #-}
baud2Word B300 = (7)
{-# LINE 535 "System/Posix/Terminal/Common.hsc" #-}
baud2Word B600 = (8)
{-# LINE 536 "System/Posix/Terminal/Common.hsc" #-}
baud2Word B1200 = (9)
{-# LINE 537 "System/Posix/Terminal/Common.hsc" #-}
baud2Word B1800 = (10)
{-# LINE 538 "System/Posix/Terminal/Common.hsc" #-}
baud2Word B2400 = (11)
{-# LINE 539 "System/Posix/Terminal/Common.hsc" #-}
baud2Word B4800 = (12)
{-# LINE 540 "System/Posix/Terminal/Common.hsc" #-}
baud2Word B9600 = (13)
{-# LINE 541 "System/Posix/Terminal/Common.hsc" #-}
baud2Word B19200 = (14)
{-# LINE 542 "System/Posix/Terminal/Common.hsc" #-}
baud2Word B38400 = (15)
{-# LINE 543 "System/Posix/Terminal/Common.hsc" #-}

{-# LINE 544 "System/Posix/Terminal/Common.hsc" #-}
baud2Word B57600 = (4097)
{-# LINE 545 "System/Posix/Terminal/Common.hsc" #-}

{-# LINE 548 "System/Posix/Terminal/Common.hsc" #-}

{-# LINE 549 "System/Posix/Terminal/Common.hsc" #-}
baud2Word B115200 = (4098)
{-# LINE 550 "System/Posix/Terminal/Common.hsc" #-}

{-# LINE 553 "System/Posix/Terminal/Common.hsc" #-}

-- And convert a word back to a baud rate
-- We really need some cpp macros here.

word2Baud :: CSpeed -> BaudRate
word2Baud x =
    if x == (0) then B0
{-# LINE 560 "System/Posix/Terminal/Common.hsc" #-}
    else if x == (1) then B50
{-# LINE 561 "System/Posix/Terminal/Common.hsc" #-}
    else if x == (2) then B75
{-# LINE 562 "System/Posix/Terminal/Common.hsc" #-}
    else if x == (3) then B110
{-# LINE 563 "System/Posix/Terminal/Common.hsc" #-}
    else if x == (4) then B134
{-# LINE 564 "System/Posix/Terminal/Common.hsc" #-}
    else if x == (5) then B150
{-# LINE 565 "System/Posix/Terminal/Common.hsc" #-}
    else if x == (6) then B200
{-# LINE 566 "System/Posix/Terminal/Common.hsc" #-}
    else if x == (7) then B300
{-# LINE 567 "System/Posix/Terminal/Common.hsc" #-}
    else if x == (8) then B600
{-# LINE 568 "System/Posix/Terminal/Common.hsc" #-}
    else if x == (9) then B1200
{-# LINE 569 "System/Posix/Terminal/Common.hsc" #-}
    else if x == (10) then B1800
{-# LINE 570 "System/Posix/Terminal/Common.hsc" #-}
    else if x == (11) then B2400
{-# LINE 571 "System/Posix/Terminal/Common.hsc" #-}
    else if x == (12) then B4800
{-# LINE 572 "System/Posix/Terminal/Common.hsc" #-}
    else if x == (13) then B9600
{-# LINE 573 "System/Posix/Terminal/Common.hsc" #-}
    else if x == (14) then B19200
{-# LINE 574 "System/Posix/Terminal/Common.hsc" #-}
    else if x == (15) then B38400
{-# LINE 575 "System/Posix/Terminal/Common.hsc" #-}

{-# LINE 576 "System/Posix/Terminal/Common.hsc" #-}
    else if x == (4097) then B57600
{-# LINE 577 "System/Posix/Terminal/Common.hsc" #-}

{-# LINE 578 "System/Posix/Terminal/Common.hsc" #-}

{-# LINE 579 "System/Posix/Terminal/Common.hsc" #-}
    else if x == (4098) then B115200
{-# LINE 580 "System/Posix/Terminal/Common.hsc" #-}

{-# LINE 581 "System/Posix/Terminal/Common.hsc" #-}
    else error "unknown baud rate"

-- Clear termios i_flag

clearInputFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
clearInputFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 588 "System/Posix/Terminal/Common.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60)
{-# LINE 591 "System/Posix/Terminal/Common.hsc" #-}
      iflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p2
{-# LINE 592 "System/Posix/Terminal/Common.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p1 (iflag .&. complement flag)
{-# LINE 593 "System/Posix/Terminal/Common.hsc" #-}
  return $ makeTerminalAttributes fp

-- Set termios i_flag

setInputFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
setInputFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 600 "System/Posix/Terminal/Common.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60)
{-# LINE 603 "System/Posix/Terminal/Common.hsc" #-}
      iflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p2
{-# LINE 604 "System/Posix/Terminal/Common.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p1 (iflag .|. flag)
{-# LINE 605 "System/Posix/Terminal/Common.hsc" #-}
  return $ makeTerminalAttributes fp

-- Examine termios i_flag

testInputFlag :: CTcflag -> TerminalAttributes -> Bool
testInputFlag flag termios = unsafePerformIO $
  withTerminalAttributes termios $ \p ->  do
    iflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 613 "System/Posix/Terminal/Common.hsc" #-}
    return $! ((iflag .&. flag) /= 0)

-- Clear termios c_flag

clearControlFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
clearControlFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 620 "System/Posix/Terminal/Common.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60)
{-# LINE 623 "System/Posix/Terminal/Common.hsc" #-}
      cflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p2
{-# LINE 624 "System/Posix/Terminal/Common.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p1 (cflag .&. complement flag)
{-# LINE 625 "System/Posix/Terminal/Common.hsc" #-}
  return $ makeTerminalAttributes fp

-- Set termios c_flag

setControlFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
setControlFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 632 "System/Posix/Terminal/Common.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60)
{-# LINE 635 "System/Posix/Terminal/Common.hsc" #-}
      cflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p2
{-# LINE 636 "System/Posix/Terminal/Common.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p1 (cflag .|. flag)
{-# LINE 637 "System/Posix/Terminal/Common.hsc" #-}
  return $ makeTerminalAttributes fp

-- Examine termios c_flag

testControlFlag :: CTcflag -> TerminalAttributes -> Bool
testControlFlag flag termios = unsafePerformIO $
  withTerminalAttributes termios $ \p -> do
    cflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 645 "System/Posix/Terminal/Common.hsc" #-}
    return $! ((cflag .&. flag) /= 0)

-- Clear termios l_flag

clearLocalFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
clearLocalFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 652 "System/Posix/Terminal/Common.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60)
{-# LINE 655 "System/Posix/Terminal/Common.hsc" #-}
      lflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) p2
{-# LINE 656 "System/Posix/Terminal/Common.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 12)) p1 (lflag .&. complement flag)
{-# LINE 657 "System/Posix/Terminal/Common.hsc" #-}
  return $ makeTerminalAttributes fp

-- Set termios l_flag

setLocalFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
setLocalFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 664 "System/Posix/Terminal/Common.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60)
{-# LINE 667 "System/Posix/Terminal/Common.hsc" #-}
      lflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) p2
{-# LINE 668 "System/Posix/Terminal/Common.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 12)) p1 (lflag .|. flag)
{-# LINE 669 "System/Posix/Terminal/Common.hsc" #-}
  return $ makeTerminalAttributes fp

-- Examine termios l_flag

testLocalFlag :: CTcflag -> TerminalAttributes -> Bool
testLocalFlag flag termios = unsafePerformIO $
  withTerminalAttributes termios $ \p ->  do
    lflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) p
{-# LINE 677 "System/Posix/Terminal/Common.hsc" #-}
    return $! ((lflag .&. flag) /= 0)

-- Clear termios o_flag

clearOutputFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
clearOutputFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 684 "System/Posix/Terminal/Common.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60)
{-# LINE 687 "System/Posix/Terminal/Common.hsc" #-}
      oflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p2
{-# LINE 688 "System/Posix/Terminal/Common.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p1 (oflag .&. complement flag)
{-# LINE 689 "System/Posix/Terminal/Common.hsc" #-}
  return $ makeTerminalAttributes fp

-- Set termios o_flag

setOutputFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
setOutputFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 696 "System/Posix/Terminal/Common.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60)
{-# LINE 699 "System/Posix/Terminal/Common.hsc" #-}
      oflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p2
{-# LINE 700 "System/Posix/Terminal/Common.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p1 (oflag .|. flag)
{-# LINE 701 "System/Posix/Terminal/Common.hsc" #-}
  return $ makeTerminalAttributes fp

-- Examine termios o_flag

testOutputFlag :: CTcflag -> TerminalAttributes -> Bool
testOutputFlag flag termios = unsafePerformIO $
  withTerminalAttributes termios $ \p -> do
    oflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 709 "System/Posix/Terminal/Common.hsc" #-}
    return $! ((oflag .&. flag) /= 0)

withNewTermios :: TerminalAttributes -> (Ptr CTermios -> IO a)
  -> IO TerminalAttributes
withNewTermios termios action = do
  fp1 <- mallocForeignPtrBytes (60)
{-# LINE 715 "System/Posix/Terminal/Common.hsc" #-}
  withForeignPtr fp1 $ \p1 -> do
   withTerminalAttributes termios $ \p2 -> do
    copyBytes p1 p2 (60)
{-# LINE 718 "System/Posix/Terminal/Common.hsc" #-}
    _ <- action p1
    return ()
  return $ makeTerminalAttributes fp1
