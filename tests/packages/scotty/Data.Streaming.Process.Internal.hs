{-# LINE 1 "Data/Streaming/Process/Internal.hs" #-}
module Data.Streaming.Process.Internal
    ( StreamingProcessHandle (..)
    , InputSource (..)
    , OutputSink (..)
    ) where

import           Control.Concurrent.STM (TMVar)
import           System.Exit            (ExitCode)
import           System.IO              (Handle)
import           System.Process         (ProcessHandle, StdStream (CreatePipe))

-- | Class for all things which can be used to provide standard input.
--
-- Since 0.1.4
class InputSource a where
    isStdStream :: (Maybe Handle -> IO a, Maybe StdStream)
instance InputSource Handle where
    isStdStream = (\(Just h) -> return h, Just CreatePipe)

-- | Class for all things which can be used to consume standard output or
-- error.
--
-- Since 0.1.4
class OutputSink a where
    osStdStream :: (Maybe Handle -> IO a, Maybe StdStream)
instance OutputSink Handle where
    osStdStream = (\(Just h) -> return h, Just CreatePipe)

-- | Wraps up the standard @ProcessHandle@ to avoid the @waitForProcess@
-- deadlock. See the linked documentation from the module header for more
-- information.
--
-- Since 0.1.4
data StreamingProcessHandle = StreamingProcessHandle
    ProcessHandle
    (TMVar ExitCode)
