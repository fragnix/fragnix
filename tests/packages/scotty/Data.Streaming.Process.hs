{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Data/Streaming/Process.hs" #-}





















































































{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | A full tutorial for this module is available at:
-- <https://github.com/snoyberg/conduit/blob/master/PROCESS.md>.
--
-- Note that, while the tutorial covers @Data.Conduit.Process@, that module closely
-- follows the present one, and almost all concepts in the tutorial apply here.
module Data.Streaming.Process
    ( -- * Functions
      streamingProcess
    , closeStreamingProcessHandle
      -- * Specialized streaming types
    , Inherited (..)
    , ClosedStream (..)
    , UseProvidedHandle (..)
      -- * Process handle
    , StreamingProcessHandle
    , waitForStreamingProcess
    , waitForStreamingProcessSTM
    , getStreamingProcessExitCode
    , getStreamingProcessExitCodeSTM
    , streamingProcessHandleRaw
    , streamingProcessHandleTMVar
      -- * Type classes
    , InputSource
    , OutputSink
      -- * Checked processes
    , withCheckedProcess
    , ProcessExitedUnsuccessfully (..)
      -- * Reexport
    , module System.Process
    ) where

import           Control.Applicative             ((<$>), (<*>))
import           Control.Concurrent              (forkIOWithUnmask)
import           Control.Concurrent.STM          (STM, TMVar, atomically,
                                                  newEmptyTMVar, putTMVar,
                                                  readTMVar)
import           Control.Exception               (Exception, throwIO, try, throw,
                                                  SomeException, finally)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Data.Maybe                      (fromMaybe)
import           Data.Streaming.Process.Internal
import           Data.Typeable                   (Typeable)
import           System.Exit                     (ExitCode (ExitSuccess))
import           System.IO                       (hClose)
import           System.Process

import qualified System.Process.Internals        as PI

import           Control.Concurrent.STM          (tryReadTMVar)

-- | Use the @Handle@ provided by the @CreateProcess@ value. This would allow
-- you, for example, to open up a @Handle@ to a file, set it as @std_out@, and
-- avoid any additional overhead of dealing with providing that data to your
-- process.
--
-- Since 0.1.4
data UseProvidedHandle = UseProvidedHandle

-- | Inherit the stream from the current process.
--
-- Since 0.1.4
data Inherited = Inherited

-- | Close the stream with the child process.
--
-- Since 0.1.4
data ClosedStream = ClosedStream

instance InputSource ClosedStream where
    isStdStream = (\(Just h) -> hClose h >> return ClosedStream, Just CreatePipe)
instance InputSource Inherited where
    isStdStream = (\Nothing -> return Inherited, Just Inherit)
instance InputSource UseProvidedHandle where
    isStdStream = (\Nothing -> return UseProvidedHandle, Nothing)

instance OutputSink ClosedStream where
    osStdStream = (\(Just h) -> hClose h >> return ClosedStream, Just CreatePipe)
instance OutputSink Inherited where
    osStdStream = (\Nothing -> return Inherited, Just Inherit)
instance OutputSink UseProvidedHandle where
    osStdStream = (\Nothing -> return UseProvidedHandle, Nothing)

-- | Blocking call to wait for a process to exit.
--
-- Since 0.1.4
waitForStreamingProcess :: MonadIO m => StreamingProcessHandle -> m ExitCode
waitForStreamingProcess = liftIO . atomically . waitForStreamingProcessSTM

-- | STM version of @waitForStreamingProcess@.
--
-- Since 0.1.4
waitForStreamingProcessSTM :: StreamingProcessHandle -> STM ExitCode
waitForStreamingProcessSTM = readTMVar . streamingProcessHandleTMVar

-- | Non-blocking call to check for a process exit code.
--
-- Since 0.1.4
getStreamingProcessExitCode :: MonadIO m => StreamingProcessHandle -> m (Maybe ExitCode)
getStreamingProcessExitCode = liftIO . atomically .  getStreamingProcessExitCodeSTM

-- | STM version of @getStreamingProcessExitCode@.
--
-- Since 0.1.4
getStreamingProcessExitCodeSTM :: StreamingProcessHandle -> STM (Maybe ExitCode)
getStreamingProcessExitCodeSTM = tryReadTMVar . streamingProcessHandleTMVar

-- | Get the raw @ProcessHandle@ from a @StreamingProcessHandle@. Note that
-- you should avoid using this to get the process exit code, and instead
-- use the provided functions.
--
-- Since 0.1.4
streamingProcessHandleRaw :: StreamingProcessHandle -> ProcessHandle
streamingProcessHandleRaw (StreamingProcessHandle ph _ _) = ph

-- | Get the @TMVar@ storing the process exit code. In general, one of the
-- above functions should be used instead to avoid accidentally corrupting the variable\'s state..
--
-- Since 0.1.4
streamingProcessHandleTMVar :: StreamingProcessHandle -> TMVar ExitCode
streamingProcessHandleTMVar (StreamingProcessHandle _ var _) = var

-- | The primary function for running a process. Note that, with the
-- exception of 'UseProvidedHandle', the values for @std_in@, @std_out@
-- and @std_err@ will be ignored by this function.
--
-- Since 0.1.4
streamingProcess :: (MonadIO m, InputSource stdin, OutputSink stdout, OutputSink stderr)
               => CreateProcess
               -> m (stdin, stdout, stderr, StreamingProcessHandle)
streamingProcess cp = liftIO $ do
    let (getStdin, stdinStream) = isStdStream
        (getStdout, stdoutStream) = osStdStream
        (getStderr, stderrStream) = osStdStream

    (stdinH, stdoutH, stderrH, ph) <- PI.createProcess_ "streamingProcess" cp
        { std_in = fromMaybe (std_in cp) stdinStream
        , std_out = fromMaybe (std_out cp) stdoutStream
        , std_err = fromMaybe (std_err cp) stderrStream
        }

    ec <- atomically newEmptyTMVar
    -- Apparently waitForProcess can throw an exception itself when
    -- delegate_ctlc is True, so to avoid this TMVar from being left empty, we
    -- capture any exceptions and store them as an impure exception in the
    -- TMVar
    _ <- forkIOWithUnmask $ \_unmask -> try (waitForProcess ph)
        >>= atomically
          . putTMVar ec
          . either
              (throw :: SomeException -> a)
              id

    let close =
            mclose stdinH `finally` mclose stdoutH `finally` mclose stderrH
          where
            mclose = maybe (return ()) hClose

    (,,,)
        <$> getStdin stdinH
        <*> getStdout stdoutH
        <*> getStderr stderrH
        <*> return (StreamingProcessHandle ph ec close)

-- | Free any resources (e.g. @Handle@s) acquired by a call to 'streamingProcess'.
--
-- @since 0.1.16
closeStreamingProcessHandle :: MonadIO m => StreamingProcessHandle -> m ()
closeStreamingProcessHandle (StreamingProcessHandle _ _ f) = liftIO f

-- | Indicates that a process exited with an non-success exit code.
--
-- Since 0.1.7
data ProcessExitedUnsuccessfully = ProcessExitedUnsuccessfully CreateProcess ExitCode
    deriving Typeable
instance Show ProcessExitedUnsuccessfully where
    show (ProcessExitedUnsuccessfully cp ec) = concat
        [ "Process exited with "
        , show ec
        , ": "
        , showCmdSpec (cmdspec cp)
        ]
      where
        showCmdSpec (ShellCommand str) = str
        showCmdSpec (RawCommand x xs) = unwords (x:map showArg xs)

        -- Ensure that strings that need to be escaped are
        showArg x
            | any (\c -> c == '"' || c == ' ') x = show x
            | otherwise = x
instance Exception ProcessExitedUnsuccessfully

-- | Run a process and supply its streams to the given callback function. After
-- the callback completes, wait for the process to complete and check its exit
-- code. If the exit code is not a success, throw a
-- 'ProcessExitedUnsuccessfully'.
--
-- NOTE: This function does not kill the child process or ensure
-- resources are cleaned up in the event of an exception from the
-- provided function. For that, please use @withCheckedProcessCleanup@
-- from the @conduit-extra@ package.
--
-- Since 0.1.7
withCheckedProcess :: ( InputSource stdin
                      , OutputSink stderr
                      , OutputSink stdout
                      , MonadIO m
                      )
                   => CreateProcess
                   -> (stdin -> stdout -> stderr -> m b)
                   -> m b
withCheckedProcess cp f = do
    (x, y, z, sph) <- streamingProcess cp
    res <- f x y z
    liftIO $ do
        ec <- waitForStreamingProcess sph `finally` closeStreamingProcessHandle sph
        if ec == ExitSuccess
            then return res
            else throwIO $ ProcessExitedUnsuccessfully cp ec
