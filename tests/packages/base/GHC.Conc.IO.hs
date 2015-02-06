{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "GHC/Conc/IO.hs" #-}













































{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , MagicHash
           , UnboxedTuples
  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Conc.IO
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic concurrency stuff.
--
-----------------------------------------------------------------------------

-- No: #hide, because bits of this module are exposed by the stm package.
-- However, we don't want this module to be the home location for the
-- bits it exports, we'd rather have Control.Concurrent and the other
-- higher level modules be the home.  Hence: #not-home

module GHC.Conc.IO
        ( ensureIOManagerIsRunning
        , ioManagerCapabilitiesChanged

        -- * Waiting
        , threadDelay
        , registerDelay
        , threadWaitRead
        , threadWaitWrite
        , threadWaitReadSTM
        , threadWaitWriteSTM
        , closeFdWith

        ) where

import Foreign
import GHC.Base
import GHC.Conc.Sync as Sync
import GHC.Real ( fromIntegral )
import System.Posix.Types

import qualified GHC.Event.Thread as Event

ensureIOManagerIsRunning :: IO ()
ensureIOManagerIsRunning = Event.ensureIOManagerIsRunning

ioManagerCapabilitiesChanged :: IO ()
ioManagerCapabilitiesChanged = Event.ioManagerCapabilitiesChanged

-- | Block the current thread until data is available to read on the
-- given file descriptor (GHC only).
--
-- This will throw an 'IOError' if the file descriptor was closed
-- while this thread was blocked.  To safely close a file descriptor
-- that has been used with 'threadWaitRead', use 'closeFdWith'.
threadWaitRead :: Fd -> IO ()
threadWaitRead fd
  | threaded  = Event.threadWaitRead fd
  | otherwise = IO $ \s ->
        case fromIntegral fd of { I# fd# ->
        case waitRead# fd# s of { s' -> (# s', () #)
        }}

-- | Block the current thread until data can be written to the
-- given file descriptor (GHC only).
--
-- This will throw an 'IOError' if the file descriptor was closed
-- while this thread was blocked.  To safely close a file descriptor
-- that has been used with 'threadWaitWrite', use 'closeFdWith'.
threadWaitWrite :: Fd -> IO ()
threadWaitWrite fd
  | threaded  = Event.threadWaitWrite fd
  | otherwise = IO $ \s ->
        case fromIntegral fd of { I# fd# ->
        case waitWrite# fd# s of { s' -> (# s', () #)
        }}

-- | Returns an STM action that can be used to wait for data
-- to read from a file descriptor. The second returned value
-- is an IO action that can be used to deregister interest
-- in the file descriptor.
threadWaitReadSTM :: Fd -> IO (Sync.STM (), IO ())
threadWaitReadSTM fd 
  | threaded  = Event.threadWaitReadSTM fd
  | otherwise = do
      m <- Sync.newTVarIO False
      _ <- Sync.forkIO $ do
        threadWaitRead fd
        Sync.atomically $ Sync.writeTVar m True
      let waitAction = do b <- Sync.readTVar m
                          if b then return () else retry
      let killAction = return ()
      return (waitAction, killAction)

-- | Returns an STM action that can be used to wait until data
-- can be written to a file descriptor. The second returned value
-- is an IO action that can be used to deregister interest
-- in the file descriptor.
threadWaitWriteSTM :: Fd -> IO (Sync.STM (), IO ())
threadWaitWriteSTM fd 
  | threaded  = Event.threadWaitWriteSTM fd
  | otherwise = do
      m <- Sync.newTVarIO False
      _ <- Sync.forkIO $ do
        threadWaitWrite fd
        Sync.atomically $ Sync.writeTVar m True
      let waitAction = do b <- Sync.readTVar m
                          if b then return () else retry
      let killAction = return ()
      return (waitAction, killAction)

-- | Close a file descriptor in a concurrency-safe way (GHC only).  If
-- you are using 'threadWaitRead' or 'threadWaitWrite' to perform
-- blocking I\/O, you /must/ use this function to close file
-- descriptors, or blocked threads may not be woken.
--
-- Any threads that are blocked on the file descriptor via
-- 'threadWaitRead' or 'threadWaitWrite' will be unblocked by having
-- IO exceptions thrown.
closeFdWith :: (Fd -> IO ()) -- ^ Low-level action that performs the real close.
            -> Fd            -- ^ File descriptor to close.
            -> IO ()
closeFdWith close fd
  | threaded  = Event.closeFdWith close fd
  | otherwise = close fd

-- | Suspends the current thread for a given number of microseconds
-- (GHC only).
--
-- There is no guarantee that the thread will be rescheduled promptly
-- when the delay has expired, but the thread will never continue to
-- run /earlier/ than specified.
--
threadDelay :: Int -> IO ()
threadDelay time
  | threaded  = Event.threadDelay time
  | otherwise = IO $ \s ->
        case time of { I# time# ->
        case delay# time# s of { s' -> (# s', () #)
        }}

-- | Set the value of returned TVar to True after a given number of
-- microseconds. The caveats associated with threadDelay also apply.
--
registerDelay :: Int -> IO (TVar Bool)
registerDelay usecs
  | threaded = Event.registerDelay usecs
  | otherwise = error "registerDelay: requires -threaded"

foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool
