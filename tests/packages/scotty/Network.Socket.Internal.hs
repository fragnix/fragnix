{-# LANGUAGE Haskell98, CPP, DeriveDataTypeable, ForeignFunctionInterface, TypeSynonymInstances #-}
{-# LINE 1 "dist/dist-sandbox-261cd265/build/Network/Socket/Internal.hs" #-}




















































{-# LINE 1 "Network/Socket/Internal.hsc" #-}
{-# LANGUAGE CPP #-}
{-# LINE 2 "Network/Socket/Internal.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Socket.Internal
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/network/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A module containing semi-public 'Network.Socket' internals.
-- Modules which extend the 'Network.Socket' module will need to use
-- this module while ideally most users will be able to make do with
-- the public interface.
--
-----------------------------------------------------------------------------


{-# LINE 22 "Network/Socket/Internal.hsc" #-}

module Network.Socket.Internal
    (
    -- * Socket addresses
      HostAddress

{-# LINE 28 "Network/Socket/Internal.hsc" #-}
    , HostAddress6
    , FlowInfo
    , ScopeID

{-# LINE 32 "Network/Socket/Internal.hsc" #-}
    , PortNumber(..)
    , SockAddr(..)

    , peekSockAddr
    , pokeSockAddr
    , sizeOfSockAddr
    , sizeOfSockAddrByFamily
    , withSockAddr
    , withNewSockAddr

    -- * Protocol families
    , Family(..)

    -- * Socket error functions

{-# LINE 49 "Network/Socket/Internal.hsc" #-}
    , throwSocketError
    , throwSocketErrorCode

    -- * Guards for socket operations that may fail
    , throwSocketErrorIfMinus1_
    , throwSocketErrorIfMinus1Retry
    , throwSocketErrorIfMinus1Retry_
    , throwSocketErrorIfMinus1RetryMayBlock

    -- ** Guards that wait and retry if the operation would block
    -- | These guards are based on 'throwSocketErrorIfMinus1RetryMayBlock'.
    -- They wait for socket readiness if the action fails with @EWOULDBLOCK@
    -- or similar.
    , throwSocketErrorWaitRead
    , throwSocketErrorWaitWrite

    -- * Initialization
    , withSocketsDo

    -- * Low-level helpers
    , zeroMemory
    ) where

import Foreign.C.Error (throwErrno, throwErrnoIfMinus1Retry,
                        throwErrnoIfMinus1RetryMayBlock, throwErrnoIfMinus1_,
                        Errno(..), errnoToIOError)

{-# LINE 79 "Network/Socket/Internal.hsc" #-}
import Foreign.C.Types (CInt(..))
import GHC.Conc (threadWaitRead, threadWaitWrite)


{-# LINE 94 "Network/Socket/Internal.hsc" #-}

import Network.Socket.Types

-- ---------------------------------------------------------------------
-- Guards for socket operations that may fail

-- | Throw an 'IOError' corresponding to the current socket error.
throwSocketError :: String  -- ^ textual description of the error location
                 -> IO a

-- | Like 'throwSocketError', but the error code is supplied as an argument.
--
-- On Windows, do not use errno.  Use a system error code instead.
throwSocketErrorCode :: String -> CInt -> IO a

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@.  Discards the result of the
-- IO action after error handling.
throwSocketErrorIfMinus1_
    :: (Eq a, Num a)
    => String  -- ^ textual description of the location
    -> IO a    -- ^ the 'IO' operation to be executed
    -> IO ()

{-# SPECIALIZE throwSocketErrorIfMinus1_ :: String -> IO CInt -> IO () #-}

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@, but retries in case of an
-- interrupted operation.
throwSocketErrorIfMinus1Retry
    :: (Eq a, Num a)
    => String  -- ^ textual description of the location
    -> IO a    -- ^ the 'IO' operation to be executed
    -> IO a

{-# SPECIALIZE throwSocketErrorIfMinus1Retry :: String -> IO CInt -> IO CInt #-}

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@, but retries in case of an
-- interrupted operation. Discards the result of the IO action after
-- error handling.
throwSocketErrorIfMinus1Retry_
    :: (Eq a, Num a)
    => String  -- ^ textual description of the location
    -> IO a    -- ^ the 'IO' operation to be executed
    -> IO ()
throwSocketErrorIfMinus1Retry_ loc m =
    throwSocketErrorIfMinus1Retry loc m >> return ()
{-# SPECIALIZE throwSocketErrorIfMinus1Retry_ :: String -> IO CInt -> IO () #-}

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@, but retries in case of an
-- interrupted operation.  Checks for operations that would block and
-- executes an alternative action before retrying in that case.
throwSocketErrorIfMinus1RetryMayBlock
    :: (Eq a, Num a)
    => String  -- ^ textual description of the location
    -> IO b    -- ^ action to execute before retrying if an
               --   immediate retry would block
    -> IO a    -- ^ the 'IO' operation to be executed
    -> IO a

{-# SPECIALIZE throwSocketErrorIfMinus1RetryMayBlock
        :: String -> IO b -> IO CInt -> IO CInt #-}


{-# LINE 160 "Network/Socket/Internal.hsc" #-}

throwSocketErrorIfMinus1RetryMayBlock name on_block act =
    throwErrnoIfMinus1RetryMayBlock name act on_block

throwSocketErrorIfMinus1Retry = throwErrnoIfMinus1Retry

throwSocketErrorIfMinus1_ = throwErrnoIfMinus1_

throwSocketError = throwErrno

throwSocketErrorCode loc errno =
    ioError (errnoToIOError loc (Errno errno) Nothing Nothing)


{-# LINE 220 "Network/Socket/Internal.hsc" #-}

-- | Like 'throwSocketErrorIfMinus1Retry', but if the action fails with
-- @EWOULDBLOCK@ or similar, wait for the socket to be read-ready,
-- and try again.
throwSocketErrorWaitRead :: (Eq a, Num a) => Socket -> String -> IO a -> IO a
throwSocketErrorWaitRead sock name io =
    throwSocketErrorIfMinus1RetryMayBlock name
        (threadWaitRead $ fromIntegral $ sockFd sock)
        io

-- | Like 'throwSocketErrorIfMinus1Retry', but if the action fails with
-- @EWOULDBLOCK@ or similar, wait for the socket to be write-ready,
-- and try again.
throwSocketErrorWaitWrite :: (Eq a, Num a) => Socket -> String -> IO a -> IO a
throwSocketErrorWaitWrite sock name io =
    throwSocketErrorIfMinus1RetryMayBlock name
        (threadWaitWrite $ fromIntegral $ sockFd sock)
        io

-- ---------------------------------------------------------------------------
-- WinSock support

{-| With older versions of the @network@ library on Windows operating systems,
the networking subsystem must be initialised using 'withSocketsDo' before
any networking operations can be used. eg.

> main = withSocketsDo $ do {...}

It is fine to nest calls to 'withSocketsDo', and to perform networking operations
after 'withSocketsDo' has returned.

In newer versions of the @network@ library it is only necessary to call
'withSocketsDo' if you are calling the 'MkSocket' constructor directly.
However, for compatibility with older versions on Windows, it is good practice
to always call 'withSocketsDo' (it's very cheap).
-}
{-# INLINE withSocketsDo #-}
withSocketsDo :: IO a -> IO a

{-# LINE 259 "Network/Socket/Internal.hsc" #-}
withSocketsDo x = x

{-# LINE 275 "Network/Socket/Internal.hsc" #-}
