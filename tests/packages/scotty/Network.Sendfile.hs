{-# LINE 1 "Network/Sendfile.hs" #-}















































{-# LANGUAGE CPP #-}

{-|
  Cross platform library for the sendfile system call.
  This library tries to call minimum system calls which
  are the bottleneck of web servers.
-}

module Network.Sendfile (
    sendfile
  , sendfileWithHeader
  , sendfileFd
  , sendfileFdWithHeader
  , FileRange(..)
  ) where

import Network.Sendfile.Types

import Network.Sendfile.Linux
