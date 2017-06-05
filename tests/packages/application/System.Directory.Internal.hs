{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "System/Directory/Internal.hs" #-}























































{-# LANGUAGE CPP #-}
-- |
-- Stability: unstable
-- Portability: unportable
--
-- Internal modules are always subject to change from version to version.
-- The contents of this module are also platform-dependent, hence what is
-- shown in the Hackage documentation may differ from what is actually
-- available on your system.




















































module System.Directory.Internal
  ( module System.Directory.Internal.Common

  , module System.Directory.Internal.Posix

  , module System.Directory.Internal.C_utimensat

  ) where

import System.Directory.Internal.Common

import System.Directory.Internal.Posix

import System.Directory.Internal.C_utimensat
