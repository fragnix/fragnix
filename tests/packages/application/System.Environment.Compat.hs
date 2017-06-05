{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/System/Environment/Compat.hs" #-}

















































{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- | Miscellaneous information about the system environment.
module System.Environment.Compat (
  getArgs
, getProgName
, getEnv
, lookupEnv
, setEnv
, unsetEnv
, withArgs
, withProgName
, getEnvironment
) where

import           System.Environment

