{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Data/IORef/Compat.hs" #-}

















































{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.IORef.Compat (
  module Base
, modifyIORef'
, atomicModifyIORef'
, atomicWriteIORef
) where

import Data.IORef as Base

