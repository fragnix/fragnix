{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Control/Monad/ST/Lazy/Unsafe/Compat.hs" #-}

















































{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Control.Monad.ST.Lazy.Unsafe.Compat (
  -- * Unsafe operations
  unsafeInterleaveST
, unsafeIOToST
) where

import Control.Monad.ST.Lazy.Unsafe (unsafeInterleaveST, unsafeIOToST)
