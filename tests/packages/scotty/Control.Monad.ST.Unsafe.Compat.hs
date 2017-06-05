{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Control/Monad/ST/Unsafe/Compat.hs" #-}

















































{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Control.Monad.ST.Unsafe.Compat (
  -- * Unsafe operations
  unsafeInterleaveST
, unsafeIOToST
, unsafeSTToIO
) where

import Control.Monad.ST.Unsafe (unsafeInterleaveST, unsafeIOToST, unsafeSTToIO)
