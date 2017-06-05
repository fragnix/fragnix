{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Foreign/Marshal/Compat.hs" #-}

















































{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Foreign.Marshal.Compat (
  module Base
, module Alloc
, module Array
, module Utils
) where
import Foreign.Marshal as Base

import Foreign.Marshal.Alloc.Compat as Alloc
import Foreign.Marshal.Array.Compat as Array
import Foreign.Marshal.Utils.Compat as Utils
