{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Foreign/Compat.hs" #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Compat (
  module Base
, module Marshal
) where
import Foreign as Base

import Foreign.Marshal.Compat as Marshal
