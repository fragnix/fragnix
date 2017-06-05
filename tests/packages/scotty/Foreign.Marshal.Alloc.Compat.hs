{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Foreign/Marshal/Alloc/Compat.hs" #-}

















































{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.Marshal.Alloc.Compat (
  module Base
, calloc
, callocBytes
) where
import Foreign.Marshal.Alloc as Base

