{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Foreign/ForeignPtr/Compat.hs" #-}

















































{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
module Foreign.ForeignPtr.Compat (
  module Base
, plusForeignPtr
) where

import Foreign.ForeignPtr as Base

import GHC.Exts (Int(..), plusAddr#)
import GHC.ForeignPtr (ForeignPtr(..))

plusForeignPtr :: ForeignPtr a -> Int -> ForeignPtr b
-- ^Advances the given address by the given offset in bytes.
--
-- The new 'ForeignPtr' shares the finalizer of the original,
-- equivalent from a finalization standpoint to just creating another
-- reference to the original. That is, the finalizer will not be
-- called before the new 'ForeignPtr' is unreachable, nor will it be
-- called an additional time due to this call, and the finalizer will
-- be called with the same address that it would have had this call
-- not happened, *not* the new address.
--
-- /Since: 4.10.0.0/
plusForeignPtr (ForeignPtr addr c) (I# d) = ForeignPtr (plusAddr# addr d) c
