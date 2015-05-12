{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples,
  UnliftedFFITypes, DeriveDataTypeable, MultiParamTypeClasses,
  NondecreasingIndentation, ExplicitForAll, PatternGuards #-}
module F9115654309439442691 where
import GHC.Prim (MutableByteArray#)
import Data.Typeable.Internal (Typeable)

data MutableByteArray s = MutableByteArray (MutableByteArray# s)