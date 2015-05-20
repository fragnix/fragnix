{-# LANGUAGE NoImplicitPrelude, UnboxedTuples, MagicHash,
  DeriveDataTypeable, MultiParamTypeClasses,
  NondecreasingIndentation, ExplicitForAll, PatternGuards #-}
module F903085878682639365 where
import GHC.Prim (Int#)
import GHC.Prim (ByteArray#)
import GHC.Prim (MutableByteArray#)
import GHC.Prim (State#)
import GHC.Prim (Addr#)

class Prim a where
        sizeOf# :: a -> Int#
        
        alignment# :: a -> Int#
        