{-# LANGUAGE NoImplicitPrelude, UnboxedTuples, MagicHash,
  DeriveDataTypeable, MultiParamTypeClasses,
  NondecreasingIndentation, ExplicitForAll, PatternGuards #-}
module F903085878682639365 where
import GHC.Prim (Int#)
import GHC.Prim (ByteArray#)
import GHC.Prim (MutableByteArray#)
import GHC.Prim (State#)
import GHC.Prim (Addr#)

import {-# SOURCE #-} F3790969093102345469 ()

import {-# SOURCE #-} F8979369651481690361 ()

class Prim a where
        sizeOf# :: a -> Int#
        
        alignment# :: a -> Int#
        