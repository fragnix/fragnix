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
        
        indexByteArray# :: ByteArray# -> Int# -> a
        
        readByteArray# ::
                       MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
        
        writeByteArray# ::
                        MutableByteArray# s -> Int# -> a -> State# s -> State# s
        
        setByteArray# ::
                      MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
        
        indexOffAddr# :: Addr# -> Int# -> a
        
        readOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, a #)
        
        writeOffAddr# :: Addr# -> Int# -> a -> State# s -> State# s
        
        setOffAddr# :: Addr# -> Int# -> Int# -> a -> State# s -> State# s