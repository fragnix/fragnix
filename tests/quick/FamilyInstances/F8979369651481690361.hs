{-# LANGUAGE NoImplicitPrelude, UnboxedTuples, MagicHash,
  DeriveDataTypeable, MultiParamTypeClasses,
  NondecreasingIndentation, ExplicitForAll, PatternGuards #-}
module F8979369651481690361 where
import F903085878682639365 (Prim(..))
import GHC.Word (Word8)
import F903085878682639365 (sizeOf#)
import F903085878682639365 (alignment#)
import F903085878682639365 (indexByteArray#)
import GHC.Word (Word8(W8#))
import Prelude (undefined)

instance Prim Word8 where
        sizeOf# _ = undefined
        alignment# _ = undefined
        indexByteArray# arr# i# = undefined
        readByteArray# arr# i# s#
          = undefined
        writeByteArray# arr# i# (W8# x#) s#
          = undefined
        setByteArray# arr# i# n# (W8# x#) s#
          = undefined
        indexOffAddr# addr# i# = undefined
        readOffAddr# addr# i# s#
          = undefined
        writeOffAddr# addr# i# (W8# x#) s#
          = undefined
        setOffAddr# addr# i# n# (W8# x#) s#
          = undefined
        
        {-# INLINE sizeOf# #-}
        
        {-# INLINE alignment# #-}
        
        {-# INLINE indexByteArray# #-}
        
        {-# INLINE readByteArray# #-}
        
        {-# INLINE writeByteArray# #-}
        
        {-# INLINE setByteArray# #-}
        
        {-# INLINE indexOffAddr# #-}
        
        {-# INLINE readOffAddr# #-}
        
        {-# INLINE writeOffAddr# #-}
        
        {-# INLINE setOffAddr# #-}