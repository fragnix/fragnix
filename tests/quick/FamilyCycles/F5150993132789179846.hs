{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples,
  UnliftedFFITypes, DeriveDataTypeable, MultiParamTypeClasses,
  NondecreasingIndentation, ExplicitForAll, PatternGuards #-}
module F5150993132789179846 where
import F9115654309439442691 (MutableByteArray(MutableByteArray))
import GHC.Types (isTrue#)
import GHC.Prim (sameMutableByteArray#)
import F9115654309439442691 (MutableByteArray)
import GHC.Types (Bool)

sameMutableByteArray ::
                     MutableByteArray s -> MutableByteArray s -> Bool
sameMutableByteArray (MutableByteArray arr#)
  (MutableByteArray brr#) = isTrue# (sameMutableByteArray# arr# brr#)