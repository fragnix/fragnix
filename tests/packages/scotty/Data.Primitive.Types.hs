{-# LINE 1 "Data/Primitive/Types.hs" #-}
# 1 "Data/Primitive/Types.hs"
# 1 "<command-line>"
# 8 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 8 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1






















































































# 8 "<command-line>" 2
# 1 "Data/Primitive/Types.hs"
{-# LANGUAGE CPP, UnboxedTuples, MagicHash, DeriveDataTypeable #-}

-- |
-- Module      : Data.Primitive.Types
-- Copyright   : (c) Roman Leshchinskiy 2009-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Basic types and classes for primitive array operations
--

module Data.Primitive.Types (
  Prim(..),

  Addr(..),
) where

import Control.Monad.Primitive
import Data.Primitive.MachDeps
import Data.Primitive.Internal.Operations

import GHC.Base (
    Int(..), Char(..),
  )
import GHC.Float (
    Float(..), Double(..)
  )
import GHC.Word (
    Word(..), Word8(..), Word16(..), Word32(..), Word64(..)
  )
import GHC.Int (
    Int8(..), Int16(..), Int32(..), Int64(..)
  )

import GHC.Prim

    hiding (setByteArray#)


import Data.Typeable ( Typeable )
import Data.Data ( Data(..) )
import Data.Primitive.Internal.Compat ( isTrue#, mkNoRepType )

-- | A machine address
data Addr = Addr Addr# deriving ( Typeable )

instance Eq Addr where
  Addr a# == Addr b# = isTrue# (eqAddr# a# b#)
  Addr a# /= Addr b# = isTrue# (neAddr# a# b#)

instance Ord Addr where
  Addr a# > Addr b# = isTrue# (gtAddr# a# b#)
  Addr a# >= Addr b# = isTrue# (geAddr# a# b#)
  Addr a# < Addr b# = isTrue# (ltAddr# a# b#)
  Addr a# <= Addr b# = isTrue# (leAddr# a# b#)

instance Data Addr where
  toConstr _ = error "toConstr"
  gunfold _ _ = error "gunfold"
  dataTypeOf _ = mkNoRepType "Data.Primitive.Types.Addr"


-- | Class of types supporting primitive array operations
class Prim a where

  -- | Size of values of type @a@. The argument is not used.
  sizeOf#    :: a -> Int#

  -- | Alignment of values of type @a@. The argument is not used.
  alignment# :: a -> Int#

  -- | Read a value from the array. The offset is in elements of type
  -- @a@ rather than in bytes.
  indexByteArray# :: ByteArray# -> Int# -> a

  -- | Read a value from the mutable array. The offset is in elements of type
  -- @a@ rather than in bytes.
  readByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)

  -- | Write a value to the mutable array. The offset is in elements of type
  -- @a@ rather than in bytes.
  writeByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s

  -- | Fill a slice of the mutable array with a value. The offset and length
  -- of the chunk are in elements of type @a@ rather than in bytes.
  setByteArray# :: MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s

  -- | Read a value from a memory position given by an address and an offset.
  -- The memory block the address refers to must be immutable. The offset is in
  -- elements of type @a@ rather than in bytes.
  indexOffAddr# :: Addr# -> Int# -> a

  -- | Read a value from a memory position given by an address and an offset.
  -- The offset is in elements of type @a@ rather than in bytes.
  readOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, a #)

  -- | Write a value to a memory position given by an address and an offset.
  -- The offset is in elements of type @a@ rather than in bytes.
  writeOffAddr# :: Addr# -> Int# -> a -> State# s -> State# s

  -- | Fill a memory block given by an address, an offset and a length.
  -- The offset and length are in elements of type @a@ rather than in bytes.
  setOffAddr# :: Addr# -> Int# -> Int# -> a -> State# s -> State# s

# 137 "Data/Primitive/Types.hs"

unI# :: Int -> Int#
unI# (I# n#) = n#

instance Prim Word where {                                          sizeOf# _ = unI#  sIZEOF_WORD                                           ; alignment# _ = unI#  aLIGNMENT_WORD                                     ; indexByteArray# arr# i# =  W# (            indexWordArray# arr# i#)               ; readByteArray#  arr# i# s# = case  readWordArray# arr# i# s# of                                { (# s1#, x# #) -> (# s1#,  W# x# #) }  ; writeByteArray# arr# i# ( W# x#) s# =  writeWordArray# arr# i# x# s#    ; setByteArray# arr# i# n# ( W# x#) s#                              = case unsafeCoerce# (internal ( setWordArray# arr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                                                                                 ; indexOffAddr# addr# i# =  W# (            indexWordOffAddr# addr# i#)              ; readOffAddr#  addr# i# s# = case  readWordOffAddr# addr# i# s# of                               { (# s1#, x# #) -> (# s1#,  W# x# #) }  ; writeOffAddr# addr# i# ( W# x#) s# =  writeWordOffAddr# addr# i# x# s#   ; setOffAddr# addr# i# n# ( W# x#) s#                               = case unsafeCoerce# (internal ( setWordOffAddr# addr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                 ; {-# INLINE sizeOf# #-}                                        ; {-# INLINE alignment# #-}                                     ; {-# INLINE indexByteArray# #-}                                ; {-# INLINE readByteArray# #-}                                 ; {-# INLINE writeByteArray# #-}                                ; {-# INLINE setByteArray# #-}                                  ; {-# INLINE indexOffAddr# #-}                                  ; {-# INLINE readOffAddr# #-}                                   ; {-# INLINE writeOffAddr# #-}                                  ; {-# INLINE setOffAddr# #-}                                    }


instance Prim Word8 where {                                          sizeOf# _ = unI#  sIZEOF_WORD8                                           ; alignment# _ = unI#  aLIGNMENT_WORD8                                     ; indexByteArray# arr# i# =  W8# (            indexWord8Array# arr# i#)               ; readByteArray#  arr# i# s# = case  readWord8Array# arr# i# s# of                                { (# s1#, x# #) -> (# s1#,  W8# x# #) }  ; writeByteArray# arr# i# ( W8# x#) s# =  writeWord8Array# arr# i# x# s#    ; setByteArray# arr# i# n# ( W8# x#) s#                              = case unsafeCoerce# (internal ( setWord8Array# arr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                                                                                 ; indexOffAddr# addr# i# =  W8# (            indexWord8OffAddr# addr# i#)              ; readOffAddr#  addr# i# s# = case  readWord8OffAddr# addr# i# s# of                               { (# s1#, x# #) -> (# s1#,  W8# x# #) }  ; writeOffAddr# addr# i# ( W8# x#) s# =  writeWord8OffAddr# addr# i# x# s#   ; setOffAddr# addr# i# n# ( W8# x#) s#                               = case unsafeCoerce# (internal ( setWord8OffAddr# addr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                 ; {-# INLINE sizeOf# #-}                                        ; {-# INLINE alignment# #-}                                     ; {-# INLINE indexByteArray# #-}                                ; {-# INLINE readByteArray# #-}                                 ; {-# INLINE writeByteArray# #-}                                ; {-# INLINE setByteArray# #-}                                  ; {-# INLINE indexOffAddr# #-}                                  ; {-# INLINE readOffAddr# #-}                                   ; {-# INLINE writeOffAddr# #-}                                  ; {-# INLINE setOffAddr# #-}                                    }


instance Prim Word16 where {                                          sizeOf# _ = unI#  sIZEOF_WORD16                                           ; alignment# _ = unI#  aLIGNMENT_WORD16                                     ; indexByteArray# arr# i# =  W16# (            indexWord16Array# arr# i#)               ; readByteArray#  arr# i# s# = case  readWord16Array# arr# i# s# of                                { (# s1#, x# #) -> (# s1#,  W16# x# #) }  ; writeByteArray# arr# i# ( W16# x#) s# =  writeWord16Array# arr# i# x# s#    ; setByteArray# arr# i# n# ( W16# x#) s#                              = case unsafeCoerce# (internal ( setWord16Array# arr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                                                                                 ; indexOffAddr# addr# i# =  W16# (            indexWord16OffAddr# addr# i#)              ; readOffAddr#  addr# i# s# = case  readWord16OffAddr# addr# i# s# of                               { (# s1#, x# #) -> (# s1#,  W16# x# #) }  ; writeOffAddr# addr# i# ( W16# x#) s# =  writeWord16OffAddr# addr# i# x# s#   ; setOffAddr# addr# i# n# ( W16# x#) s#                               = case unsafeCoerce# (internal ( setWord16OffAddr# addr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                 ; {-# INLINE sizeOf# #-}                                        ; {-# INLINE alignment# #-}                                     ; {-# INLINE indexByteArray# #-}                                ; {-# INLINE readByteArray# #-}                                 ; {-# INLINE writeByteArray# #-}                                ; {-# INLINE setByteArray# #-}                                  ; {-# INLINE indexOffAddr# #-}                                  ; {-# INLINE readOffAddr# #-}                                   ; {-# INLINE writeOffAddr# #-}                                  ; {-# INLINE setOffAddr# #-}                                    }


instance Prim Word32 where {                                          sizeOf# _ = unI#  sIZEOF_WORD32                                           ; alignment# _ = unI#  aLIGNMENT_WORD32                                     ; indexByteArray# arr# i# =  W32# (            indexWord32Array# arr# i#)               ; readByteArray#  arr# i# s# = case  readWord32Array# arr# i# s# of                                { (# s1#, x# #) -> (# s1#,  W32# x# #) }  ; writeByteArray# arr# i# ( W32# x#) s# =  writeWord32Array# arr# i# x# s#    ; setByteArray# arr# i# n# ( W32# x#) s#                              = case unsafeCoerce# (internal ( setWord32Array# arr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                                                                                 ; indexOffAddr# addr# i# =  W32# (            indexWord32OffAddr# addr# i#)              ; readOffAddr#  addr# i# s# = case  readWord32OffAddr# addr# i# s# of                               { (# s1#, x# #) -> (# s1#,  W32# x# #) }  ; writeOffAddr# addr# i# ( W32# x#) s# =  writeWord32OffAddr# addr# i# x# s#   ; setOffAddr# addr# i# n# ( W32# x#) s#                               = case unsafeCoerce# (internal ( setWord32OffAddr# addr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                 ; {-# INLINE sizeOf# #-}                                        ; {-# INLINE alignment# #-}                                     ; {-# INLINE indexByteArray# #-}                                ; {-# INLINE readByteArray# #-}                                 ; {-# INLINE writeByteArray# #-}                                ; {-# INLINE setByteArray# #-}                                  ; {-# INLINE indexOffAddr# #-}                                  ; {-# INLINE readOffAddr# #-}                                   ; {-# INLINE writeOffAddr# #-}                                  ; {-# INLINE setOffAddr# #-}                                    }


instance Prim Word64 where {                                          sizeOf# _ = unI#  sIZEOF_WORD64                                           ; alignment# _ = unI#  aLIGNMENT_WORD64                                     ; indexByteArray# arr# i# =  W64# (            indexWord64Array# arr# i#)               ; readByteArray#  arr# i# s# = case  readWord64Array# arr# i# s# of                                { (# s1#, x# #) -> (# s1#,  W64# x# #) }  ; writeByteArray# arr# i# ( W64# x#) s# =  writeWord64Array# arr# i# x# s#    ; setByteArray# arr# i# n# ( W64# x#) s#                              = case unsafeCoerce# (internal ( setWord64Array# arr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                                                                                 ; indexOffAddr# addr# i# =  W64# (            indexWord64OffAddr# addr# i#)              ; readOffAddr#  addr# i# s# = case  readWord64OffAddr# addr# i# s# of                               { (# s1#, x# #) -> (# s1#,  W64# x# #) }  ; writeOffAddr# addr# i# ( W64# x#) s# =  writeWord64OffAddr# addr# i# x# s#   ; setOffAddr# addr# i# n# ( W64# x#) s#                               = case unsafeCoerce# (internal ( setWord64OffAddr# addr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                 ; {-# INLINE sizeOf# #-}                                        ; {-# INLINE alignment# #-}                                     ; {-# INLINE indexByteArray# #-}                                ; {-# INLINE readByteArray# #-}                                 ; {-# INLINE writeByteArray# #-}                                ; {-# INLINE setByteArray# #-}                                  ; {-# INLINE indexOffAddr# #-}                                  ; {-# INLINE readOffAddr# #-}                                   ; {-# INLINE writeOffAddr# #-}                                  ; {-# INLINE setOffAddr# #-}                                    }


instance Prim Int where {                                          sizeOf# _ = unI#  sIZEOF_INT                                           ; alignment# _ = unI#  aLIGNMENT_INT                                     ; indexByteArray# arr# i# =  I# (            indexIntArray# arr# i#)               ; readByteArray#  arr# i# s# = case  readIntArray# arr# i# s# of                                { (# s1#, x# #) -> (# s1#,  I# x# #) }  ; writeByteArray# arr# i# ( I# x#) s# =  writeIntArray# arr# i# x# s#    ; setByteArray# arr# i# n# ( I# x#) s#                              = case unsafeCoerce# (internal ( setIntArray# arr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                                                                                 ; indexOffAddr# addr# i# =  I# (            indexIntOffAddr# addr# i#)              ; readOffAddr#  addr# i# s# = case  readIntOffAddr# addr# i# s# of                               { (# s1#, x# #) -> (# s1#,  I# x# #) }  ; writeOffAddr# addr# i# ( I# x#) s# =  writeIntOffAddr# addr# i# x# s#   ; setOffAddr# addr# i# n# ( I# x#) s#                               = case unsafeCoerce# (internal ( setIntOffAddr# addr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                 ; {-# INLINE sizeOf# #-}                                        ; {-# INLINE alignment# #-}                                     ; {-# INLINE indexByteArray# #-}                                ; {-# INLINE readByteArray# #-}                                 ; {-# INLINE writeByteArray# #-}                                ; {-# INLINE setByteArray# #-}                                  ; {-# INLINE indexOffAddr# #-}                                  ; {-# INLINE readOffAddr# #-}                                   ; {-# INLINE writeOffAddr# #-}                                  ; {-# INLINE setOffAddr# #-}                                    }


instance Prim Int8 where {                                          sizeOf# _ = unI#  sIZEOF_INT8                                           ; alignment# _ = unI#  aLIGNMENT_INT8                                     ; indexByteArray# arr# i# =  I8# (            indexInt8Array# arr# i#)               ; readByteArray#  arr# i# s# = case  readInt8Array# arr# i# s# of                                { (# s1#, x# #) -> (# s1#,  I8# x# #) }  ; writeByteArray# arr# i# ( I8# x#) s# =  writeInt8Array# arr# i# x# s#    ; setByteArray# arr# i# n# ( I8# x#) s#                              = case unsafeCoerce# (internal ( setInt8Array# arr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                                                                                 ; indexOffAddr# addr# i# =  I8# (            indexInt8OffAddr# addr# i#)              ; readOffAddr#  addr# i# s# = case  readInt8OffAddr# addr# i# s# of                               { (# s1#, x# #) -> (# s1#,  I8# x# #) }  ; writeOffAddr# addr# i# ( I8# x#) s# =  writeInt8OffAddr# addr# i# x# s#   ; setOffAddr# addr# i# n# ( I8# x#) s#                               = case unsafeCoerce# (internal ( setInt8OffAddr# addr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                 ; {-# INLINE sizeOf# #-}                                        ; {-# INLINE alignment# #-}                                     ; {-# INLINE indexByteArray# #-}                                ; {-# INLINE readByteArray# #-}                                 ; {-# INLINE writeByteArray# #-}                                ; {-# INLINE setByteArray# #-}                                  ; {-# INLINE indexOffAddr# #-}                                  ; {-# INLINE readOffAddr# #-}                                   ; {-# INLINE writeOffAddr# #-}                                  ; {-# INLINE setOffAddr# #-}                                    }


instance Prim Int16 where {                                          sizeOf# _ = unI#  sIZEOF_INT16                                           ; alignment# _ = unI#  aLIGNMENT_INT16                                     ; indexByteArray# arr# i# =  I16# (            indexInt16Array# arr# i#)               ; readByteArray#  arr# i# s# = case  readInt16Array# arr# i# s# of                                { (# s1#, x# #) -> (# s1#,  I16# x# #) }  ; writeByteArray# arr# i# ( I16# x#) s# =  writeInt16Array# arr# i# x# s#    ; setByteArray# arr# i# n# ( I16# x#) s#                              = case unsafeCoerce# (internal ( setInt16Array# arr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                                                                                 ; indexOffAddr# addr# i# =  I16# (            indexInt16OffAddr# addr# i#)              ; readOffAddr#  addr# i# s# = case  readInt16OffAddr# addr# i# s# of                               { (# s1#, x# #) -> (# s1#,  I16# x# #) }  ; writeOffAddr# addr# i# ( I16# x#) s# =  writeInt16OffAddr# addr# i# x# s#   ; setOffAddr# addr# i# n# ( I16# x#) s#                               = case unsafeCoerce# (internal ( setInt16OffAddr# addr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                 ; {-# INLINE sizeOf# #-}                                        ; {-# INLINE alignment# #-}                                     ; {-# INLINE indexByteArray# #-}                                ; {-# INLINE readByteArray# #-}                                 ; {-# INLINE writeByteArray# #-}                                ; {-# INLINE setByteArray# #-}                                  ; {-# INLINE indexOffAddr# #-}                                  ; {-# INLINE readOffAddr# #-}                                   ; {-# INLINE writeOffAddr# #-}                                  ; {-# INLINE setOffAddr# #-}                                    }


instance Prim Int32 where {                                          sizeOf# _ = unI#  sIZEOF_INT32                                           ; alignment# _ = unI#  aLIGNMENT_INT32                                     ; indexByteArray# arr# i# =  I32# (            indexInt32Array# arr# i#)               ; readByteArray#  arr# i# s# = case  readInt32Array# arr# i# s# of                                { (# s1#, x# #) -> (# s1#,  I32# x# #) }  ; writeByteArray# arr# i# ( I32# x#) s# =  writeInt32Array# arr# i# x# s#    ; setByteArray# arr# i# n# ( I32# x#) s#                              = case unsafeCoerce# (internal ( setInt32Array# arr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                                                                                 ; indexOffAddr# addr# i# =  I32# (            indexInt32OffAddr# addr# i#)              ; readOffAddr#  addr# i# s# = case  readInt32OffAddr# addr# i# s# of                               { (# s1#, x# #) -> (# s1#,  I32# x# #) }  ; writeOffAddr# addr# i# ( I32# x#) s# =  writeInt32OffAddr# addr# i# x# s#   ; setOffAddr# addr# i# n# ( I32# x#) s#                               = case unsafeCoerce# (internal ( setInt32OffAddr# addr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                 ; {-# INLINE sizeOf# #-}                                        ; {-# INLINE alignment# #-}                                     ; {-# INLINE indexByteArray# #-}                                ; {-# INLINE readByteArray# #-}                                 ; {-# INLINE writeByteArray# #-}                                ; {-# INLINE setByteArray# #-}                                  ; {-# INLINE indexOffAddr# #-}                                  ; {-# INLINE readOffAddr# #-}                                   ; {-# INLINE writeOffAddr# #-}                                  ; {-# INLINE setOffAddr# #-}                                    }


instance Prim Int64 where {                                          sizeOf# _ = unI#  sIZEOF_INT64                                           ; alignment# _ = unI#  aLIGNMENT_INT64                                     ; indexByteArray# arr# i# =  I64# (            indexInt64Array# arr# i#)               ; readByteArray#  arr# i# s# = case  readInt64Array# arr# i# s# of                                { (# s1#, x# #) -> (# s1#,  I64# x# #) }  ; writeByteArray# arr# i# ( I64# x#) s# =  writeInt64Array# arr# i# x# s#    ; setByteArray# arr# i# n# ( I64# x#) s#                              = case unsafeCoerce# (internal ( setInt64Array# arr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                                                                                 ; indexOffAddr# addr# i# =  I64# (            indexInt64OffAddr# addr# i#)              ; readOffAddr#  addr# i# s# = case  readInt64OffAddr# addr# i# s# of                               { (# s1#, x# #) -> (# s1#,  I64# x# #) }  ; writeOffAddr# addr# i# ( I64# x#) s# =  writeInt64OffAddr# addr# i# x# s#   ; setOffAddr# addr# i# n# ( I64# x#) s#                               = case unsafeCoerce# (internal ( setInt64OffAddr# addr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                 ; {-# INLINE sizeOf# #-}                                        ; {-# INLINE alignment# #-}                                     ; {-# INLINE indexByteArray# #-}                                ; {-# INLINE readByteArray# #-}                                 ; {-# INLINE writeByteArray# #-}                                ; {-# INLINE setByteArray# #-}                                  ; {-# INLINE indexOffAddr# #-}                                  ; {-# INLINE readOffAddr# #-}                                   ; {-# INLINE writeOffAddr# #-}                                  ; {-# INLINE setOffAddr# #-}                                    }


instance Prim Float where {                                          sizeOf# _ = unI#  sIZEOF_FLOAT                                           ; alignment# _ = unI#  aLIGNMENT_FLOAT                                     ; indexByteArray# arr# i# =  F# (            indexFloatArray# arr# i#)               ; readByteArray#  arr# i# s# = case  readFloatArray# arr# i# s# of                                { (# s1#, x# #) -> (# s1#,  F# x# #) }  ; writeByteArray# arr# i# ( F# x#) s# =  writeFloatArray# arr# i# x# s#    ; setByteArray# arr# i# n# ( F# x#) s#                              = case unsafeCoerce# (internal ( setFloatArray# arr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                                                                                 ; indexOffAddr# addr# i# =  F# (            indexFloatOffAddr# addr# i#)              ; readOffAddr#  addr# i# s# = case  readFloatOffAddr# addr# i# s# of                               { (# s1#, x# #) -> (# s1#,  F# x# #) }  ; writeOffAddr# addr# i# ( F# x#) s# =  writeFloatOffAddr# addr# i# x# s#   ; setOffAddr# addr# i# n# ( F# x#) s#                               = case unsafeCoerce# (internal ( setFloatOffAddr# addr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                 ; {-# INLINE sizeOf# #-}                                        ; {-# INLINE alignment# #-}                                     ; {-# INLINE indexByteArray# #-}                                ; {-# INLINE readByteArray# #-}                                 ; {-# INLINE writeByteArray# #-}                                ; {-# INLINE setByteArray# #-}                                  ; {-# INLINE indexOffAddr# #-}                                  ; {-# INLINE readOffAddr# #-}                                   ; {-# INLINE writeOffAddr# #-}                                  ; {-# INLINE setOffAddr# #-}                                    }


instance Prim Double where {                                          sizeOf# _ = unI#  sIZEOF_DOUBLE                                           ; alignment# _ = unI#  aLIGNMENT_DOUBLE                                     ; indexByteArray# arr# i# =  D# (            indexDoubleArray# arr# i#)               ; readByteArray#  arr# i# s# = case  readDoubleArray# arr# i# s# of                                { (# s1#, x# #) -> (# s1#,  D# x# #) }  ; writeByteArray# arr# i# ( D# x#) s# =  writeDoubleArray# arr# i# x# s#    ; setByteArray# arr# i# n# ( D# x#) s#                              = case unsafeCoerce# (internal ( setDoubleArray# arr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                                                                                 ; indexOffAddr# addr# i# =  D# (            indexDoubleOffAddr# addr# i#)              ; readOffAddr#  addr# i# s# = case  readDoubleOffAddr# addr# i# s# of                               { (# s1#, x# #) -> (# s1#,  D# x# #) }  ; writeOffAddr# addr# i# ( D# x#) s# =  writeDoubleOffAddr# addr# i# x# s#   ; setOffAddr# addr# i# n# ( D# x#) s#                               = case unsafeCoerce# (internal ( setDoubleOffAddr# addr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                 ; {-# INLINE sizeOf# #-}                                        ; {-# INLINE alignment# #-}                                     ; {-# INLINE indexByteArray# #-}                                ; {-# INLINE readByteArray# #-}                                 ; {-# INLINE writeByteArray# #-}                                ; {-# INLINE setByteArray# #-}                                  ; {-# INLINE indexOffAddr# #-}                                  ; {-# INLINE readOffAddr# #-}                                   ; {-# INLINE writeOffAddr# #-}                                  ; {-# INLINE setOffAddr# #-}                                    }


instance Prim Char where {                                          sizeOf# _ = unI#  sIZEOF_CHAR                                           ; alignment# _ = unI#  aLIGNMENT_CHAR                                     ; indexByteArray# arr# i# =  C# (            indexWideCharArray# arr# i#)               ; readByteArray#  arr# i# s# = case  readWideCharArray# arr# i# s# of                                { (# s1#, x# #) -> (# s1#,  C# x# #) }  ; writeByteArray# arr# i# ( C# x#) s# =  writeWideCharArray# arr# i# x# s#    ; setByteArray# arr# i# n# ( C# x#) s#                              = case unsafeCoerce# (internal ( setWideCharArray# arr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                                                                                 ; indexOffAddr# addr# i# =  C# (            indexWideCharOffAddr# addr# i#)              ; readOffAddr#  addr# i# s# = case  readWideCharOffAddr# addr# i# s# of                               { (# s1#, x# #) -> (# s1#,  C# x# #) }  ; writeOffAddr# addr# i# ( C# x#) s# =  writeWideCharOffAddr# addr# i# x# s#   ; setOffAddr# addr# i# n# ( C# x#) s#                               = case unsafeCoerce# (internal ( setWideCharOffAddr# addr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                 ; {-# INLINE sizeOf# #-}                                        ; {-# INLINE alignment# #-}                                     ; {-# INLINE indexByteArray# #-}                                ; {-# INLINE readByteArray# #-}                                 ; {-# INLINE writeByteArray# #-}                                ; {-# INLINE setByteArray# #-}                                  ; {-# INLINE indexOffAddr# #-}                                  ; {-# INLINE readOffAddr# #-}                                   ; {-# INLINE writeOffAddr# #-}                                  ; {-# INLINE setOffAddr# #-}                                    }


instance Prim Addr where {                                          sizeOf# _ = unI#  sIZEOF_PTR                                           ; alignment# _ = unI#  aLIGNMENT_PTR                                     ; indexByteArray# arr# i# =  Addr (            indexAddrArray# arr# i#)               ; readByteArray#  arr# i# s# = case  readAddrArray# arr# i# s# of                                { (# s1#, x# #) -> (# s1#,  Addr x# #) }  ; writeByteArray# arr# i# ( Addr x#) s# =  writeAddrArray# arr# i# x# s#    ; setByteArray# arr# i# n# ( Addr x#) s#                              = case unsafeCoerce# (internal ( setAddrArray# arr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                                                                                 ; indexOffAddr# addr# i# =  Addr (            indexAddrOffAddr# addr# i#)              ; readOffAddr#  addr# i# s# = case  readAddrOffAddr# addr# i# s# of                               { (# s1#, x# #) -> (# s1#,  Addr x# #) }  ; writeOffAddr# addr# i# ( Addr x#) s# =  writeAddrOffAddr# addr# i# x# s#   ; setOffAddr# addr# i# n# ( Addr x#) s#                               = case unsafeCoerce# (internal ( setAddrOffAddr# addr# i# n# x#)) s# of         { (# s1#, _ #) -> s1# }                                 ; {-# INLINE sizeOf# #-}                                        ; {-# INLINE alignment# #-}                                     ; {-# INLINE indexByteArray# #-}                                ; {-# INLINE readByteArray# #-}                                 ; {-# INLINE writeByteArray# #-}                                ; {-# INLINE setByteArray# #-}                                  ; {-# INLINE indexOffAddr# #-}                                  ; {-# INLINE readOffAddr# #-}                                   ; {-# INLINE writeOffAddr# #-}                                  ; {-# INLINE setOffAddr# #-}                                    }
