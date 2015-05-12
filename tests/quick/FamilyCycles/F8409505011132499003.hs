{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable, BangPatterns,
  MultiParamTypeClasses, TypeFamilies, FlexibleContexts,
  StandaloneDeriving, NondecreasingIndentation, ExplicitForAll,
  PatternGuards #-}
module F8409505011132499003 where
import GHC.Types (Bool(False))
import GHC.Types (Bool(True))
import GHC.Word (Word8)
import GHC.Types (Bool)

toBool :: Word8 -> Bool
toBool 0 = False
toBool _ = True