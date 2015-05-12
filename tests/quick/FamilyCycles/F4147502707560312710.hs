{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable, BangPatterns,
  MultiParamTypeClasses, TypeFamilies, FlexibleContexts,
  StandaloneDeriving, NondecreasingIndentation, ExplicitForAll,
  PatternGuards #-}
module F4147502707560312710 where
import GHC.Types (Bool(True))
import GHC.Types (Bool(False))
import GHC.Types (Bool)
import GHC.Word (Word8)

fromBool :: Bool -> Word8
fromBool True = 1
fromBool False = 0