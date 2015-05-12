{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable, BangPatterns,
  MultiParamTypeClasses, TypeFamilies, FlexibleContexts,
  StandaloneDeriving, NondecreasingIndentation, ExplicitForAll,
  PatternGuards #-}
module MVectorFamilyInstance
       (MVector(..), Bool(..), MVector(..), Word8(..)) where
import MVectorFamily (MVector)
import GHC.Types (Bool)
import qualified MVectorData as P (MVector)
import GHC.Word (Word8)

newtype instance MVector s Bool = MV_Bool (P.MVector s Word8)