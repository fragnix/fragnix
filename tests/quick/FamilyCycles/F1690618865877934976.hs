{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable, BangPatterns,
  MultiParamTypeClasses, TypeFamilies, FlexibleContexts,
  StandaloneDeriving, NondecreasingIndentation, ExplicitForAll,
  PatternGuards #-}
module F1690618865877934976
       (MVector(..), Bool(..), MVector(..), Word8(..)) where
import F5053685185079289079 (MVector)
import GHC.Types (Bool)
import qualified F8599798322733369681 as P (MVector)
import GHC.Word (Word8)

newtype instance MVector s Bool = MV_Bool (P.MVector s Word8)