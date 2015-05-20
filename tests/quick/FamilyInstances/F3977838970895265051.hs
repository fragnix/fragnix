{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable, BangPatterns,
  MultiParamTypeClasses, TypeFamilies, FlexibleContexts,
  StandaloneDeriving, NondecreasingIndentation, ExplicitForAll,
  PatternGuards #-}
module F3977838970895265051 where
import qualified F9165819906499745633 as M (MVector)
import F8599798322733369681 (MVector(MVector))
import GHC.Types (Bool)
import GHC.Types (Int)
import Data.Word (Word8)
import qualified F9165819906499745633 as M (basicLength)
import Prelude (String)
import GHC.Show (show)

f :: Int
f = M.basicLength (MVector 5 5 5 :: MVector () Word8)
