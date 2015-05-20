{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable,
  MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables,
  NondecreasingIndentation, ExplicitForAll, PatternGuards #-}
module F6650074180974082130 where
import F903085878682639365 (Prim)
import qualified F9165819906499745633 as G (MVector)
import F8599798322733369681 (MVector)
import qualified F9165819906499745633 as G (basicLength)
import F8599798322733369681 (MVector(MVector))
import qualified F9165819906499745633 as G (basicUnsafeSlice)
import GHC.Num ((+))

instance Prim a => G.MVector MVector a where
        basicLength (MVector _ n _) = n
        basicUnsafeSlice j m (MVector i n arr) = MVector (i + j) m arr
