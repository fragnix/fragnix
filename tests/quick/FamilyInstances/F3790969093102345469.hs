{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable,
  FlexibleInstances, MultiParamTypeClasses, TypeFamilies,
  ScopedTypeVariables, Rank2Types, NondecreasingIndentation,
  ExplicitForAll, PatternGuards #-}
module F3790969093102345469 where
import GHC.Show (Show)
import F903085878682639365 (Prim)
import GHC.Show (showsPrec)
import F9165819906499745633 (MVector)
import Prelude (undefined)

data Vector a = Vector

instance (Show a, Prim a) => Show (Vector a) where
        showsPrec = undefined