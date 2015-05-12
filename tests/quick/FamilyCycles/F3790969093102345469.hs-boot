{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable,
  FlexibleInstances, MultiParamTypeClasses, TypeFamilies,
  ScopedTypeVariables, Rank2Types, NondecreasingIndentation,RoleAnnotations,
  ExplicitForAll, PatternGuards #-}
module F3790969093102345469 where
import GHC.Show (Show)
import {-# SOURCE #-} F903085878682639365 (Prim)
import GHC.Show (showsPrec)

data Vector a = Vector
type role Vector phantom

instance (Show a, Prim a) => Show (Vector a)