{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable, Rank2Types,
  MultiParamTypeClasses, FlexibleContexts, TypeFamilies,
  ScopedTypeVariables, BangPatterns, NondecreasingIndentation,
  ExplicitForAll, PatternGuards #-}
module F262630417951198712 where
import F9165819906499745633 (MVector)
import GHC.Types (Int)
import GHC.Base (Monad)
import GHC.Classes ((<))
import GHC.Num ((+))
import GHC.Base (otherwise)
import GHC.Base (return)

class Vector v a where
        
        basicLength :: v a -> Int