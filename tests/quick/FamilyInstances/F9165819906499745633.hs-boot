{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable,
  MultiParamTypeClasses, BangPatterns, ScopedTypeVariables,
  TypeFamilies, NondecreasingIndentation, ExplicitForAll,
  PatternGuards #-}
module F9165819906499745633 where
import GHC.Types (Int)
import GHC.Types (Bool)
import GHC.Base (return)
import GHC.Classes ((==))
import GHC.Base (otherwise)
import GHC.Num ((*))
import GHC.Classes ((<))
import GHC.Num ((-))
import GHC.Num ((+))
import GHC.Base (($))
import GHC.Classes (not)
import GHC.Prim (seq)


class MVector v a where
        basicLength :: v s a -> Int
        
        basicUnsafeSlice :: Int -> Int -> v s a -> v s a
        