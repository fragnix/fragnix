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
import qualified F9165819906499745633 as G (basicOverlaps)
import GHC.Classes ((&&))
import GHC.Classes ((||))
import GHC.Classes ((>=))
import GHC.Classes ((<))
import qualified F9165819906499745633 as G (basicUnsafeNew)
import GHC.Num ((*))
import GHC.Err (undefined)
import qualified F9165819906499745633 as G (basicUnsafeRead)
import qualified F9165819906499745633 as G (basicUnsafeWrite)
import qualified F9165819906499745633 as G (basicUnsafeCopy)
import qualified F9165819906499745633 as G (basicUnsafeMove)
import qualified F9165819906499745633 as G (basicSet)

instance Prim a => G.MVector MVector a where
        basicLength (MVector _ n _) = n
        basicUnsafeSlice j m (MVector i n arr) = MVector (i + j) m arr
        
        {-# INLINE basicOverlaps #-}
        basicOverlaps (MVector i m arr1) (MVector j n arr2)
          = undefined
        
        {-# INLINE basicUnsafeNew #-}
        basicUnsafeNew n
          = undefined
        
        {-# INLINE basicUnsafeRead #-}
        basicUnsafeRead (MVector i n arr) j = undefined
        
        {-# INLINE basicUnsafeWrite #-}
        basicUnsafeWrite (MVector i n arr) j x
          = undefined
        
        {-# INLINE basicUnsafeCopy #-}
        basicUnsafeCopy (MVector i n dst) (MVector j _ src)
          = undefined
        
        {-# INLINE basicUnsafeMove #-}
        basicUnsafeMove (MVector i n dst) (MVector j _ src)
          = undefined
        
        {-# INLINE basicSet #-}
        basicSet (MVector i n arr) x = undefined
