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
import {-# SOURCE #-} F3977838970895265051 ()
import {-# SOURCE #-} F6650074180974082130 ()


class MVector v a where
        basicLength :: v s a -> Int
        
        basicUnsafeSlice :: Int -> Int -> v s a -> v s a
        
        basicOverlaps :: v s a -> v s a -> Bool
        
        basicUnsafeNew :: Int -> m (v s a)
        
        basicUnsafeReplicate ::
                                Int -> a -> m (v s a)
        
        basicUnsafeRead ::  v s a -> Int -> m a
        
        basicUnsafeWrite ::
                            v s a -> Int -> a -> m ()
        
        basicClear ::  v s a -> m ()
        
        basicSet ::  v s a -> a -> m ()
        
        basicUnsafeCopy ::
                           v s a -> v s a -> m ()
        
        basicUnsafeMove ::
                           v s a -> v s a -> m ()
        
        basicUnsafeGrow ::
                           v s a -> Int -> m (v s a)
        