{-# LANGUAGE Haskell2010, CPP, DeriveDataTypeable #-}
{-# LINE 1 "Data/Vector/Unboxed/Base.hs" #-}

















































{-# LANGUAGE BangPatterns, MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
{-# OPTIONS_HADDOCK hide #-}

-- |
-- Module      : Data.Vector.Unboxed.Base
-- Copyright   : (c) Roman Leshchinskiy 2009-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- Adaptive unboxed vectors: basic implementation
--

module Data.Vector.Unboxed.Base (
  MVector(..), IOVector, STVector, Vector(..), Unbox
) where

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M

import qualified Data.Vector.Primitive as P

import Control.DeepSeq ( NFData(rnf) )

import Control.Monad.Primitive
import Control.Monad ( liftM )

import Data.Word ( Word, Word8, Word16, Word32, Word64 )
import Data.Int  ( Int8, Int16, Int32, Int64 )
import Data.Complex

import Data.Typeable ( Typeable )

import Data.Data     ( Data(..) )



import qualified Data.Vector.Internal.Check as Ck





data family MVector s a
data family Vector    a

type IOVector = MVector RealWorld
type STVector s = MVector s

type instance G.Mutable Vector = MVector

class (G.Vector Vector a, M.MVector MVector a) => Unbox a

instance NFData (Vector a) where rnf !_ = ()
instance NFData (MVector s a) where rnf !_ = ()

-- -----------------
-- Data and Typeable
-- -----------------
deriving instance Typeable Vector
deriving instance Typeable MVector

instance (Data a, Unbox a) => Data (Vector a) where
  gfoldl       = G.gfoldl
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = G.mkType "Data.Vector.Unboxed.Vector"
  dataCast1    = G.dataCast

-- ----
-- Unit
-- ----

newtype instance MVector s () = MV_Unit Int
newtype instance Vector    () = V_Unit Int

instance Unbox ()

instance M.MVector MVector () where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}

  basicLength (MV_Unit n) = n

  basicUnsafeSlice i m (MV_Unit n) = MV_Unit m

  basicOverlaps _ _ = False

  basicUnsafeNew n = return (MV_Unit n)

  basicUnsafeRead (MV_Unit _) _ = return ()

  basicUnsafeWrite (MV_Unit _) _ () = return ()

  basicClear _ = return ()

  basicSet (MV_Unit _) () = return ()

  basicUnsafeCopy (MV_Unit _) (MV_Unit _) = return ()

  basicUnsafeGrow (MV_Unit n) m = return $ MV_Unit (n+m)

instance G.Vector Vector () where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MV_Unit n) = return $ V_Unit n

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (V_Unit n) = return $ MV_Unit n

  {-# INLINE basicLength #-}
  basicLength (V_Unit n) = n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i m (V_Unit n) = V_Unit m

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_Unit _) i = return ()

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_Unit _) (V_Unit _) = return ()

  {-# INLINE elemseq #-}
  elemseq _ = seq


-- ---------------
-- Primitive types
-- ---------------



newtype instance MVector s Int = MV_Int (P.MVector s Int)
newtype instance Vector    Int = V_Int  (P.Vector    Int)
instance Unbox Int
instance M.MVector MVector Int where {                                     {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicOverlaps #-}                                          ; {-# INLINE basicUnsafeNew #-}                                         ; {-# INLINE basicUnsafeReplicate #-}                                   ; {-# INLINE basicUnsafeRead #-}                                        ; {-# INLINE basicUnsafeWrite #-}                                       ; {-# INLINE basicClear #-}                                             ; {-# INLINE basicSet #-}                                               ; {-# INLINE basicUnsafeCopy #-}                                        ; {-# INLINE basicUnsafeGrow #-}                                        ; basicLength ( MV_Int v) = M.basicLength v                                 ; basicUnsafeSlice i n ( MV_Int v) =  MV_Int $ M.basicUnsafeSlice i n v         ; basicOverlaps ( MV_Int v1) ( MV_Int v2) = M.basicOverlaps v1 v2               ; basicUnsafeNew n =  MV_Int `liftM` M.basicUnsafeNew n                     ; basicUnsafeReplicate n x =  MV_Int `liftM` M.basicUnsafeReplicate n x     ; basicUnsafeRead ( MV_Int v) i = M.basicUnsafeRead v i                     ; basicUnsafeWrite ( MV_Int v) i x = M.basicUnsafeWrite v i x               ; basicClear ( MV_Int v) = M.basicClear v                                   ; basicSet ( MV_Int v) x = M.basicSet v x                                   ; basicUnsafeCopy ( MV_Int v1) ( MV_Int v2) = M.basicUnsafeCopy v1 v2           ; basicUnsafeMove ( MV_Int v1) ( MV_Int v2) = M.basicUnsafeMove v1 v2           ; basicUnsafeGrow ( MV_Int v) n =  MV_Int `liftM` M.basicUnsafeGrow v n }
instance G.Vector Vector Int where {                                       {-# INLINE basicUnsafeFreeze #-}                                      ; {-# INLINE basicUnsafeThaw #-}                                        ; {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicUnsafeIndexM #-}                                      ; {-# INLINE elemseq #-}                                                ; basicUnsafeFreeze ( MV_Int v) =  V_Int `liftM` G.basicUnsafeFreeze v        ; basicUnsafeThaw ( V_Int v) =  MV_Int `liftM` G.basicUnsafeThaw v            ; basicLength ( V_Int v) = G.basicLength v                                 ; basicUnsafeSlice i n ( V_Int v) =  V_Int $ G.basicUnsafeSlice i n v         ; basicUnsafeIndexM ( V_Int v) i = G.basicUnsafeIndexM v i                 ; basicUnsafeCopy ( MV_Int mv) ( V_Int v) = G.basicUnsafeCopy mv v            ; elemseq _ = seq }

newtype instance MVector s Int8 = MV_Int8 (P.MVector s Int8)
newtype instance Vector    Int8 = V_Int8  (P.Vector    Int8)
instance Unbox Int8
instance M.MVector MVector Int8 where {                                     {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicOverlaps #-}                                          ; {-# INLINE basicUnsafeNew #-}                                         ; {-# INLINE basicUnsafeReplicate #-}                                   ; {-# INLINE basicUnsafeRead #-}                                        ; {-# INLINE basicUnsafeWrite #-}                                       ; {-# INLINE basicClear #-}                                             ; {-# INLINE basicSet #-}                                               ; {-# INLINE basicUnsafeCopy #-}                                        ; {-# INLINE basicUnsafeGrow #-}                                        ; basicLength ( MV_Int8 v) = M.basicLength v                                 ; basicUnsafeSlice i n ( MV_Int8 v) =  MV_Int8 $ M.basicUnsafeSlice i n v         ; basicOverlaps ( MV_Int8 v1) ( MV_Int8 v2) = M.basicOverlaps v1 v2               ; basicUnsafeNew n =  MV_Int8 `liftM` M.basicUnsafeNew n                     ; basicUnsafeReplicate n x =  MV_Int8 `liftM` M.basicUnsafeReplicate n x     ; basicUnsafeRead ( MV_Int8 v) i = M.basicUnsafeRead v i                     ; basicUnsafeWrite ( MV_Int8 v) i x = M.basicUnsafeWrite v i x               ; basicClear ( MV_Int8 v) = M.basicClear v                                   ; basicSet ( MV_Int8 v) x = M.basicSet v x                                   ; basicUnsafeCopy ( MV_Int8 v1) ( MV_Int8 v2) = M.basicUnsafeCopy v1 v2           ; basicUnsafeMove ( MV_Int8 v1) ( MV_Int8 v2) = M.basicUnsafeMove v1 v2           ; basicUnsafeGrow ( MV_Int8 v) n =  MV_Int8 `liftM` M.basicUnsafeGrow v n }
instance G.Vector Vector Int8 where {                                       {-# INLINE basicUnsafeFreeze #-}                                      ; {-# INLINE basicUnsafeThaw #-}                                        ; {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicUnsafeIndexM #-}                                      ; {-# INLINE elemseq #-}                                                ; basicUnsafeFreeze ( MV_Int8 v) =  V_Int8 `liftM` G.basicUnsafeFreeze v        ; basicUnsafeThaw ( V_Int8 v) =  MV_Int8 `liftM` G.basicUnsafeThaw v            ; basicLength ( V_Int8 v) = G.basicLength v                                 ; basicUnsafeSlice i n ( V_Int8 v) =  V_Int8 $ G.basicUnsafeSlice i n v         ; basicUnsafeIndexM ( V_Int8 v) i = G.basicUnsafeIndexM v i                 ; basicUnsafeCopy ( MV_Int8 mv) ( V_Int8 v) = G.basicUnsafeCopy mv v            ; elemseq _ = seq }

newtype instance MVector s Int16 = MV_Int16 (P.MVector s Int16)
newtype instance Vector    Int16 = V_Int16  (P.Vector    Int16)
instance Unbox Int16
instance M.MVector MVector Int16 where {                                     {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicOverlaps #-}                                          ; {-# INLINE basicUnsafeNew #-}                                         ; {-# INLINE basicUnsafeReplicate #-}                                   ; {-# INLINE basicUnsafeRead #-}                                        ; {-# INLINE basicUnsafeWrite #-}                                       ; {-# INLINE basicClear #-}                                             ; {-# INLINE basicSet #-}                                               ; {-# INLINE basicUnsafeCopy #-}                                        ; {-# INLINE basicUnsafeGrow #-}                                        ; basicLength ( MV_Int16 v) = M.basicLength v                                 ; basicUnsafeSlice i n ( MV_Int16 v) =  MV_Int16 $ M.basicUnsafeSlice i n v         ; basicOverlaps ( MV_Int16 v1) ( MV_Int16 v2) = M.basicOverlaps v1 v2               ; basicUnsafeNew n =  MV_Int16 `liftM` M.basicUnsafeNew n                     ; basicUnsafeReplicate n x =  MV_Int16 `liftM` M.basicUnsafeReplicate n x     ; basicUnsafeRead ( MV_Int16 v) i = M.basicUnsafeRead v i                     ; basicUnsafeWrite ( MV_Int16 v) i x = M.basicUnsafeWrite v i x               ; basicClear ( MV_Int16 v) = M.basicClear v                                   ; basicSet ( MV_Int16 v) x = M.basicSet v x                                   ; basicUnsafeCopy ( MV_Int16 v1) ( MV_Int16 v2) = M.basicUnsafeCopy v1 v2           ; basicUnsafeMove ( MV_Int16 v1) ( MV_Int16 v2) = M.basicUnsafeMove v1 v2           ; basicUnsafeGrow ( MV_Int16 v) n =  MV_Int16 `liftM` M.basicUnsafeGrow v n }
instance G.Vector Vector Int16 where {                                       {-# INLINE basicUnsafeFreeze #-}                                      ; {-# INLINE basicUnsafeThaw #-}                                        ; {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicUnsafeIndexM #-}                                      ; {-# INLINE elemseq #-}                                                ; basicUnsafeFreeze ( MV_Int16 v) =  V_Int16 `liftM` G.basicUnsafeFreeze v        ; basicUnsafeThaw ( V_Int16 v) =  MV_Int16 `liftM` G.basicUnsafeThaw v            ; basicLength ( V_Int16 v) = G.basicLength v                                 ; basicUnsafeSlice i n ( V_Int16 v) =  V_Int16 $ G.basicUnsafeSlice i n v         ; basicUnsafeIndexM ( V_Int16 v) i = G.basicUnsafeIndexM v i                 ; basicUnsafeCopy ( MV_Int16 mv) ( V_Int16 v) = G.basicUnsafeCopy mv v            ; elemseq _ = seq }

newtype instance MVector s Int32 = MV_Int32 (P.MVector s Int32)
newtype instance Vector    Int32 = V_Int32  (P.Vector    Int32)
instance Unbox Int32
instance M.MVector MVector Int32 where {                                     {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicOverlaps #-}                                          ; {-# INLINE basicUnsafeNew #-}                                         ; {-# INLINE basicUnsafeReplicate #-}                                   ; {-# INLINE basicUnsafeRead #-}                                        ; {-# INLINE basicUnsafeWrite #-}                                       ; {-# INLINE basicClear #-}                                             ; {-# INLINE basicSet #-}                                               ; {-# INLINE basicUnsafeCopy #-}                                        ; {-# INLINE basicUnsafeGrow #-}                                        ; basicLength ( MV_Int32 v) = M.basicLength v                                 ; basicUnsafeSlice i n ( MV_Int32 v) =  MV_Int32 $ M.basicUnsafeSlice i n v         ; basicOverlaps ( MV_Int32 v1) ( MV_Int32 v2) = M.basicOverlaps v1 v2               ; basicUnsafeNew n =  MV_Int32 `liftM` M.basicUnsafeNew n                     ; basicUnsafeReplicate n x =  MV_Int32 `liftM` M.basicUnsafeReplicate n x     ; basicUnsafeRead ( MV_Int32 v) i = M.basicUnsafeRead v i                     ; basicUnsafeWrite ( MV_Int32 v) i x = M.basicUnsafeWrite v i x               ; basicClear ( MV_Int32 v) = M.basicClear v                                   ; basicSet ( MV_Int32 v) x = M.basicSet v x                                   ; basicUnsafeCopy ( MV_Int32 v1) ( MV_Int32 v2) = M.basicUnsafeCopy v1 v2           ; basicUnsafeMove ( MV_Int32 v1) ( MV_Int32 v2) = M.basicUnsafeMove v1 v2           ; basicUnsafeGrow ( MV_Int32 v) n =  MV_Int32 `liftM` M.basicUnsafeGrow v n }
instance G.Vector Vector Int32 where {                                       {-# INLINE basicUnsafeFreeze #-}                                      ; {-# INLINE basicUnsafeThaw #-}                                        ; {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicUnsafeIndexM #-}                                      ; {-# INLINE elemseq #-}                                                ; basicUnsafeFreeze ( MV_Int32 v) =  V_Int32 `liftM` G.basicUnsafeFreeze v        ; basicUnsafeThaw ( V_Int32 v) =  MV_Int32 `liftM` G.basicUnsafeThaw v            ; basicLength ( V_Int32 v) = G.basicLength v                                 ; basicUnsafeSlice i n ( V_Int32 v) =  V_Int32 $ G.basicUnsafeSlice i n v         ; basicUnsafeIndexM ( V_Int32 v) i = G.basicUnsafeIndexM v i                 ; basicUnsafeCopy ( MV_Int32 mv) ( V_Int32 v) = G.basicUnsafeCopy mv v            ; elemseq _ = seq }

newtype instance MVector s Int64 = MV_Int64 (P.MVector s Int64)
newtype instance Vector    Int64 = V_Int64  (P.Vector    Int64)
instance Unbox Int64
instance M.MVector MVector Int64 where {                                     {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicOverlaps #-}                                          ; {-# INLINE basicUnsafeNew #-}                                         ; {-# INLINE basicUnsafeReplicate #-}                                   ; {-# INLINE basicUnsafeRead #-}                                        ; {-# INLINE basicUnsafeWrite #-}                                       ; {-# INLINE basicClear #-}                                             ; {-# INLINE basicSet #-}                                               ; {-# INLINE basicUnsafeCopy #-}                                        ; {-# INLINE basicUnsafeGrow #-}                                        ; basicLength ( MV_Int64 v) = M.basicLength v                                 ; basicUnsafeSlice i n ( MV_Int64 v) =  MV_Int64 $ M.basicUnsafeSlice i n v         ; basicOverlaps ( MV_Int64 v1) ( MV_Int64 v2) = M.basicOverlaps v1 v2               ; basicUnsafeNew n =  MV_Int64 `liftM` M.basicUnsafeNew n                     ; basicUnsafeReplicate n x =  MV_Int64 `liftM` M.basicUnsafeReplicate n x     ; basicUnsafeRead ( MV_Int64 v) i = M.basicUnsafeRead v i                     ; basicUnsafeWrite ( MV_Int64 v) i x = M.basicUnsafeWrite v i x               ; basicClear ( MV_Int64 v) = M.basicClear v                                   ; basicSet ( MV_Int64 v) x = M.basicSet v x                                   ; basicUnsafeCopy ( MV_Int64 v1) ( MV_Int64 v2) = M.basicUnsafeCopy v1 v2           ; basicUnsafeMove ( MV_Int64 v1) ( MV_Int64 v2) = M.basicUnsafeMove v1 v2           ; basicUnsafeGrow ( MV_Int64 v) n =  MV_Int64 `liftM` M.basicUnsafeGrow v n }
instance G.Vector Vector Int64 where {                                       {-# INLINE basicUnsafeFreeze #-}                                      ; {-# INLINE basicUnsafeThaw #-}                                        ; {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicUnsafeIndexM #-}                                      ; {-# INLINE elemseq #-}                                                ; basicUnsafeFreeze ( MV_Int64 v) =  V_Int64 `liftM` G.basicUnsafeFreeze v        ; basicUnsafeThaw ( V_Int64 v) =  MV_Int64 `liftM` G.basicUnsafeThaw v            ; basicLength ( V_Int64 v) = G.basicLength v                                 ; basicUnsafeSlice i n ( V_Int64 v) =  V_Int64 $ G.basicUnsafeSlice i n v         ; basicUnsafeIndexM ( V_Int64 v) i = G.basicUnsafeIndexM v i                 ; basicUnsafeCopy ( MV_Int64 mv) ( V_Int64 v) = G.basicUnsafeCopy mv v            ; elemseq _ = seq }


newtype instance MVector s Word = MV_Word (P.MVector s Word)
newtype instance Vector    Word = V_Word  (P.Vector    Word)
instance Unbox Word
instance M.MVector MVector Word where {                                     {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicOverlaps #-}                                          ; {-# INLINE basicUnsafeNew #-}                                         ; {-# INLINE basicUnsafeReplicate #-}                                   ; {-# INLINE basicUnsafeRead #-}                                        ; {-# INLINE basicUnsafeWrite #-}                                       ; {-# INLINE basicClear #-}                                             ; {-# INLINE basicSet #-}                                               ; {-# INLINE basicUnsafeCopy #-}                                        ; {-# INLINE basicUnsafeGrow #-}                                        ; basicLength ( MV_Word v) = M.basicLength v                                 ; basicUnsafeSlice i n ( MV_Word v) =  MV_Word $ M.basicUnsafeSlice i n v         ; basicOverlaps ( MV_Word v1) ( MV_Word v2) = M.basicOverlaps v1 v2               ; basicUnsafeNew n =  MV_Word `liftM` M.basicUnsafeNew n                     ; basicUnsafeReplicate n x =  MV_Word `liftM` M.basicUnsafeReplicate n x     ; basicUnsafeRead ( MV_Word v) i = M.basicUnsafeRead v i                     ; basicUnsafeWrite ( MV_Word v) i x = M.basicUnsafeWrite v i x               ; basicClear ( MV_Word v) = M.basicClear v                                   ; basicSet ( MV_Word v) x = M.basicSet v x                                   ; basicUnsafeCopy ( MV_Word v1) ( MV_Word v2) = M.basicUnsafeCopy v1 v2           ; basicUnsafeMove ( MV_Word v1) ( MV_Word v2) = M.basicUnsafeMove v1 v2           ; basicUnsafeGrow ( MV_Word v) n =  MV_Word `liftM` M.basicUnsafeGrow v n }
instance G.Vector Vector Word where {                                       {-# INLINE basicUnsafeFreeze #-}                                      ; {-# INLINE basicUnsafeThaw #-}                                        ; {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicUnsafeIndexM #-}                                      ; {-# INLINE elemseq #-}                                                ; basicUnsafeFreeze ( MV_Word v) =  V_Word `liftM` G.basicUnsafeFreeze v        ; basicUnsafeThaw ( V_Word v) =  MV_Word `liftM` G.basicUnsafeThaw v            ; basicLength ( V_Word v) = G.basicLength v                                 ; basicUnsafeSlice i n ( V_Word v) =  V_Word $ G.basicUnsafeSlice i n v         ; basicUnsafeIndexM ( V_Word v) i = G.basicUnsafeIndexM v i                 ; basicUnsafeCopy ( MV_Word mv) ( V_Word v) = G.basicUnsafeCopy mv v            ; elemseq _ = seq }

newtype instance MVector s Word8 = MV_Word8 (P.MVector s Word8)
newtype instance Vector    Word8 = V_Word8  (P.Vector    Word8)
instance Unbox Word8
instance M.MVector MVector Word8 where {                                     {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicOverlaps #-}                                          ; {-# INLINE basicUnsafeNew #-}                                         ; {-# INLINE basicUnsafeReplicate #-}                                   ; {-# INLINE basicUnsafeRead #-}                                        ; {-# INLINE basicUnsafeWrite #-}                                       ; {-# INLINE basicClear #-}                                             ; {-# INLINE basicSet #-}                                               ; {-# INLINE basicUnsafeCopy #-}                                        ; {-# INLINE basicUnsafeGrow #-}                                        ; basicLength ( MV_Word8 v) = M.basicLength v                                 ; basicUnsafeSlice i n ( MV_Word8 v) =  MV_Word8 $ M.basicUnsafeSlice i n v         ; basicOverlaps ( MV_Word8 v1) ( MV_Word8 v2) = M.basicOverlaps v1 v2               ; basicUnsafeNew n =  MV_Word8 `liftM` M.basicUnsafeNew n                     ; basicUnsafeReplicate n x =  MV_Word8 `liftM` M.basicUnsafeReplicate n x     ; basicUnsafeRead ( MV_Word8 v) i = M.basicUnsafeRead v i                     ; basicUnsafeWrite ( MV_Word8 v) i x = M.basicUnsafeWrite v i x               ; basicClear ( MV_Word8 v) = M.basicClear v                                   ; basicSet ( MV_Word8 v) x = M.basicSet v x                                   ; basicUnsafeCopy ( MV_Word8 v1) ( MV_Word8 v2) = M.basicUnsafeCopy v1 v2           ; basicUnsafeMove ( MV_Word8 v1) ( MV_Word8 v2) = M.basicUnsafeMove v1 v2           ; basicUnsafeGrow ( MV_Word8 v) n =  MV_Word8 `liftM` M.basicUnsafeGrow v n }
instance G.Vector Vector Word8 where {                                       {-# INLINE basicUnsafeFreeze #-}                                      ; {-# INLINE basicUnsafeThaw #-}                                        ; {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicUnsafeIndexM #-}                                      ; {-# INLINE elemseq #-}                                                ; basicUnsafeFreeze ( MV_Word8 v) =  V_Word8 `liftM` G.basicUnsafeFreeze v        ; basicUnsafeThaw ( V_Word8 v) =  MV_Word8 `liftM` G.basicUnsafeThaw v            ; basicLength ( V_Word8 v) = G.basicLength v                                 ; basicUnsafeSlice i n ( V_Word8 v) =  V_Word8 $ G.basicUnsafeSlice i n v         ; basicUnsafeIndexM ( V_Word8 v) i = G.basicUnsafeIndexM v i                 ; basicUnsafeCopy ( MV_Word8 mv) ( V_Word8 v) = G.basicUnsafeCopy mv v            ; elemseq _ = seq }

newtype instance MVector s Word16 = MV_Word16 (P.MVector s Word16)
newtype instance Vector    Word16 = V_Word16  (P.Vector    Word16)
instance Unbox Word16
instance M.MVector MVector Word16 where {                                     {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicOverlaps #-}                                          ; {-# INLINE basicUnsafeNew #-}                                         ; {-# INLINE basicUnsafeReplicate #-}                                   ; {-# INLINE basicUnsafeRead #-}                                        ; {-# INLINE basicUnsafeWrite #-}                                       ; {-# INLINE basicClear #-}                                             ; {-# INLINE basicSet #-}                                               ; {-# INLINE basicUnsafeCopy #-}                                        ; {-# INLINE basicUnsafeGrow #-}                                        ; basicLength ( MV_Word16 v) = M.basicLength v                                 ; basicUnsafeSlice i n ( MV_Word16 v) =  MV_Word16 $ M.basicUnsafeSlice i n v         ; basicOverlaps ( MV_Word16 v1) ( MV_Word16 v2) = M.basicOverlaps v1 v2               ; basicUnsafeNew n =  MV_Word16 `liftM` M.basicUnsafeNew n                     ; basicUnsafeReplicate n x =  MV_Word16 `liftM` M.basicUnsafeReplicate n x     ; basicUnsafeRead ( MV_Word16 v) i = M.basicUnsafeRead v i                     ; basicUnsafeWrite ( MV_Word16 v) i x = M.basicUnsafeWrite v i x               ; basicClear ( MV_Word16 v) = M.basicClear v                                   ; basicSet ( MV_Word16 v) x = M.basicSet v x                                   ; basicUnsafeCopy ( MV_Word16 v1) ( MV_Word16 v2) = M.basicUnsafeCopy v1 v2           ; basicUnsafeMove ( MV_Word16 v1) ( MV_Word16 v2) = M.basicUnsafeMove v1 v2           ; basicUnsafeGrow ( MV_Word16 v) n =  MV_Word16 `liftM` M.basicUnsafeGrow v n }
instance G.Vector Vector Word16 where {                                       {-# INLINE basicUnsafeFreeze #-}                                      ; {-# INLINE basicUnsafeThaw #-}                                        ; {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicUnsafeIndexM #-}                                      ; {-# INLINE elemseq #-}                                                ; basicUnsafeFreeze ( MV_Word16 v) =  V_Word16 `liftM` G.basicUnsafeFreeze v        ; basicUnsafeThaw ( V_Word16 v) =  MV_Word16 `liftM` G.basicUnsafeThaw v            ; basicLength ( V_Word16 v) = G.basicLength v                                 ; basicUnsafeSlice i n ( V_Word16 v) =  V_Word16 $ G.basicUnsafeSlice i n v         ; basicUnsafeIndexM ( V_Word16 v) i = G.basicUnsafeIndexM v i                 ; basicUnsafeCopy ( MV_Word16 mv) ( V_Word16 v) = G.basicUnsafeCopy mv v            ; elemseq _ = seq }

newtype instance MVector s Word32 = MV_Word32 (P.MVector s Word32)
newtype instance Vector    Word32 = V_Word32  (P.Vector    Word32)
instance Unbox Word32
instance M.MVector MVector Word32 where {                                     {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicOverlaps #-}                                          ; {-# INLINE basicUnsafeNew #-}                                         ; {-# INLINE basicUnsafeReplicate #-}                                   ; {-# INLINE basicUnsafeRead #-}                                        ; {-# INLINE basicUnsafeWrite #-}                                       ; {-# INLINE basicClear #-}                                             ; {-# INLINE basicSet #-}                                               ; {-# INLINE basicUnsafeCopy #-}                                        ; {-# INLINE basicUnsafeGrow #-}                                        ; basicLength ( MV_Word32 v) = M.basicLength v                                 ; basicUnsafeSlice i n ( MV_Word32 v) =  MV_Word32 $ M.basicUnsafeSlice i n v         ; basicOverlaps ( MV_Word32 v1) ( MV_Word32 v2) = M.basicOverlaps v1 v2               ; basicUnsafeNew n =  MV_Word32 `liftM` M.basicUnsafeNew n                     ; basicUnsafeReplicate n x =  MV_Word32 `liftM` M.basicUnsafeReplicate n x     ; basicUnsafeRead ( MV_Word32 v) i = M.basicUnsafeRead v i                     ; basicUnsafeWrite ( MV_Word32 v) i x = M.basicUnsafeWrite v i x               ; basicClear ( MV_Word32 v) = M.basicClear v                                   ; basicSet ( MV_Word32 v) x = M.basicSet v x                                   ; basicUnsafeCopy ( MV_Word32 v1) ( MV_Word32 v2) = M.basicUnsafeCopy v1 v2           ; basicUnsafeMove ( MV_Word32 v1) ( MV_Word32 v2) = M.basicUnsafeMove v1 v2           ; basicUnsafeGrow ( MV_Word32 v) n =  MV_Word32 `liftM` M.basicUnsafeGrow v n }
instance G.Vector Vector Word32 where {                                       {-# INLINE basicUnsafeFreeze #-}                                      ; {-# INLINE basicUnsafeThaw #-}                                        ; {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicUnsafeIndexM #-}                                      ; {-# INLINE elemseq #-}                                                ; basicUnsafeFreeze ( MV_Word32 v) =  V_Word32 `liftM` G.basicUnsafeFreeze v        ; basicUnsafeThaw ( V_Word32 v) =  MV_Word32 `liftM` G.basicUnsafeThaw v            ; basicLength ( V_Word32 v) = G.basicLength v                                 ; basicUnsafeSlice i n ( V_Word32 v) =  V_Word32 $ G.basicUnsafeSlice i n v         ; basicUnsafeIndexM ( V_Word32 v) i = G.basicUnsafeIndexM v i                 ; basicUnsafeCopy ( MV_Word32 mv) ( V_Word32 v) = G.basicUnsafeCopy mv v            ; elemseq _ = seq }

newtype instance MVector s Word64 = MV_Word64 (P.MVector s Word64)
newtype instance Vector    Word64 = V_Word64  (P.Vector    Word64)
instance Unbox Word64
instance M.MVector MVector Word64 where {                                     {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicOverlaps #-}                                          ; {-# INLINE basicUnsafeNew #-}                                         ; {-# INLINE basicUnsafeReplicate #-}                                   ; {-# INLINE basicUnsafeRead #-}                                        ; {-# INLINE basicUnsafeWrite #-}                                       ; {-# INLINE basicClear #-}                                             ; {-# INLINE basicSet #-}                                               ; {-# INLINE basicUnsafeCopy #-}                                        ; {-# INLINE basicUnsafeGrow #-}                                        ; basicLength ( MV_Word64 v) = M.basicLength v                                 ; basicUnsafeSlice i n ( MV_Word64 v) =  MV_Word64 $ M.basicUnsafeSlice i n v         ; basicOverlaps ( MV_Word64 v1) ( MV_Word64 v2) = M.basicOverlaps v1 v2               ; basicUnsafeNew n =  MV_Word64 `liftM` M.basicUnsafeNew n                     ; basicUnsafeReplicate n x =  MV_Word64 `liftM` M.basicUnsafeReplicate n x     ; basicUnsafeRead ( MV_Word64 v) i = M.basicUnsafeRead v i                     ; basicUnsafeWrite ( MV_Word64 v) i x = M.basicUnsafeWrite v i x               ; basicClear ( MV_Word64 v) = M.basicClear v                                   ; basicSet ( MV_Word64 v) x = M.basicSet v x                                   ; basicUnsafeCopy ( MV_Word64 v1) ( MV_Word64 v2) = M.basicUnsafeCopy v1 v2           ; basicUnsafeMove ( MV_Word64 v1) ( MV_Word64 v2) = M.basicUnsafeMove v1 v2           ; basicUnsafeGrow ( MV_Word64 v) n =  MV_Word64 `liftM` M.basicUnsafeGrow v n }
instance G.Vector Vector Word64 where {                                       {-# INLINE basicUnsafeFreeze #-}                                      ; {-# INLINE basicUnsafeThaw #-}                                        ; {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicUnsafeIndexM #-}                                      ; {-# INLINE elemseq #-}                                                ; basicUnsafeFreeze ( MV_Word64 v) =  V_Word64 `liftM` G.basicUnsafeFreeze v        ; basicUnsafeThaw ( V_Word64 v) =  MV_Word64 `liftM` G.basicUnsafeThaw v            ; basicLength ( V_Word64 v) = G.basicLength v                                 ; basicUnsafeSlice i n ( V_Word64 v) =  V_Word64 $ G.basicUnsafeSlice i n v         ; basicUnsafeIndexM ( V_Word64 v) i = G.basicUnsafeIndexM v i                 ; basicUnsafeCopy ( MV_Word64 mv) ( V_Word64 v) = G.basicUnsafeCopy mv v            ; elemseq _ = seq }


newtype instance MVector s Float = MV_Float (P.MVector s Float)
newtype instance Vector    Float = V_Float  (P.Vector    Float)
instance Unbox Float
instance M.MVector MVector Float where {                                     {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicOverlaps #-}                                          ; {-# INLINE basicUnsafeNew #-}                                         ; {-# INLINE basicUnsafeReplicate #-}                                   ; {-# INLINE basicUnsafeRead #-}                                        ; {-# INLINE basicUnsafeWrite #-}                                       ; {-# INLINE basicClear #-}                                             ; {-# INLINE basicSet #-}                                               ; {-# INLINE basicUnsafeCopy #-}                                        ; {-# INLINE basicUnsafeGrow #-}                                        ; basicLength ( MV_Float v) = M.basicLength v                                 ; basicUnsafeSlice i n ( MV_Float v) =  MV_Float $ M.basicUnsafeSlice i n v         ; basicOverlaps ( MV_Float v1) ( MV_Float v2) = M.basicOverlaps v1 v2               ; basicUnsafeNew n =  MV_Float `liftM` M.basicUnsafeNew n                     ; basicUnsafeReplicate n x =  MV_Float `liftM` M.basicUnsafeReplicate n x     ; basicUnsafeRead ( MV_Float v) i = M.basicUnsafeRead v i                     ; basicUnsafeWrite ( MV_Float v) i x = M.basicUnsafeWrite v i x               ; basicClear ( MV_Float v) = M.basicClear v                                   ; basicSet ( MV_Float v) x = M.basicSet v x                                   ; basicUnsafeCopy ( MV_Float v1) ( MV_Float v2) = M.basicUnsafeCopy v1 v2           ; basicUnsafeMove ( MV_Float v1) ( MV_Float v2) = M.basicUnsafeMove v1 v2           ; basicUnsafeGrow ( MV_Float v) n =  MV_Float `liftM` M.basicUnsafeGrow v n }
instance G.Vector Vector Float where {                                       {-# INLINE basicUnsafeFreeze #-}                                      ; {-# INLINE basicUnsafeThaw #-}                                        ; {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicUnsafeIndexM #-}                                      ; {-# INLINE elemseq #-}                                                ; basicUnsafeFreeze ( MV_Float v) =  V_Float `liftM` G.basicUnsafeFreeze v        ; basicUnsafeThaw ( V_Float v) =  MV_Float `liftM` G.basicUnsafeThaw v            ; basicLength ( V_Float v) = G.basicLength v                                 ; basicUnsafeSlice i n ( V_Float v) =  V_Float $ G.basicUnsafeSlice i n v         ; basicUnsafeIndexM ( V_Float v) i = G.basicUnsafeIndexM v i                 ; basicUnsafeCopy ( MV_Float mv) ( V_Float v) = G.basicUnsafeCopy mv v            ; elemseq _ = seq }

newtype instance MVector s Double = MV_Double (P.MVector s Double)
newtype instance Vector    Double = V_Double  (P.Vector    Double)
instance Unbox Double
instance M.MVector MVector Double where {                                     {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicOverlaps #-}                                          ; {-# INLINE basicUnsafeNew #-}                                         ; {-# INLINE basicUnsafeReplicate #-}                                   ; {-# INLINE basicUnsafeRead #-}                                        ; {-# INLINE basicUnsafeWrite #-}                                       ; {-# INLINE basicClear #-}                                             ; {-# INLINE basicSet #-}                                               ; {-# INLINE basicUnsafeCopy #-}                                        ; {-# INLINE basicUnsafeGrow #-}                                        ; basicLength ( MV_Double v) = M.basicLength v                                 ; basicUnsafeSlice i n ( MV_Double v) =  MV_Double $ M.basicUnsafeSlice i n v         ; basicOverlaps ( MV_Double v1) ( MV_Double v2) = M.basicOverlaps v1 v2               ; basicUnsafeNew n =  MV_Double `liftM` M.basicUnsafeNew n                     ; basicUnsafeReplicate n x =  MV_Double `liftM` M.basicUnsafeReplicate n x     ; basicUnsafeRead ( MV_Double v) i = M.basicUnsafeRead v i                     ; basicUnsafeWrite ( MV_Double v) i x = M.basicUnsafeWrite v i x               ; basicClear ( MV_Double v) = M.basicClear v                                   ; basicSet ( MV_Double v) x = M.basicSet v x                                   ; basicUnsafeCopy ( MV_Double v1) ( MV_Double v2) = M.basicUnsafeCopy v1 v2           ; basicUnsafeMove ( MV_Double v1) ( MV_Double v2) = M.basicUnsafeMove v1 v2           ; basicUnsafeGrow ( MV_Double v) n =  MV_Double `liftM` M.basicUnsafeGrow v n }
instance G.Vector Vector Double where {                                       {-# INLINE basicUnsafeFreeze #-}                                      ; {-# INLINE basicUnsafeThaw #-}                                        ; {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicUnsafeIndexM #-}                                      ; {-# INLINE elemseq #-}                                                ; basicUnsafeFreeze ( MV_Double v) =  V_Double `liftM` G.basicUnsafeFreeze v        ; basicUnsafeThaw ( V_Double v) =  MV_Double `liftM` G.basicUnsafeThaw v            ; basicLength ( V_Double v) = G.basicLength v                                 ; basicUnsafeSlice i n ( V_Double v) =  V_Double $ G.basicUnsafeSlice i n v         ; basicUnsafeIndexM ( V_Double v) i = G.basicUnsafeIndexM v i                 ; basicUnsafeCopy ( MV_Double mv) ( V_Double v) = G.basicUnsafeCopy mv v            ; elemseq _ = seq }


newtype instance MVector s Char = MV_Char (P.MVector s Char)
newtype instance Vector    Char = V_Char  (P.Vector    Char)
instance Unbox Char
instance M.MVector MVector Char where {                                     {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicOverlaps #-}                                          ; {-# INLINE basicUnsafeNew #-}                                         ; {-# INLINE basicUnsafeReplicate #-}                                   ; {-# INLINE basicUnsafeRead #-}                                        ; {-# INLINE basicUnsafeWrite #-}                                       ; {-# INLINE basicClear #-}                                             ; {-# INLINE basicSet #-}                                               ; {-# INLINE basicUnsafeCopy #-}                                        ; {-# INLINE basicUnsafeGrow #-}                                        ; basicLength ( MV_Char v) = M.basicLength v                                 ; basicUnsafeSlice i n ( MV_Char v) =  MV_Char $ M.basicUnsafeSlice i n v         ; basicOverlaps ( MV_Char v1) ( MV_Char v2) = M.basicOverlaps v1 v2               ; basicUnsafeNew n =  MV_Char `liftM` M.basicUnsafeNew n                     ; basicUnsafeReplicate n x =  MV_Char `liftM` M.basicUnsafeReplicate n x     ; basicUnsafeRead ( MV_Char v) i = M.basicUnsafeRead v i                     ; basicUnsafeWrite ( MV_Char v) i x = M.basicUnsafeWrite v i x               ; basicClear ( MV_Char v) = M.basicClear v                                   ; basicSet ( MV_Char v) x = M.basicSet v x                                   ; basicUnsafeCopy ( MV_Char v1) ( MV_Char v2) = M.basicUnsafeCopy v1 v2           ; basicUnsafeMove ( MV_Char v1) ( MV_Char v2) = M.basicUnsafeMove v1 v2           ; basicUnsafeGrow ( MV_Char v) n =  MV_Char `liftM` M.basicUnsafeGrow v n }
instance G.Vector Vector Char where {                                       {-# INLINE basicUnsafeFreeze #-}                                      ; {-# INLINE basicUnsafeThaw #-}                                        ; {-# INLINE basicLength #-}                                            ; {-# INLINE basicUnsafeSlice #-}                                       ; {-# INLINE basicUnsafeIndexM #-}                                      ; {-# INLINE elemseq #-}                                                ; basicUnsafeFreeze ( MV_Char v) =  V_Char `liftM` G.basicUnsafeFreeze v        ; basicUnsafeThaw ( V_Char v) =  MV_Char `liftM` G.basicUnsafeThaw v            ; basicLength ( V_Char v) = G.basicLength v                                 ; basicUnsafeSlice i n ( V_Char v) =  V_Char $ G.basicUnsafeSlice i n v         ; basicUnsafeIndexM ( V_Char v) i = G.basicUnsafeIndexM v i                 ; basicUnsafeCopy ( MV_Char mv) ( V_Char v) = G.basicUnsafeCopy mv v            ; elemseq _ = seq }

-- ----
-- Bool
-- ----

fromBool :: Bool -> Word8
{-# INLINE fromBool #-}
fromBool True = 1
fromBool False = 0

toBool :: Word8 -> Bool
{-# INLINE toBool #-}
toBool 0 = False
toBool _ = True

newtype instance MVector s Bool = MV_Bool (P.MVector s Word8)
newtype instance Vector    Bool = V_Bool  (P.Vector    Word8)

instance Unbox Bool

instance M.MVector MVector Bool where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Bool v) = M.basicLength v
  basicUnsafeSlice i n (MV_Bool v) = MV_Bool $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Bool v1) (MV_Bool v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Bool `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n x = MV_Bool `liftM` M.basicUnsafeReplicate n (fromBool x)
  basicUnsafeRead (MV_Bool v) i = toBool `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Bool v) i x = M.basicUnsafeWrite v i (fromBool x)
  basicClear (MV_Bool v) = M.basicClear v
  basicSet (MV_Bool v) x = M.basicSet v (fromBool x)
  basicUnsafeCopy (MV_Bool v1) (MV_Bool v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Bool v1) (MV_Bool v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Bool v) n = MV_Bool `liftM` M.basicUnsafeGrow v n

instance G.Vector Vector Bool where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Bool v) = V_Bool `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Bool v) = MV_Bool `liftM` G.basicUnsafeThaw v
  basicLength (V_Bool v) = G.basicLength v
  basicUnsafeSlice i n (V_Bool v) = V_Bool $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Bool v) i = toBool `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Bool mv) (V_Bool v) = G.basicUnsafeCopy mv v
  elemseq _ = seq

-- -------
-- Complex
-- -------

newtype instance MVector s (Complex a) = MV_Complex (MVector s (a,a))
newtype instance Vector    (Complex a) = V_Complex  (Vector    (a,a))

instance (RealFloat a, Unbox a) => Unbox (Complex a)

instance (RealFloat a, Unbox a) => M.MVector MVector (Complex a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Complex v) = M.basicLength v
  basicUnsafeSlice i n (MV_Complex v) = MV_Complex $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Complex v1) (MV_Complex v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Complex `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n (x :+ y) = MV_Complex `liftM` M.basicUnsafeReplicate n (x,y)
  basicUnsafeRead (MV_Complex v) i = uncurry (:+) `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Complex v) i (x :+ y) = M.basicUnsafeWrite v i (x,y)
  basicClear (MV_Complex v) = M.basicClear v
  basicSet (MV_Complex v) (x :+ y) = M.basicSet v (x,y)
  basicUnsafeCopy (MV_Complex v1) (MV_Complex v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Complex v1) (MV_Complex v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Complex v) n = MV_Complex `liftM` M.basicUnsafeGrow v n

instance (RealFloat a, Unbox a) => G.Vector Vector (Complex a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Complex v) = V_Complex `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Complex v) = MV_Complex `liftM` G.basicUnsafeThaw v
  basicLength (V_Complex v) = G.basicLength v
  basicUnsafeSlice i n (V_Complex v) = V_Complex $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Complex v) i
                = uncurry (:+) `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Complex mv) (V_Complex v)
                = G.basicUnsafeCopy mv v
  elemseq _ (x :+ y) z = G.elemseq (undefined :: Vector a) x
                       $ G.elemseq (undefined :: Vector a) y z

-- ------
-- Tuples
-- ------

data instance MVector s (a, b)
    = MV_2 {-# UNPACK #-} !Int !(MVector s a)
                               !(MVector s b)
data instance Vector (a, b)
    = V_2 {-# UNPACK #-} !Int !(Vector a)
                              !(Vector b)
instance (Unbox a, Unbox b) => Unbox (a, b)
instance (Unbox a, Unbox b) => M.MVector MVector (a, b) where
  {-# INLINE basicLength  #-}
  basicLength (MV_2 n_ as bs) = n_
  {-# INLINE basicUnsafeSlice  #-}
  basicUnsafeSlice i_ m_ (MV_2 n_ as bs)
      = MV_2 m_ (M.basicUnsafeSlice i_ m_ as)
                (M.basicUnsafeSlice i_ m_ bs)
  {-# INLINE basicOverlaps  #-}
  basicOverlaps (MV_2 n_1 as1 bs1) (MV_2 n_2 as2 bs2)
      = M.basicOverlaps as1 as2
        || M.basicOverlaps bs1 bs2
  {-# INLINE basicUnsafeNew  #-}
  basicUnsafeNew n_
      = do
          as <- M.basicUnsafeNew n_
          bs <- M.basicUnsafeNew n_
          return $ MV_2 n_ as bs
  {-# INLINE basicUnsafeReplicate  #-}
  basicUnsafeReplicate n_ (a, b)
      = do
          as <- M.basicUnsafeReplicate n_ a
          bs <- M.basicUnsafeReplicate n_ b
          return $ MV_2 n_ as bs
  {-# INLINE basicUnsafeRead  #-}
  basicUnsafeRead (MV_2 n_ as bs) i_
      = do
          a <- M.basicUnsafeRead as i_
          b <- M.basicUnsafeRead bs i_
          return (a, b)
  {-# INLINE basicUnsafeWrite  #-}
  basicUnsafeWrite (MV_2 n_ as bs) i_ (a, b)
      = do
          M.basicUnsafeWrite as i_ a
          M.basicUnsafeWrite bs i_ b
  {-# INLINE basicClear  #-}
  basicClear (MV_2 n_ as bs)
      = do
          M.basicClear as
          M.basicClear bs
  {-# INLINE basicSet  #-}
  basicSet (MV_2 n_ as bs) (a, b)
      = do
          M.basicSet as a
          M.basicSet bs b
  {-# INLINE basicUnsafeCopy  #-}
  basicUnsafeCopy (MV_2 n_1 as1 bs1) (MV_2 n_2 as2 bs2)
      = do
          M.basicUnsafeCopy as1 as2
          M.basicUnsafeCopy bs1 bs2
  {-# INLINE basicUnsafeMove  #-}
  basicUnsafeMove (MV_2 n_1 as1 bs1) (MV_2 n_2 as2 bs2)
      = do
          M.basicUnsafeMove as1 as2
          M.basicUnsafeMove bs1 bs2
  {-# INLINE basicUnsafeGrow  #-}
  basicUnsafeGrow (MV_2 n_ as bs) m_
      = do
          as' <- M.basicUnsafeGrow as m_
          bs' <- M.basicUnsafeGrow bs m_
          return $ MV_2 (m_+n_) as' bs'
instance (Unbox a, Unbox b) => G.Vector Vector (a, b) where
  {-# INLINE basicUnsafeFreeze  #-}
  basicUnsafeFreeze (MV_2 n_ as bs)
      = do
          as' <- G.basicUnsafeFreeze as
          bs' <- G.basicUnsafeFreeze bs
          return $ V_2 n_ as' bs'
  {-# INLINE basicUnsafeThaw  #-}
  basicUnsafeThaw (V_2 n_ as bs)
      = do
          as' <- G.basicUnsafeThaw as
          bs' <- G.basicUnsafeThaw bs
          return $ MV_2 n_ as' bs'
  {-# INLINE basicLength  #-}
  basicLength (V_2 n_ as bs) = n_
  {-# INLINE basicUnsafeSlice  #-}
  basicUnsafeSlice i_ m_ (V_2 n_ as bs)
      = V_2 m_ (G.basicUnsafeSlice i_ m_ as)
               (G.basicUnsafeSlice i_ m_ bs)
  {-# INLINE basicUnsafeIndexM  #-}
  basicUnsafeIndexM (V_2 n_ as bs) i_
      = do
          a <- G.basicUnsafeIndexM as i_
          b <- G.basicUnsafeIndexM bs i_
          return (a, b)
  {-# INLINE basicUnsafeCopy  #-}
  basicUnsafeCopy (MV_2 n_1 as1 bs1) (V_2 n_2 as2 bs2)
      = do
          G.basicUnsafeCopy as1 as2
          G.basicUnsafeCopy bs1 bs2
  {-# INLINE elemseq  #-}
  elemseq _ (a, b)
      = G.elemseq (undefined :: Vector a) a
        . G.elemseq (undefined :: Vector b) b
data instance MVector s (a, b, c)
    = MV_3 {-# UNPACK #-} !Int !(MVector s a)
                               !(MVector s b)
                               !(MVector s c)
data instance Vector (a, b, c)
    = V_3 {-# UNPACK #-} !Int !(Vector a)
                              !(Vector b)
                              !(Vector c)
instance (Unbox a, Unbox b, Unbox c) => Unbox (a, b, c)
instance (Unbox a,
          Unbox b,
          Unbox c) => M.MVector MVector (a, b, c) where
  {-# INLINE basicLength  #-}
  basicLength (MV_3 n_ as bs cs) = n_
  {-# INLINE basicUnsafeSlice  #-}
  basicUnsafeSlice i_ m_ (MV_3 n_ as bs cs)
      = MV_3 m_ (M.basicUnsafeSlice i_ m_ as)
                (M.basicUnsafeSlice i_ m_ bs)
                (M.basicUnsafeSlice i_ m_ cs)
  {-# INLINE basicOverlaps  #-}
  basicOverlaps (MV_3 n_1 as1 bs1 cs1) (MV_3 n_2 as2 bs2 cs2)
      = M.basicOverlaps as1 as2
        || M.basicOverlaps bs1 bs2
        || M.basicOverlaps cs1 cs2
  {-# INLINE basicUnsafeNew  #-}
  basicUnsafeNew n_
      = do
          as <- M.basicUnsafeNew n_
          bs <- M.basicUnsafeNew n_
          cs <- M.basicUnsafeNew n_
          return $ MV_3 n_ as bs cs
  {-# INLINE basicUnsafeReplicate  #-}
  basicUnsafeReplicate n_ (a, b, c)
      = do
          as <- M.basicUnsafeReplicate n_ a
          bs <- M.basicUnsafeReplicate n_ b
          cs <- M.basicUnsafeReplicate n_ c
          return $ MV_3 n_ as bs cs
  {-# INLINE basicUnsafeRead  #-}
  basicUnsafeRead (MV_3 n_ as bs cs) i_
      = do
          a <- M.basicUnsafeRead as i_
          b <- M.basicUnsafeRead bs i_
          c <- M.basicUnsafeRead cs i_
          return (a, b, c)
  {-# INLINE basicUnsafeWrite  #-}
  basicUnsafeWrite (MV_3 n_ as bs cs) i_ (a, b, c)
      = do
          M.basicUnsafeWrite as i_ a
          M.basicUnsafeWrite bs i_ b
          M.basicUnsafeWrite cs i_ c
  {-# INLINE basicClear  #-}
  basicClear (MV_3 n_ as bs cs)
      = do
          M.basicClear as
          M.basicClear bs
          M.basicClear cs
  {-# INLINE basicSet  #-}
  basicSet (MV_3 n_ as bs cs) (a, b, c)
      = do
          M.basicSet as a
          M.basicSet bs b
          M.basicSet cs c
  {-# INLINE basicUnsafeCopy  #-}
  basicUnsafeCopy (MV_3 n_1 as1 bs1 cs1) (MV_3 n_2 as2 bs2 cs2)
      = do
          M.basicUnsafeCopy as1 as2
          M.basicUnsafeCopy bs1 bs2
          M.basicUnsafeCopy cs1 cs2
  {-# INLINE basicUnsafeMove  #-}
  basicUnsafeMove (MV_3 n_1 as1 bs1 cs1) (MV_3 n_2 as2 bs2 cs2)
      = do
          M.basicUnsafeMove as1 as2
          M.basicUnsafeMove bs1 bs2
          M.basicUnsafeMove cs1 cs2
  {-# INLINE basicUnsafeGrow  #-}
  basicUnsafeGrow (MV_3 n_ as bs cs) m_
      = do
          as' <- M.basicUnsafeGrow as m_
          bs' <- M.basicUnsafeGrow bs m_
          cs' <- M.basicUnsafeGrow cs m_
          return $ MV_3 (m_+n_) as' bs' cs'
instance (Unbox a,
          Unbox b,
          Unbox c) => G.Vector Vector (a, b, c) where
  {-# INLINE basicUnsafeFreeze  #-}
  basicUnsafeFreeze (MV_3 n_ as bs cs)
      = do
          as' <- G.basicUnsafeFreeze as
          bs' <- G.basicUnsafeFreeze bs
          cs' <- G.basicUnsafeFreeze cs
          return $ V_3 n_ as' bs' cs'
  {-# INLINE basicUnsafeThaw  #-}
  basicUnsafeThaw (V_3 n_ as bs cs)
      = do
          as' <- G.basicUnsafeThaw as
          bs' <- G.basicUnsafeThaw bs
          cs' <- G.basicUnsafeThaw cs
          return $ MV_3 n_ as' bs' cs'
  {-# INLINE basicLength  #-}
  basicLength (V_3 n_ as bs cs) = n_
  {-# INLINE basicUnsafeSlice  #-}
  basicUnsafeSlice i_ m_ (V_3 n_ as bs cs)
      = V_3 m_ (G.basicUnsafeSlice i_ m_ as)
               (G.basicUnsafeSlice i_ m_ bs)
               (G.basicUnsafeSlice i_ m_ cs)
  {-# INLINE basicUnsafeIndexM  #-}
  basicUnsafeIndexM (V_3 n_ as bs cs) i_
      = do
          a <- G.basicUnsafeIndexM as i_
          b <- G.basicUnsafeIndexM bs i_
          c <- G.basicUnsafeIndexM cs i_
          return (a, b, c)
  {-# INLINE basicUnsafeCopy  #-}
  basicUnsafeCopy (MV_3 n_1 as1 bs1 cs1) (V_3 n_2 as2 bs2 cs2)
      = do
          G.basicUnsafeCopy as1 as2
          G.basicUnsafeCopy bs1 bs2
          G.basicUnsafeCopy cs1 cs2
  {-# INLINE elemseq  #-}
  elemseq _ (a, b, c)
      = G.elemseq (undefined :: Vector a) a
        . G.elemseq (undefined :: Vector b) b
        . G.elemseq (undefined :: Vector c) c
data instance MVector s (a, b, c, d)
    = MV_4 {-# UNPACK #-} !Int !(MVector s a)
                               !(MVector s b)
                               !(MVector s c)
                               !(MVector s d)
data instance Vector (a, b, c, d)
    = V_4 {-# UNPACK #-} !Int !(Vector a)
                              !(Vector b)
                              !(Vector c)
                              !(Vector d)
instance (Unbox a, Unbox b, Unbox c, Unbox d) => Unbox (a, b, c, d)
instance (Unbox a,
          Unbox b,
          Unbox c,
          Unbox d) => M.MVector MVector (a, b, c, d) where
  {-# INLINE basicLength  #-}
  basicLength (MV_4 n_ as bs cs ds) = n_
  {-# INLINE basicUnsafeSlice  #-}
  basicUnsafeSlice i_ m_ (MV_4 n_ as bs cs ds)
      = MV_4 m_ (M.basicUnsafeSlice i_ m_ as)
                (M.basicUnsafeSlice i_ m_ bs)
                (M.basicUnsafeSlice i_ m_ cs)
                (M.basicUnsafeSlice i_ m_ ds)
  {-# INLINE basicOverlaps  #-}
  basicOverlaps (MV_4 n_1 as1 bs1 cs1 ds1) (MV_4 n_2 as2 bs2 cs2 ds2)
      = M.basicOverlaps as1 as2
        || M.basicOverlaps bs1 bs2
        || M.basicOverlaps cs1 cs2
        || M.basicOverlaps ds1 ds2
  {-# INLINE basicUnsafeNew  #-}
  basicUnsafeNew n_
      = do
          as <- M.basicUnsafeNew n_
          bs <- M.basicUnsafeNew n_
          cs <- M.basicUnsafeNew n_
          ds <- M.basicUnsafeNew n_
          return $ MV_4 n_ as bs cs ds
  {-# INLINE basicUnsafeReplicate  #-}
  basicUnsafeReplicate n_ (a, b, c, d)
      = do
          as <- M.basicUnsafeReplicate n_ a
          bs <- M.basicUnsafeReplicate n_ b
          cs <- M.basicUnsafeReplicate n_ c
          ds <- M.basicUnsafeReplicate n_ d
          return $ MV_4 n_ as bs cs ds
  {-# INLINE basicUnsafeRead  #-}
  basicUnsafeRead (MV_4 n_ as bs cs ds) i_
      = do
          a <- M.basicUnsafeRead as i_
          b <- M.basicUnsafeRead bs i_
          c <- M.basicUnsafeRead cs i_
          d <- M.basicUnsafeRead ds i_
          return (a, b, c, d)
  {-# INLINE basicUnsafeWrite  #-}
  basicUnsafeWrite (MV_4 n_ as bs cs ds) i_ (a, b, c, d)
      = do
          M.basicUnsafeWrite as i_ a
          M.basicUnsafeWrite bs i_ b
          M.basicUnsafeWrite cs i_ c
          M.basicUnsafeWrite ds i_ d
  {-# INLINE basicClear  #-}
  basicClear (MV_4 n_ as bs cs ds)
      = do
          M.basicClear as
          M.basicClear bs
          M.basicClear cs
          M.basicClear ds
  {-# INLINE basicSet  #-}
  basicSet (MV_4 n_ as bs cs ds) (a, b, c, d)
      = do
          M.basicSet as a
          M.basicSet bs b
          M.basicSet cs c
          M.basicSet ds d
  {-# INLINE basicUnsafeCopy  #-}
  basicUnsafeCopy (MV_4 n_1 as1 bs1 cs1 ds1) (MV_4 n_2 as2
                                                       bs2
                                                       cs2
                                                       ds2)
      = do
          M.basicUnsafeCopy as1 as2
          M.basicUnsafeCopy bs1 bs2
          M.basicUnsafeCopy cs1 cs2
          M.basicUnsafeCopy ds1 ds2
  {-# INLINE basicUnsafeMove  #-}
  basicUnsafeMove (MV_4 n_1 as1 bs1 cs1 ds1) (MV_4 n_2 as2
                                                       bs2
                                                       cs2
                                                       ds2)
      = do
          M.basicUnsafeMove as1 as2
          M.basicUnsafeMove bs1 bs2
          M.basicUnsafeMove cs1 cs2
          M.basicUnsafeMove ds1 ds2
  {-# INLINE basicUnsafeGrow  #-}
  basicUnsafeGrow (MV_4 n_ as bs cs ds) m_
      = do
          as' <- M.basicUnsafeGrow as m_
          bs' <- M.basicUnsafeGrow bs m_
          cs' <- M.basicUnsafeGrow cs m_
          ds' <- M.basicUnsafeGrow ds m_
          return $ MV_4 (m_+n_) as' bs' cs' ds'
instance (Unbox a,
          Unbox b,
          Unbox c,
          Unbox d) => G.Vector Vector (a, b, c, d) where
  {-# INLINE basicUnsafeFreeze  #-}
  basicUnsafeFreeze (MV_4 n_ as bs cs ds)
      = do
          as' <- G.basicUnsafeFreeze as
          bs' <- G.basicUnsafeFreeze bs
          cs' <- G.basicUnsafeFreeze cs
          ds' <- G.basicUnsafeFreeze ds
          return $ V_4 n_ as' bs' cs' ds'
  {-# INLINE basicUnsafeThaw  #-}
  basicUnsafeThaw (V_4 n_ as bs cs ds)
      = do
          as' <- G.basicUnsafeThaw as
          bs' <- G.basicUnsafeThaw bs
          cs' <- G.basicUnsafeThaw cs
          ds' <- G.basicUnsafeThaw ds
          return $ MV_4 n_ as' bs' cs' ds'
  {-# INLINE basicLength  #-}
  basicLength (V_4 n_ as bs cs ds) = n_
  {-# INLINE basicUnsafeSlice  #-}
  basicUnsafeSlice i_ m_ (V_4 n_ as bs cs ds)
      = V_4 m_ (G.basicUnsafeSlice i_ m_ as)
               (G.basicUnsafeSlice i_ m_ bs)
               (G.basicUnsafeSlice i_ m_ cs)
               (G.basicUnsafeSlice i_ m_ ds)
  {-# INLINE basicUnsafeIndexM  #-}
  basicUnsafeIndexM (V_4 n_ as bs cs ds) i_
      = do
          a <- G.basicUnsafeIndexM as i_
          b <- G.basicUnsafeIndexM bs i_
          c <- G.basicUnsafeIndexM cs i_
          d <- G.basicUnsafeIndexM ds i_
          return (a, b, c, d)
  {-# INLINE basicUnsafeCopy  #-}
  basicUnsafeCopy (MV_4 n_1 as1 bs1 cs1 ds1) (V_4 n_2 as2
                                                      bs2
                                                      cs2
                                                      ds2)
      = do
          G.basicUnsafeCopy as1 as2
          G.basicUnsafeCopy bs1 bs2
          G.basicUnsafeCopy cs1 cs2
          G.basicUnsafeCopy ds1 ds2
  {-# INLINE elemseq  #-}
  elemseq _ (a, b, c, d)
      = G.elemseq (undefined :: Vector a) a
        . G.elemseq (undefined :: Vector b) b
        . G.elemseq (undefined :: Vector c) c
        . G.elemseq (undefined :: Vector d) d
data instance MVector s (a, b, c, d, e)
    = MV_5 {-# UNPACK #-} !Int !(MVector s a)
                               !(MVector s b)
                               !(MVector s c)
                               !(MVector s d)
                               !(MVector s e)
data instance Vector (a, b, c, d, e)
    = V_5 {-# UNPACK #-} !Int !(Vector a)
                              !(Vector b)
                              !(Vector c)
                              !(Vector d)
                              !(Vector e)
instance (Unbox a,
          Unbox b,
          Unbox c,
          Unbox d,
          Unbox e) => Unbox (a, b, c, d, e)
instance (Unbox a,
          Unbox b,
          Unbox c,
          Unbox d,
          Unbox e) => M.MVector MVector (a, b, c, d, e) where
  {-# INLINE basicLength  #-}
  basicLength (MV_5 n_ as bs cs ds es) = n_
  {-# INLINE basicUnsafeSlice  #-}
  basicUnsafeSlice i_ m_ (MV_5 n_ as bs cs ds es)
      = MV_5 m_ (M.basicUnsafeSlice i_ m_ as)
                (M.basicUnsafeSlice i_ m_ bs)
                (M.basicUnsafeSlice i_ m_ cs)
                (M.basicUnsafeSlice i_ m_ ds)
                (M.basicUnsafeSlice i_ m_ es)
  {-# INLINE basicOverlaps  #-}
  basicOverlaps (MV_5 n_1 as1 bs1 cs1 ds1 es1) (MV_5 n_2 as2
                                                         bs2
                                                         cs2
                                                         ds2
                                                         es2)
      = M.basicOverlaps as1 as2
        || M.basicOverlaps bs1 bs2
        || M.basicOverlaps cs1 cs2
        || M.basicOverlaps ds1 ds2
        || M.basicOverlaps es1 es2
  {-# INLINE basicUnsafeNew  #-}
  basicUnsafeNew n_
      = do
          as <- M.basicUnsafeNew n_
          bs <- M.basicUnsafeNew n_
          cs <- M.basicUnsafeNew n_
          ds <- M.basicUnsafeNew n_
          es <- M.basicUnsafeNew n_
          return $ MV_5 n_ as bs cs ds es
  {-# INLINE basicUnsafeReplicate  #-}
  basicUnsafeReplicate n_ (a, b, c, d, e)
      = do
          as <- M.basicUnsafeReplicate n_ a
          bs <- M.basicUnsafeReplicate n_ b
          cs <- M.basicUnsafeReplicate n_ c
          ds <- M.basicUnsafeReplicate n_ d
          es <- M.basicUnsafeReplicate n_ e
          return $ MV_5 n_ as bs cs ds es
  {-# INLINE basicUnsafeRead  #-}
  basicUnsafeRead (MV_5 n_ as bs cs ds es) i_
      = do
          a <- M.basicUnsafeRead as i_
          b <- M.basicUnsafeRead bs i_
          c <- M.basicUnsafeRead cs i_
          d <- M.basicUnsafeRead ds i_
          e <- M.basicUnsafeRead es i_
          return (a, b, c, d, e)
  {-# INLINE basicUnsafeWrite  #-}
  basicUnsafeWrite (MV_5 n_ as bs cs ds es) i_ (a, b, c, d, e)
      = do
          M.basicUnsafeWrite as i_ a
          M.basicUnsafeWrite bs i_ b
          M.basicUnsafeWrite cs i_ c
          M.basicUnsafeWrite ds i_ d
          M.basicUnsafeWrite es i_ e
  {-# INLINE basicClear  #-}
  basicClear (MV_5 n_ as bs cs ds es)
      = do
          M.basicClear as
          M.basicClear bs
          M.basicClear cs
          M.basicClear ds
          M.basicClear es
  {-# INLINE basicSet  #-}
  basicSet (MV_5 n_ as bs cs ds es) (a, b, c, d, e)
      = do
          M.basicSet as a
          M.basicSet bs b
          M.basicSet cs c
          M.basicSet ds d
          M.basicSet es e
  {-# INLINE basicUnsafeCopy  #-}
  basicUnsafeCopy (MV_5 n_1 as1 bs1 cs1 ds1 es1) (MV_5 n_2 as2
                                                           bs2
                                                           cs2
                                                           ds2
                                                           es2)
      = do
          M.basicUnsafeCopy as1 as2
          M.basicUnsafeCopy bs1 bs2
          M.basicUnsafeCopy cs1 cs2
          M.basicUnsafeCopy ds1 ds2
          M.basicUnsafeCopy es1 es2
  {-# INLINE basicUnsafeMove  #-}
  basicUnsafeMove (MV_5 n_1 as1 bs1 cs1 ds1 es1) (MV_5 n_2 as2
                                                           bs2
                                                           cs2
                                                           ds2
                                                           es2)
      = do
          M.basicUnsafeMove as1 as2
          M.basicUnsafeMove bs1 bs2
          M.basicUnsafeMove cs1 cs2
          M.basicUnsafeMove ds1 ds2
          M.basicUnsafeMove es1 es2
  {-# INLINE basicUnsafeGrow  #-}
  basicUnsafeGrow (MV_5 n_ as bs cs ds es) m_
      = do
          as' <- M.basicUnsafeGrow as m_
          bs' <- M.basicUnsafeGrow bs m_
          cs' <- M.basicUnsafeGrow cs m_
          ds' <- M.basicUnsafeGrow ds m_
          es' <- M.basicUnsafeGrow es m_
          return $ MV_5 (m_+n_) as' bs' cs' ds' es'
instance (Unbox a,
          Unbox b,
          Unbox c,
          Unbox d,
          Unbox e) => G.Vector Vector (a, b, c, d, e) where
  {-# INLINE basicUnsafeFreeze  #-}
  basicUnsafeFreeze (MV_5 n_ as bs cs ds es)
      = do
          as' <- G.basicUnsafeFreeze as
          bs' <- G.basicUnsafeFreeze bs
          cs' <- G.basicUnsafeFreeze cs
          ds' <- G.basicUnsafeFreeze ds
          es' <- G.basicUnsafeFreeze es
          return $ V_5 n_ as' bs' cs' ds' es'
  {-# INLINE basicUnsafeThaw  #-}
  basicUnsafeThaw (V_5 n_ as bs cs ds es)
      = do
          as' <- G.basicUnsafeThaw as
          bs' <- G.basicUnsafeThaw bs
          cs' <- G.basicUnsafeThaw cs
          ds' <- G.basicUnsafeThaw ds
          es' <- G.basicUnsafeThaw es
          return $ MV_5 n_ as' bs' cs' ds' es'
  {-# INLINE basicLength  #-}
  basicLength (V_5 n_ as bs cs ds es) = n_
  {-# INLINE basicUnsafeSlice  #-}
  basicUnsafeSlice i_ m_ (V_5 n_ as bs cs ds es)
      = V_5 m_ (G.basicUnsafeSlice i_ m_ as)
               (G.basicUnsafeSlice i_ m_ bs)
               (G.basicUnsafeSlice i_ m_ cs)
               (G.basicUnsafeSlice i_ m_ ds)
               (G.basicUnsafeSlice i_ m_ es)
  {-# INLINE basicUnsafeIndexM  #-}
  basicUnsafeIndexM (V_5 n_ as bs cs ds es) i_
      = do
          a <- G.basicUnsafeIndexM as i_
          b <- G.basicUnsafeIndexM bs i_
          c <- G.basicUnsafeIndexM cs i_
          d <- G.basicUnsafeIndexM ds i_
          e <- G.basicUnsafeIndexM es i_
          return (a, b, c, d, e)
  {-# INLINE basicUnsafeCopy  #-}
  basicUnsafeCopy (MV_5 n_1 as1 bs1 cs1 ds1 es1) (V_5 n_2 as2
                                                          bs2
                                                          cs2
                                                          ds2
                                                          es2)
      = do
          G.basicUnsafeCopy as1 as2
          G.basicUnsafeCopy bs1 bs2
          G.basicUnsafeCopy cs1 cs2
          G.basicUnsafeCopy ds1 ds2
          G.basicUnsafeCopy es1 es2
  {-# INLINE elemseq  #-}
  elemseq _ (a, b, c, d, e)
      = G.elemseq (undefined :: Vector a) a
        . G.elemseq (undefined :: Vector b) b
        . G.elemseq (undefined :: Vector c) c
        . G.elemseq (undefined :: Vector d) d
        . G.elemseq (undefined :: Vector e) e
data instance MVector s (a, b, c, d, e, f)
    = MV_6 {-# UNPACK #-} !Int !(MVector s a)
                               !(MVector s b)
                               !(MVector s c)
                               !(MVector s d)
                               !(MVector s e)
                               !(MVector s f)
data instance Vector (a, b, c, d, e, f)
    = V_6 {-# UNPACK #-} !Int !(Vector a)
                              !(Vector b)
                              !(Vector c)
                              !(Vector d)
                              !(Vector e)
                              !(Vector f)
instance (Unbox a,
          Unbox b,
          Unbox c,
          Unbox d,
          Unbox e,
          Unbox f) => Unbox (a, b, c, d, e, f)
instance (Unbox a,
          Unbox b,
          Unbox c,
          Unbox d,
          Unbox e,
          Unbox f) => M.MVector MVector (a, b, c, d, e, f) where
  {-# INLINE basicLength  #-}
  basicLength (MV_6 n_ as bs cs ds es fs) = n_
  {-# INLINE basicUnsafeSlice  #-}
  basicUnsafeSlice i_ m_ (MV_6 n_ as bs cs ds es fs)
      = MV_6 m_ (M.basicUnsafeSlice i_ m_ as)
                (M.basicUnsafeSlice i_ m_ bs)
                (M.basicUnsafeSlice i_ m_ cs)
                (M.basicUnsafeSlice i_ m_ ds)
                (M.basicUnsafeSlice i_ m_ es)
                (M.basicUnsafeSlice i_ m_ fs)
  {-# INLINE basicOverlaps  #-}
  basicOverlaps (MV_6 n_1 as1 bs1 cs1 ds1 es1 fs1) (MV_6 n_2 as2
                                                             bs2
                                                             cs2
                                                             ds2
                                                             es2
                                                             fs2)
      = M.basicOverlaps as1 as2
        || M.basicOverlaps bs1 bs2
        || M.basicOverlaps cs1 cs2
        || M.basicOverlaps ds1 ds2
        || M.basicOverlaps es1 es2
        || M.basicOverlaps fs1 fs2
  {-# INLINE basicUnsafeNew  #-}
  basicUnsafeNew n_
      = do
          as <- M.basicUnsafeNew n_
          bs <- M.basicUnsafeNew n_
          cs <- M.basicUnsafeNew n_
          ds <- M.basicUnsafeNew n_
          es <- M.basicUnsafeNew n_
          fs <- M.basicUnsafeNew n_
          return $ MV_6 n_ as bs cs ds es fs
  {-# INLINE basicUnsafeReplicate  #-}
  basicUnsafeReplicate n_ (a, b, c, d, e, f)
      = do
          as <- M.basicUnsafeReplicate n_ a
          bs <- M.basicUnsafeReplicate n_ b
          cs <- M.basicUnsafeReplicate n_ c
          ds <- M.basicUnsafeReplicate n_ d
          es <- M.basicUnsafeReplicate n_ e
          fs <- M.basicUnsafeReplicate n_ f
          return $ MV_6 n_ as bs cs ds es fs
  {-# INLINE basicUnsafeRead  #-}
  basicUnsafeRead (MV_6 n_ as bs cs ds es fs) i_
      = do
          a <- M.basicUnsafeRead as i_
          b <- M.basicUnsafeRead bs i_
          c <- M.basicUnsafeRead cs i_
          d <- M.basicUnsafeRead ds i_
          e <- M.basicUnsafeRead es i_
          f <- M.basicUnsafeRead fs i_
          return (a, b, c, d, e, f)
  {-# INLINE basicUnsafeWrite  #-}
  basicUnsafeWrite (MV_6 n_ as bs cs ds es fs) i_ (a, b, c, d, e, f)
      = do
          M.basicUnsafeWrite as i_ a
          M.basicUnsafeWrite bs i_ b
          M.basicUnsafeWrite cs i_ c
          M.basicUnsafeWrite ds i_ d
          M.basicUnsafeWrite es i_ e
          M.basicUnsafeWrite fs i_ f
  {-# INLINE basicClear  #-}
  basicClear (MV_6 n_ as bs cs ds es fs)
      = do
          M.basicClear as
          M.basicClear bs
          M.basicClear cs
          M.basicClear ds
          M.basicClear es
          M.basicClear fs
  {-# INLINE basicSet  #-}
  basicSet (MV_6 n_ as bs cs ds es fs) (a, b, c, d, e, f)
      = do
          M.basicSet as a
          M.basicSet bs b
          M.basicSet cs c
          M.basicSet ds d
          M.basicSet es e
          M.basicSet fs f
  {-# INLINE basicUnsafeCopy  #-}
  basicUnsafeCopy (MV_6 n_1 as1 bs1 cs1 ds1 es1 fs1) (MV_6 n_2 as2
                                                               bs2
                                                               cs2
                                                               ds2
                                                               es2
                                                               fs2)
      = do
          M.basicUnsafeCopy as1 as2
          M.basicUnsafeCopy bs1 bs2
          M.basicUnsafeCopy cs1 cs2
          M.basicUnsafeCopy ds1 ds2
          M.basicUnsafeCopy es1 es2
          M.basicUnsafeCopy fs1 fs2
  {-# INLINE basicUnsafeMove  #-}
  basicUnsafeMove (MV_6 n_1 as1 bs1 cs1 ds1 es1 fs1) (MV_6 n_2 as2
                                                               bs2
                                                               cs2
                                                               ds2
                                                               es2
                                                               fs2)
      = do
          M.basicUnsafeMove as1 as2
          M.basicUnsafeMove bs1 bs2
          M.basicUnsafeMove cs1 cs2
          M.basicUnsafeMove ds1 ds2
          M.basicUnsafeMove es1 es2
          M.basicUnsafeMove fs1 fs2
  {-# INLINE basicUnsafeGrow  #-}
  basicUnsafeGrow (MV_6 n_ as bs cs ds es fs) m_
      = do
          as' <- M.basicUnsafeGrow as m_
          bs' <- M.basicUnsafeGrow bs m_
          cs' <- M.basicUnsafeGrow cs m_
          ds' <- M.basicUnsafeGrow ds m_
          es' <- M.basicUnsafeGrow es m_
          fs' <- M.basicUnsafeGrow fs m_
          return $ MV_6 (m_+n_) as' bs' cs' ds' es' fs'
instance (Unbox a,
          Unbox b,
          Unbox c,
          Unbox d,
          Unbox e,
          Unbox f) => G.Vector Vector (a, b, c, d, e, f) where
  {-# INLINE basicUnsafeFreeze  #-}
  basicUnsafeFreeze (MV_6 n_ as bs cs ds es fs)
      = do
          as' <- G.basicUnsafeFreeze as
          bs' <- G.basicUnsafeFreeze bs
          cs' <- G.basicUnsafeFreeze cs
          ds' <- G.basicUnsafeFreeze ds
          es' <- G.basicUnsafeFreeze es
          fs' <- G.basicUnsafeFreeze fs
          return $ V_6 n_ as' bs' cs' ds' es' fs'
  {-# INLINE basicUnsafeThaw  #-}
  basicUnsafeThaw (V_6 n_ as bs cs ds es fs)
      = do
          as' <- G.basicUnsafeThaw as
          bs' <- G.basicUnsafeThaw bs
          cs' <- G.basicUnsafeThaw cs
          ds' <- G.basicUnsafeThaw ds
          es' <- G.basicUnsafeThaw es
          fs' <- G.basicUnsafeThaw fs
          return $ MV_6 n_ as' bs' cs' ds' es' fs'
  {-# INLINE basicLength  #-}
  basicLength (V_6 n_ as bs cs ds es fs) = n_
  {-# INLINE basicUnsafeSlice  #-}
  basicUnsafeSlice i_ m_ (V_6 n_ as bs cs ds es fs)
      = V_6 m_ (G.basicUnsafeSlice i_ m_ as)
               (G.basicUnsafeSlice i_ m_ bs)
               (G.basicUnsafeSlice i_ m_ cs)
               (G.basicUnsafeSlice i_ m_ ds)
               (G.basicUnsafeSlice i_ m_ es)
               (G.basicUnsafeSlice i_ m_ fs)
  {-# INLINE basicUnsafeIndexM  #-}
  basicUnsafeIndexM (V_6 n_ as bs cs ds es fs) i_
      = do
          a <- G.basicUnsafeIndexM as i_
          b <- G.basicUnsafeIndexM bs i_
          c <- G.basicUnsafeIndexM cs i_
          d <- G.basicUnsafeIndexM ds i_
          e <- G.basicUnsafeIndexM es i_
          f <- G.basicUnsafeIndexM fs i_
          return (a, b, c, d, e, f)
  {-# INLINE basicUnsafeCopy  #-}
  basicUnsafeCopy (MV_6 n_1 as1 bs1 cs1 ds1 es1 fs1) (V_6 n_2 as2
                                                              bs2
                                                              cs2
                                                              ds2
                                                              es2
                                                              fs2)
      = do
          G.basicUnsafeCopy as1 as2
          G.basicUnsafeCopy bs1 bs2
          G.basicUnsafeCopy cs1 cs2
          G.basicUnsafeCopy ds1 ds2
          G.basicUnsafeCopy es1 es2
          G.basicUnsafeCopy fs1 fs2
  {-# INLINE elemseq  #-}
  elemseq _ (a, b, c, d, e, f)
      = G.elemseq (undefined :: Vector a) a
        . G.elemseq (undefined :: Vector b) b
        . G.elemseq (undefined :: Vector c) c
        . G.elemseq (undefined :: Vector d) d
        . G.elemseq (undefined :: Vector e) e
        . G.elemseq (undefined :: Vector f) f

