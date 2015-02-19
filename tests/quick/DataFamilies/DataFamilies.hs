{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
module DataFamilies where

data family Vector a 

newtype instance Vector Bool = V_Bool [Bool]

f :: Vector Bool -> ()
f (V_Bool v) = ()

class GVector v a where
    basicLength :: v a -> Int

instance GVector Vector Bool where
    basicLength (V_Bool v) = length v
