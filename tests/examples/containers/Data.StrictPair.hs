{-# LINE 1 "./Data/StrictPair.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                           






                          






                             






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Data/StrictPair.hs" #-}
{-# LINE 1 "./Data/StrictPair.hs" #-}
{-# LANGUAGE CPP #-}

{-# LANGUAGE Trustworthy #-}

module Data.StrictPair (StrictPair(..), toPair) where

-- | Same as regular Haskell pairs, but (x :*: _|_) = (_|_ :*: y) =
-- _|_
data StrictPair a b = !a :*: !b

toPair :: StrictPair a b -> (a, b)
toPair (x :*: y) = (x, y)
{-# INLINE toPair #-}