{-# LINE 1 "./Text/Regex/Base/Impl.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                           






                          






                                 






                                






                       






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Text/Regex/Base/Impl.hs" #-}
{-# LINE 1 "./Text/Regex/Base/Impl.hs" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Regex.Impl
-- Copyright   :  (c) Chris Kuklewicz 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org, textregexlazy@personal.mightyreason.com
-- Stability   :  experimental
-- Portability :  non-portable (Text.Regex.Base needs MPTC+FD)
-- 
-- Helper functions for defining certain instances of
-- RegexContext. These help when defining instances of RegexContext
-- with repeated types:
-- 
-- @
-- instance (RegexLike regex source) => RegexContext regex source source where
-- @
-- 
-- runs into overlapping restrictions. To avoid this I have each backend
-- define, for its own Regex type:
-- 
-- @
-- instance RegexContext Regex String String where
--   match = polymatch
--   matchM = polymatchM
-- @
-- 
-- @
-- instance RegexContext Regex ByteString ByteString where
--   match = polymatch
--   matchM = polymatchM
-- @
-------------------------------------------------------------------------------

module Text.Regex.Base.Impl(polymatch,polymatchM) where

import Text.Regex.Base
import Data.Array((!))

regexFailed :: (Monad m) => m b
{-# INLINE regexFailed #-}
regexFailed =  fail $ "regex failed to match"

actOn :: (RegexLike r s,Monad m) => ((s,MatchText s,s)->t) -> r -> s -> m t
{-# INLINE actOn #-}
actOn f r s = case matchOnceText r s of
    Nothing -> regexFailed
    Just preMApost -> return (f preMApost)

polymatch :: (RegexLike a b) => a -> b -> b
{-# INLINE polymatch #-}
polymatch r s = case matchOnceText r s of
    Nothing -> empty
    Just (_,ma,_) -> fst (ma!0)

polymatchM :: (RegexLike a b,Monad m) => a -> b -> m b
{-# INLINE polymatchM #-}
polymatchM =  actOn (\(_,ma,_)->fst (ma!0))
