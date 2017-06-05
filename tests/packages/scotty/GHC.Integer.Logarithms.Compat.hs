{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "src/GHC/Integer/Logarithms/Compat.hs" #-}























































-- |
-- Module:      GHC.Integer.Logarithms.Compat
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Low level stuff for integer logarithms.
{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}
module GHC.Integer.Logarithms.Compat
    ( -- * Functions
      integerLogBase#
    , integerLog2#
    , wordLog2#
    ) where


-- Stuff is already there
import GHC.Integer.Logarithms

