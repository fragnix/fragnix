{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Data/CaseInsensitive/Unsafe.hs" #-}


























































{-# LANGUAGE CPP, NoImplicitPrelude #-}

{-# LANGUAGE Unsafe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.CaseInsensitive.Unsafe
-- Copyright   :  (c) 2011-2013 Bas van Dijk
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- Provides an unsafe way to create a case insensitive string-like value.
--
-----------------------------------------------------------------------------

module Data.CaseInsensitive.Unsafe   ( unsafeMk ) where
import Data.CaseInsensitive.Internal ( unsafeMk )
