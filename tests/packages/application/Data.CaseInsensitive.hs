{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Data/CaseInsensitive.hs" #-}


























































{-# LANGUAGE CPP, NoImplicitPrelude #-}

{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.CaseInsensitive
-- Copyright   :  (c) 2011-2013 Bas van Dijk
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module is intended to be imported qualified. May I suggest:
--
-- @
-- import           Data.CaseInsensitive  ( CI )
-- import qualified Data.CaseInsensitive as CI
-- @
--
-- /Note that the FoldCase instance for ByteStrings is only/
-- /guaranteed to be correct for ISO-8859-1 encoded strings!/
--
-----------------------------------------------------------------------------

module Data.CaseInsensitive ( CI
                            , mk
                            , original
                            , foldedCase
                            , map
                            , FoldCase(foldCase)
                            ) where
import Data.CaseInsensitive.Internal
