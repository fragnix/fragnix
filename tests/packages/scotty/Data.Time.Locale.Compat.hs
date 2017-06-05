{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "src/Data/Time/Locale/Compat.hs" #-}

















































{-# LANGUAGE CPP #-}

-- |
-- Module      : Data.Time.Locale.Compat
-- Copyright   : 2014 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides compatibility module name
-- for TimeLocale of old-locale or time-1.5.
module Data.Time.Locale.Compat (
  -- * Time locale interface names
  TimeLocale,
  defaultTimeLocale,
  -- * Date format interface names
  iso8601DateFormat,
  rfc822DateFormat,
  ) where

import Data.Time.Format (TimeLocale, defaultTimeLocale, iso8601DateFormat, rfc822DateFormat)
