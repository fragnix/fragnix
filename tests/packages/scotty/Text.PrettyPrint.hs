{-# LANGUAGE Haskell98, CPP, BangPatterns, DeriveGeneric #-}
{-# LINE 1 "src/Text/PrettyPrint.hs" #-}




















































{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  David Terei <code@davidterei.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Provides a collection of pretty printer combinators, a set of API's
-- that provides a way to easily print out text in a consistent format
-- of your choosing.
--
-- This module should be used as opposed to the 'Text.PrettyPrint.HughesPJ'
-- module. Both are equivalent though as this module simply re-exports the
-- other.
--
-----------------------------------------------------------------------------

module Text.PrettyPrint ( 

        -- * The document type
        Doc,

        -- * Constructing documents

        -- ** Converting values into documents
        char, text, ptext, sizedText, zeroWidthText,
        int, integer, float, double, rational,

        -- ** Simple derived documents
        semi, comma, colon, space, equals,
        lparen, rparen, lbrack, rbrack, lbrace, rbrace,

        -- ** Wrapping documents in delimiters
        parens, brackets, braces, quotes, doubleQuotes,

        -- ** Combining documents
        empty,
        (<>), (<+>), hcat, hsep,
        ($$), ($+$), vcat,
        sep, cat,
        fsep, fcat,
        nest,
        hang, punctuate,

        -- * Predicates on documents
        isEmpty,

        -- * Rendering documents

        -- ** Default rendering
        render,

        -- ** Rendering with a particular style
        Style(..),
        style,
        renderStyle,

        -- ** General rendering
        fullRender,
        Mode(..), TextDetails(..)

    ) where

import Text.PrettyPrint.HughesPJ

