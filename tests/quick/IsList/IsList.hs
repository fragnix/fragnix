{-# LANGUAGE TypeFamilies #-}
module IsList where

import qualified GHC.Exts as Exts

data Text = Text

instance Exts.IsList Text where
    type Item Text = Char
    fromList       = const Text
    toList         = const ['a']
