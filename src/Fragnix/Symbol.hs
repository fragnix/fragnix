module Fragnix.Symbol where

import Language.Haskell.Names (
    SymValueInfo,SymTypeInfo,OrigName)

data Symbol =
    ValueSymbol (SymValueInfo OrigName) |
    TypeSymbol (SymTypeInfo OrigName)
        deriving (Eq,Ord,Show)

