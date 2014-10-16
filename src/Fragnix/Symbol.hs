module Fragnix.Symbol where

import Language.Haskell.Names (
    SymValueInfo(SymValue),SymTypeInfo,OrigName(OrigName),GName(GName))

data Symbol =
    ValueSymbol (SymValueInfo OrigName) |
    TypeSymbol (SymTypeInfo OrigName)
        deriving (Eq,Ord,Show)

mainsymbol :: Symbol
mainsymbol = ValueSymbol (SymValue (OrigName Nothing (GName "Main" "main")) Nothing)
