module Fragnix.Symbol where

import Language.Haskell.Names (
    SymValueInfo(SymValue),SymTypeInfo,OrigName(OrigName),GName(GName))
import Language.Haskell.Names.Interfaces ()

import Data.Aeson (ToJSON(toJSON),FromJSON(parseJSON))

import Control.Applicative ((<|>))

data Symbol =
    ValueSymbol (SymValueInfo OrigName) |
    TypeSymbol (SymTypeInfo OrigName)
        deriving (Eq,Ord,Show)

instance ToJSON Symbol where
    toJSON (ValueSymbol valuesymbol) = toJSON valuesymbol
    toJSON (TypeSymbol typesymbol) = toJSON typesymbol

instance FromJSON Symbol where
    parseJSON v =
        fmap ValueSymbol (parseJSON v) <|>
        fmap TypeSymbol (parseJSON v)

mainsymbol :: Symbol
mainsymbol = ValueSymbol (SymValue (OrigName Nothing (GName "Main" "main")) Nothing)
