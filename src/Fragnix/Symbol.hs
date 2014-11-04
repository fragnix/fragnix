{-# LANGUAGE OverloadedStrings #-}
module Fragnix.Symbol where

import Language.Haskell.Names (
    SymValueInfo(SymValue),SymTypeInfo,OrigName(OrigName),GName(GName))
import Language.Haskell.Names.Interfaces ()

import Data.Aeson (
    ToJSON(toJSON),object,(.=),
    FromJSON(parseJSON),withObject,(.:))

import Control.Applicative ((<|>),(<$>),(<*>))

data Symbol =
    ValueSymbol (SymValueInfo OrigName) |
    TypeSymbol (SymTypeInfo OrigName) |
    InstanceSymbol Symbol Symbol
        deriving (Eq,Ord,Show)

instance ToJSON Symbol where
    toJSON (ValueSymbol valuesymbol) = toJSON valuesymbol
    toJSON (TypeSymbol typesymbol) = toJSON typesymbol
    toJSON (InstanceSymbol instanceclass instancetype) = object [
        "instanceclass" .= instanceclass,
        "instancetype" .= instancetype]

instance FromJSON Symbol where
    parseJSON v =
        fmap ValueSymbol (parseJSON v) <|>
        fmap TypeSymbol (parseJSON v) <|>
        (withObject "instance object" (\o -> do
            InstanceSymbol <$> o .: "instanceclass" <*> o .: "instancetype") v)

mainsymbol :: Symbol
mainsymbol = ValueSymbol (SymValue (OrigName Nothing (GName "Main" "main")) Nothing)
