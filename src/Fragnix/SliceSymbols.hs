module Fragnix.SliceSymbols where

import Fragnix.Slice (SliceID)
import Fragnix.Environment (Environment)

import Language.Haskell.Names (
    Symbol(symbolName,symbolModule))
import Language.Haskell.Exts (
    ModuleName(ModuleName),Name(Ident))

import Data.Map (
    Map)
import qualified Data.Map as Map (
    toList,lookup,map)
import Control.Monad (
    guard)


-- | Find the possible IDs of the main slice in a map from symbol to slice.
findMainSliceIDs :: Map Symbol SliceID -> [SliceID]
findMainSliceIDs symbolSlices = do
    (symbol,sliceID) <- Map.toList symbolSlices
    guard (symbolName symbol == Ident "main")
    return sliceID

-- | Replace in the given environment the original module of all symbols
-- with the ID of the binding slice.
updateEnvironment :: Map Symbol SliceID -> Environment -> Environment
updateEnvironment symbolSlices environment =
    Map.map (map (updateSymbol symbolSlices)) environment

-- | When the given symbol is in the map replace its original module with
-- the SliceID from the map.
updateSymbol :: Map Symbol SliceID -> Symbol -> Symbol
updateSymbol symbolSlices symbol = case Map.lookup symbol symbolSlices of
    Nothing -> symbol
    Just sliceID -> symbol { symbolModule = ModuleName (show sliceID) }
