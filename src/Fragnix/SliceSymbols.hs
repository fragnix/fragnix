module Fragnix.SliceSymbols
  ( updateEnvironment
  , findMainSliceIDs
  , lookupLocalIDs
  ) where

import Fragnix.Slice (SliceID)
import Fragnix.LocalSlice (LocalSliceID)

import Language.Haskell.Names (
    Environment, Symbol(symbolName,symbolModule))
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
    guard (symbolName symbol == Ident () "main")
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
    Just sliceID -> symbol { symbolModule = ModuleName () ('F' : show sliceID) }

-- | Look up the slice IDs for all symbols in the given Map.
lookupLocalIDs :: Map Symbol LocalSliceID -> Map LocalSliceID SliceID -> Map Symbol SliceID
lookupLocalIDs symbolLocalIDs slices =
    Map.map (\localSliceID -> case Map.lookup localSliceID slices of
        Just sliceID -> sliceID
        Nothing -> error "symbol's local slice id not found") symbolLocalIDs

