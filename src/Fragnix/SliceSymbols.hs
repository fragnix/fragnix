module Fragnix.SliceSymbols
  ( updateEnvironment
  , findMainSliceIDs
  , lookupLocalIDs
  , slicesToSymbols
  ) where

import Fragnix.Core.Slice (SliceID)
import Fragnix.LocalSlice (LocalSliceID)
import Fragnix.Paths (slicesPath)
import Fragnix.Slice (loadSlicesTransitive, sliceIDModuleName)
import Fragnix.SliceCompiler (sliceModuleOptimized)
import Fragnix.SliceInstanceOptimization (sliceInstancesOptimized)

import Language.Haskell.Exts (ModuleName (ModuleName), Name (Ident))
import Language.Haskell.Names
    (Environment, Symbol (symbolModule, symbolName), resolve)

import Control.Monad (guard)
import Data.Map (Map)
import qualified Data.Map as Map (empty, lookup, map, toList, (!))


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
    Just sliceID -> symbol { symbolModule = ModuleName () (sliceIDModuleName sliceID) }

-- | Look up the slice IDs for all symbols in the given Map.
lookupLocalIDs :: Map Symbol LocalSliceID -> Map LocalSliceID SliceID -> Map Symbol SliceID
lookupLocalIDs symbolLocalIDs slices =
    Map.map (\localSliceID -> case Map.lookup localSliceID slices of
        Just sliceID -> sliceID
        Nothing      -> error "symbol's local slice id not found") symbolLocalIDs


slicesToSymbols :: [SliceID] -> IO [Symbol]
slicesToSymbols sliceIDs = do
    slices <- loadSlicesTransitive slicesPath sliceIDs
    let sliceInstanceMap = sliceInstancesOptimized slices
    let sliceModules = map (sliceModuleOptimized sliceInstanceMap) slices
    let environment' = resolve sliceModules Map.empty
    let symbols = fmap (\sliceID -> environment' Map.! ModuleName () (sliceIDModuleName sliceID)) sliceIDs
    return $ concat symbols
