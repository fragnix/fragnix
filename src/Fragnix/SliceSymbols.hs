module Fragnix.SliceSymbols where

import Fragnix.Slice (SliceID)
import Fragnix.Environment (Environment)

import Language.Haskell.Names (
    Symbol(symbolName,symbolModule))
import Language.Haskell.Names.Interfaces (
    writeInterface)
import Language.Haskell.Exts (
    ModuleName(ModuleName),Name(Ident))

import Data.Map (
    Map)
import qualified Data.Map as Map (
    toList,lookup,map,fromListWith)
import System.Directory (
    createDirectoryIfMissing)
import System.FilePath (
    (</>),dropFileName)
import Control.Monad (
    forM_,guard)

writeSliceSymbols :: Map Symbol SliceID -> IO ()
writeSliceSymbols symbolSlices = do
    let sliceSymbols = Map.fromListWith (++) (do
            (symbol,sliceID) <- Map.toList symbolSlices
            return (sliceID,[symbol]))
    forM_ (Map.toList sliceSymbols) (\(sliceID,symbols) -> do
        let path = sliceSymbolPath sliceID
        createDirectoryIfMissing True (dropFileName path)
        writeInterface path symbols)

sliceSymbolPath :: SliceID -> FilePath
sliceSymbolPath sliceID = "fragnix" </> "names" </> show sliceID

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
