module Fragnix.Basket where

import Prelude hiding (readFile, writeFile)

import Fragnix.Core.Basket (Basket)
import Fragnix.Loaf (loafToSymbols, pickLoaf, symbolsToLoaf)

import Control.Monad (forM)
import qualified Data.Map as Map (empty, foldrWithKey, fromList, insert, toList)
import Data.Text (pack, unpack)
import Language.Haskell.Exts.Syntax (ModuleName (..))
import Language.Haskell.Names (Environment, Symbol)


basketToEnvironment :: Basket -> IO Environment
basketToEnvironment b = do
  envList <- forM (Map.toList b) (\(name, loaves) -> do
    loaf <- pickLoaf loaves
    symbols <- loafToSymbols loaf
    return (ModuleName () (unpack name), symbols))
  return $ Map.fromList envList

environmentToBasket :: Environment -> Basket
environmentToBasket = Map.foldrWithKey convertToBasket Map.empty

convertToBasket :: ModuleName () -> [Symbol] -> Basket -> Basket
convertToBasket (ModuleName () name) symbols = undefined
