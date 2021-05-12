{-# LANGUAGE NamedFieldPuns #-}
module Fragnix.Basket where

import Prelude hiding (readFile, writeFile)

import Fragnix.Core.Basket (Basket)
import Fragnix.Core.Loaf (Loaf (..), LoafID)
import Fragnix.Loaf (loavesToEnv, pickLoaf)

import Control.Monad (forM)
import Data.Functor ((<&>))
import qualified Data.Map as Map (toList, insertWith)
import Language.Haskell.Names (Environment)

basketToEnvironment :: Basket -> IO Environment
basketToEnvironment b = basketToLoaves b <&> loavesToEnv

basketToLoaves :: Basket -> IO [Loaf]
basketToLoaves b = forM (Map.toList b) pickLoaf

basketToLoafIDs :: Basket -> [LoafID]
basketToLoafIDs b = fmap (head . snd) (Map.toList b)

insertLoaf :: Loaf -> Basket -> Basket
insertLoaf Loaf{name, loafID} = Map.insertWith (++) name [loafID]