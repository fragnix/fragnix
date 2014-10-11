module Fragnix.DeclarationSlices where

import Fragnix.Declaration (Declaration)
import Fragnix.Slice (Slice)

import Language.Haskell.Names (
    SymValueInfo,SymTypeInfo,OrigName)

import Data.Graph.Inductive (Node,Context)

import Data.Map (Map)
import qualified Data.Map as Map (lookup,fromList)

declarationSlices :: [Declaration] -> [Slice]
declarationSlices = undefined

contexts :: [(Node,Declaration)] -> [Context Declaration Symbol]
contexts declarationnodes = do
    (node,declaration) <- declarationnodes
    let useddeclarations = do
           mentionedsymbol <- undefined
           useddeclaration <- maybe [] (:[]) (Map.lookup mentionedsymbol boundMap)
           return (mentionedsymbol,useddeclaration)
    return ([],node,declaration,useddeclarations) where
        boundMap = Map.fromList undefined

data Symbol =
    ValueSymbol (SymValueInfo OrigName) |
    TypeSymbol (SymTypeInfo OrigName)
        deriving (Eq,Ord,Show)
