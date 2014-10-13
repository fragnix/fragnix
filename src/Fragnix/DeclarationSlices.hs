module Fragnix.DeclarationSlices where

import Fragnix.Declaration (Declaration(Declaration))
import Fragnix.Slice (
    Slice(Slice),Fragment(Fragment),Usage(Usage),UsedName,
    Reference(Primitive,OtherSlice))

import Language.Haskell.Names (
    SymValueInfo,SymTypeInfo,OrigName,Symbols)

import Data.Graph.Inductive (Node,Context,buildGr,scc,lsuc,labNodes)
import Data.Graph.Inductive.PatriciaTree (Gr)

import Data.Text (pack)
import qualified Data.Map as Map (lookup,fromList)
import Data.Maybe (maybeToList)

declarationSlices :: [Declaration] -> [Slice]
declarationSlices declarations = buildSlices (sccGraph declarationgraph (scc declarationgraph)) where
    declarationgraph = buildGr (contexts (zip [0..] declarations)) :: Gr Declaration Symbol

contexts :: [(Node,Declaration)] -> [Context Declaration Symbol]
contexts declarationnodes = do
    (node,declaration) <- declarationnodes
    let useddeclarations = do
            mentionedsymbol <- undefined
            useddeclaration <- maybeToList (Map.lookup mentionedsymbol boundMap)
            return (mentionedsymbol,useddeclaration)
    return ([],node,declaration,useddeclarations) where
        boundMap = Map.fromList undefined

sccGraph :: Gr Declaration Symbol -> [[Node]] -> Gr [Declaration] Symbol
sccGraph = undefined

buildSlices :: Gr [Declaration] Symbol -> [Slice]
buildSlices sccgraph = do
    (node,declarations) <- labNodes sccgraph
    let tempID = fromIntegral node
        fragments = Fragment (do
            Declaration _ ast _ _ <- declarations
            return (pack ast))
        usages = do
            Declaration _ _ _ symbols <- declarations
            symbol <- listSymbols symbols
            let usedname = symbolName symbol
                reference = case lookup symbol (map (\(x,y) -> (y,x)) (lsuc sccgraph node)) of
                    Nothing -> Primitive undefined
                    Just othernode -> OtherSlice (fromIntegral othernode)
            return (Usage Nothing usedname reference)
    return (Slice tempID fragments usages)

hashSlices :: [Slice] -> [Slice]
hashSlices = id

listSymbols :: Symbols -> [Symbol]
listSymbols = undefined

symbolName :: Symbol -> UsedName
symbolName = undefined

data Symbol =
    ValueSymbol (SymValueInfo OrigName) |
    TypeSymbol (SymTypeInfo OrigName)
        deriving (Eq,Ord,Show)
