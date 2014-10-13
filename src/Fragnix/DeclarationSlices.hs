module Fragnix.DeclarationSlices where

import Fragnix.Declaration (Declaration(Declaration))
import Fragnix.Slice (
    Slice(Slice),Fragment(Fragment),Usage(Usage),UsedName,
    Reference(Primitive,OtherSlice))

import Language.Haskell.Names (
    SymValueInfo,SymTypeInfo,OrigName,Symbols)

import Data.Graph.Inductive (Node,Context,buildGr,scc,lab,lsuc)
import Data.Graph.Inductive.PatriciaTree (Gr)

import Data.Text (pack)
import qualified Data.Map as Map (lookup,fromList)
import Data.Maybe (maybeToList)

declarationSlices :: [Declaration] -> [Slice]
declarationSlices declarations = map (buildSlice declarationgraph) (scc declarationgraph) where
    declarationgraph = buildGr (contexts (zip [0..] declarations)) :: Gr Declaration Symbol

buildSlice :: Gr Declaration Symbol -> [Node] -> Slice
buildSlice declarationgraph nodes = Slice tempID fragments usages where
    tempID = fromIntegral (head nodes)
    fragments = Fragment (do
        node <- nodes
        Declaration _ ast _ _ <- maybeToList (lab declarationgraph node)
        return (pack ast))
    usages = do
        node <- nodes
        Declaration _ _ _ symbols <- maybeToList (lab declarationgraph node)
        symbol <- listSymbols symbols
        let usedname = symbolName symbol
            reference = case lookup symbol (map (\(x,y) -> (y,x)) (lsuc declarationgraph node)) of
                Nothing -> Primitive undefined
                Just othernode -> OtherSlice (fromIntegral othernode)
        return (Usage Nothing usedname reference)

contexts :: [(Node,Declaration)] -> [Context Declaration Symbol]
contexts declarationnodes = do
    (node,declaration) <- declarationnodes
    let useddeclarations = do
            mentionedsymbol <- undefined
            useddeclaration <- maybeToList (Map.lookup mentionedsymbol boundMap)
            return (mentionedsymbol,useddeclaration)
    return ([],node,declaration,useddeclarations) where
        boundMap = Map.fromList undefined

listSymbols :: Symbols -> [Symbol]
listSymbols = undefined

symbolName :: Symbol -> UsedName
symbolName = undefined

data Symbol =
    ValueSymbol (SymValueInfo OrigName) |
    TypeSymbol (SymTypeInfo OrigName)
        deriving (Eq,Ord,Show)
