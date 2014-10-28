{-# LANGUAGE OverloadedStrings #-}
module Fragnix.DeclarationSlices where

import Fragnix.Declaration (
    Declaration(Declaration),Genre(TypeSignature,ClassInstance,InfixFixity))
import Fragnix.Slice (
    Slice(Slice),SliceID,Fragment(Fragment),Usage(Usage),UsedName(..),
    Reference(Primitive,OtherSlice),OriginalModule)
import Fragnix.GlobalScope (GlobalScope)
import Fragnix.Symbol (Symbol(ValueSymbol,TypeSymbol))
import Fragnix.Primitive (primitiveModules)

import Language.Haskell.Names (
    Symbols(Symbols),SymValueInfo(SymConstructor),SymTypeInfo(SymClass),
    OrigName,sv_origName,st_origName,origGName,gName,gModule,ModuleNameS)
import qualified Language.Haskell.Exts.Annotated as Name (
    Name(Ident,Symbol))
import Language.Haskell.Names.SyntaxUtils (stringToName)

import Data.Graph.Inductive (
    buildGr,scc,lab,lsuc,labNodes,insEdges,insNodes,empty)
import Data.Graph.Inductive.PatriciaTree (
    Gr)

import Control.Monad (guard)
import Data.Text (pack,unpack)
import Data.Map (Map)
import qualified Data.Map as Map (lookup,fromList)
import qualified Data.Set as Set (toList)
import Data.Maybe (maybeToList,fromJust)
import Data.Hashable (hash)
import Data.List (nub)

declarationSlices :: [Declaration] -> ([Slice],GlobalScope)
declarationSlices declarations = (slices,globalscope) where
    (tempslices,slicebindings) = unzip (buildTempSlices (sccGraph (declarationGraph declarations)))
    slices = hashSlices tempslices
    globalscope = Map.fromList (do
        (Slice sliceID _ _,boundsymbols) <- zip slices slicebindings
        boundsymbol <- boundsymbols
        return (boundsymbol,sliceID))

declarationGraph :: [Declaration] -> Gr Declaration Dependency
declarationGraph declarations =
    insEdges (signatureedges ++ usedsymboledges ++ instanceEdges ++ fixityEdges) (
        insNodes declarationnodes empty) where
    declarationnodes = zip [0..] declarations
    boundmap = Map.fromList (do
        (node,declaration) <- declarationnodes
        let Declaration _ _ boundsymbols _ = declaration
        boundsymbol <- listSymbols boundsymbols
        return (boundsymbol,node))
    usedsymboledges = do
        (node,declaration) <- declarationnodes
        let Declaration _ _ _ mentionedsymbols = declaration
        (maybequalification,mentionedsymbol) <- mentionedsymbols
        usednode <- maybeToList (Map.lookup mentionedsymbol boundmap)
        return (node,usednode,UsesSymbol maybequalification mentionedsymbol)
    signatureedges = do
        (signaturenode,Declaration TypeSignature _ _ mentionedsymbols) <- declarationnodes
        mentionedsymbol@(ValueSymbol _) <- map snd mentionedsymbols
        declarationnode <- maybeToList (Map.lookup mentionedsymbol boundmap)
        return (declarationnode,signaturenode,Signature)
    instanceEdges = do
        (instancenode,Declaration ClassInstance _ _ mentionedsymbols) <- declarationnodes
        classsymbol@(TypeSymbol (SymClass _ _)) <- map snd mentionedsymbols
        classnode <- maybeToList (Map.lookup classsymbol boundmap)
        return (classnode,instancenode,Instance)
    fixityEdges = do
        (fixitynode,Declaration InfixFixity _ _ mentionedsymbols) <- declarationnodes
        mentionedsymbol <- map snd mentionedsymbols
        bindingnode <- maybeToList (Map.lookup mentionedsymbol boundmap)
        return (bindingnode,fixitynode,Fixity)

sccGraph :: Gr a b -> Gr [a] b
sccGraph graph = buildGr (do
    let sccnodes = zip [0..] (scc graph)
        sccmap = Map.fromList (do
            (sccnode,graphnodes) <- sccnodes
            graphnode <- graphnodes
            return (graphnode,sccnode))
    (sccnode,graphnodes) <- sccnodes
    let scclabels = map (fromJust . lab graph) graphnodes
        sccsucs = do
            graphnode <- graphnodes
            (graphsuc,label) <- lsuc graph graphnode
            let sccsuc = fromJust (Map.lookup graphsuc sccmap)
            guard (not (sccsuc == sccnode))
            return (label,sccsuc)
    return ([],sccnode,scclabels,sccsucs))

buildTempSlices :: Gr [Declaration] Dependency -> [(Slice,[Symbol])]
buildTempSlices tempslicegraph = do
    (node,declarations) <- labNodes tempslicegraph
    let tempID = fromIntegral node
        fragments = Fragment (do
            Declaration _ ast _ _ <- declarations
            return ast)
        usages = nub (primitiveUsages ++ otherSliceUsages)
        primitiveUsages = do
            Declaration _ _ _ mentionedsymbols <- declarations
            (maybequalification,symbol) <- mentionedsymbols
            guard (isPrimitive symbol)
            return (Usage (fmap pack maybequalification) (symbolName symbol) (Primitive (originalModule symbol)))
        otherSliceUsages = do
            (otherSliceNodeID,UsesSymbol maybequalification symbol) <- lsuc tempslicegraph node
            return (Usage (fmap pack maybequalification) (symbolName symbol) (OtherSlice (fromIntegral otherSliceNodeID)))
        allboundsymbols = do
            Declaration _ _ boundsymbols _ <- declarations
            listSymbols boundsymbols
    return (Slice tempID fragments usages,allboundsymbols)

type TempID = Integer

hashSlices :: [Slice] -> [Slice]
hashSlices tempSlices = map (replaceSliceID (computeHash tempSliceMap)) tempSlices where
    tempSliceMap = sliceMap tempSlices

sliceMap :: [Slice] -> Map TempID Slice
sliceMap tempSlices = Map.fromList (do
    tempSlice@(Slice tempSliceID _ _) <- tempSlices
    return (tempSliceID,tempSlice))

replaceSliceID :: (TempID -> SliceID) -> Slice -> Slice
replaceSliceID f (Slice tempID fragment usages) = Slice (f tempID) fragment (map (replaceUsageID f) usages)

computeHash :: Map TempID Slice -> TempID -> SliceID
computeHash tempSliceMap tempID = abs (fromIntegral (hash (fragment,usages))) where
    Just (Slice _ fragment tempUsages) = Map.lookup tempID tempSliceMap
    usages = map (replaceUsageID (computeHash tempSliceMap)) tempUsages

replaceUsageID :: (TempID -> SliceID) -> Usage -> Usage
replaceUsageID f (Usage qualification usedName (OtherSlice tempID)) =
    (Usage qualification usedName (OtherSlice (f tempID)))
replaceUsageID _ usage = usage

isPrimitive :: Symbol -> Bool
isPrimitive symbol = unpack (originalModule symbol) `elem` primitiveModules

listSymbols :: Symbols -> [Symbol]
listSymbols (Symbols valueSymbolSet typeSymbolSet) = valueSymbols ++ typeSymbols where
    valueSymbols = map ValueSymbol (Set.toList valueSymbolSet)
    typeSymbols = map TypeSymbol (Set.toList typeSymbolSet)

symbolName :: Symbol -> UsedName
symbolName (ValueSymbol (SymConstructor origname _ typename)) =
    constructorNameUsed (gName (origGName typename)) (gName (origGName origname))
symbolName (ValueSymbol valueSymbol) =
    valueNameUsed (gName (origGName (sv_origName valueSymbol)))
symbolName (TypeSymbol typeSymbol) =
    typeNameUsed (gName (origGName (st_origName typeSymbol)))

valueNameUsed :: String -> UsedName
valueNameUsed valuename = case stringToName valuename of
    Name.Ident _ name -> ValueIdentifier (pack name)
    Name.Symbol _ name -> ValueOperator (pack name)

typeNameUsed :: String -> UsedName
typeNameUsed typename = case stringToName typename of
    Name.Ident _ name -> TypeIdentifier (pack name)
    Name.Symbol _ name -> TypeOperator (pack name)

constructorNameUsed :: String -> String -> UsedName
constructorNameUsed typename constructorname = case constructorname of
    (':':_) -> ConstructorOperator (pack typename) (pack constructorname)
    _ -> ConstructorIdentifier (pack typename) (pack constructorname)

originalModule :: Symbol -> OriginalModule
originalModule (ValueSymbol valuesymbol) = pack (gModule (origGName (sv_origName valuesymbol)))
originalModule (TypeSymbol typesymbol) = pack (gModule (origGName (st_origName typesymbol)))

data Dependency =
    UsesSymbol (Maybe ModuleNameS) Symbol |
    Signature |
    Instance |
    Fixity
        deriving (Eq,Ord,Show)
