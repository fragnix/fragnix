{-# LANGUAGE OverloadedStrings #-}
module Fragnix.DeclarationSlices where

import Fragnix.Declaration (
    Declaration(Declaration),Genre(TypeSignature,ClassInstance,InfixFixity))
import Fragnix.Slice (
    Slice(Slice),SliceID,Language(Language),Fragment(Fragment),Usage(Usage),UsedName(..),
    Reference(Primitive,OtherSlice))
import Fragnix.GlobalScope (GlobalScope)
import Fragnix.Primitive (primitiveModules)

import Language.Haskell.Names (
    Symbol(Constructor,Value,Method,Selector,symbolModule,symbolName))
import qualified Language.Haskell.Exts as Name (
    Name(Ident,Symbol))
import Language.Haskell.Exts (
    ModuleName,prettyExtension,Name,prettyPrint,
    Extension(EnableExtension),KnownExtension(Safe))

import Data.Graph.Inductive (
    buildGr,scc,lab,lsuc,labNodes,insEdges,insNodes,empty)
import Data.Graph.Inductive.PatriciaTree (
    Gr)

import Control.Monad (guard)
import Data.Text (pack)
import Data.Map (Map)
import qualified Data.Map as Map (lookup,fromList)
import Data.Maybe (maybeToList,fromJust)
import Data.Hashable (hash)
import Data.List (nub)

declarationSlices :: [Declaration] -> ([Slice],GlobalScope)
declarationSlices declarations = (slices,globalscope) where
    (tempslices,slicebindings) = unzip (buildTempSlices (sccGraph (declarationGraph declarations)))
    slices = hashSlices tempslices
    globalscope = Map.fromList (do
        (Slice sliceID _ _ _,boundsymbols) <- zip slices slicebindings
        boundsymbol <- boundsymbols
        return (boundsymbol,sliceID))

declarationGraph :: [Declaration] -> Gr Declaration Dependency
declarationGraph declarations =
    insEdges (signatureedges ++ usedsymboledges ++ instanceEdges ++ fixityEdges) (
        insNodes declarationnodes empty) where
    declarationnodes = zip [0..] declarations
    boundmap = Map.fromList (do
        (node,declaration) <- declarationnodes
        let Declaration _ _ _ boundsymbols _ = declaration
        boundsymbol <- boundsymbols
        return (boundsymbol,node))
    usedsymboledges = do
        (node,declaration) <- declarationnodes
        let Declaration _ _ _ _ mentionedsymbols = declaration
        (mentionedsymbol,maybequalification) <- mentionedsymbols
        usednode <- maybeToList (Map.lookup mentionedsymbol boundmap)
        return (node,usednode,UsesSymbol maybequalification mentionedsymbol)
    signatureedges = do
        (signaturenode,Declaration TypeSignature _ _ _ mentionedsymbols) <- declarationnodes
        mentionedsymbol@(Value _ _) <- map fst mentionedsymbols
        declarationnode <- maybeToList (Map.lookup mentionedsymbol boundmap)
        return (declarationnode,signaturenode,Signature)
    instanceEdges = do
        (instancenode,Declaration ClassInstance _ _ _ instancesymbols) <- declarationnodes
        (method@(Method _ _ _),maybequalification) <- instancesymbols
        (declarationnode,Declaration _ _ _ _ declarationsymbols) <- declarationnodes
        guard (method `elem` (map fst declarationsymbols))
        return (declarationnode,instancenode,UsesInstance maybequalification)
    fixityEdges = do
        (fixitynode,Declaration InfixFixity _ _ _ mentionedsymbols) <- declarationnodes
        mentionedsymbol <- map fst mentionedsymbols
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
        language = Language (nub (do
            Declaration _ ghcextensions _ _ _ <- declarations
            ghcextension <- ghcextensions
            guard (not (ghcextension == EnableExtension Safe))
            return (pack (prettyExtension ghcextension))))
        fragments = Fragment (do
            Declaration _ _ ast _ _ <- declarations
            return ast)
        usages = nub (primitiveUsages ++ otherSliceUsages ++ instanceUsages)
        primitiveUsages = do
            Declaration _ _ _ _ mentionedsymbols <- declarations
            (symbol,maybequalification) <- mentionedsymbols
            primitivemodule <- primitiveModule symbol
            let maybeQualificationText = fmap (pack . prettyPrint) maybequalification
                primitiveModuleText = pack (prettyPrint primitivemodule)
            return (Usage maybeQualificationText (symbolUsedName symbol) (Primitive primitiveModuleText))
        otherSliceUsages = do
            (otherSliceNodeID,UsesSymbol maybequalification symbol) <- lsuc tempslicegraph node
            let maybeQualificationText = fmap (pack . prettyPrint) maybequalification
            return (Usage maybeQualificationText (symbolUsedName symbol) (OtherSlice (fromIntegral otherSliceNodeID)))
        instanceUsages = do
            (otherSliceNodeID,UsesInstance maybequalification) <- lsuc tempslicegraph node
            let maybeQualificationText = fmap (pack . prettyPrint) maybequalification
            return (Usage maybeQualificationText Instance (OtherSlice (fromIntegral otherSliceNodeID)))
        allboundsymbols = do
            Declaration _ _ _ boundsymbols _ <- declarations
            boundsymbols
    return (Slice tempID language fragments usages,allboundsymbols)

type TempID = Integer

hashSlices :: [Slice] -> [Slice]
hashSlices tempSlices = map (replaceSliceID (computeHash tempSliceMap)) tempSlices where
    tempSliceMap = sliceMap tempSlices

sliceMap :: [Slice] -> Map TempID Slice
sliceMap tempSlices = Map.fromList (do
    tempSlice@(Slice tempSliceID _ _ _) <- tempSlices
    return (tempSliceID,tempSlice))

replaceSliceID :: (TempID -> SliceID) -> Slice -> Slice
replaceSliceID f (Slice tempID language fragment usages) = Slice (f tempID) language fragment (map (replaceUsageID f) usages)

computeHash :: Map TempID Slice -> TempID -> SliceID
computeHash tempSliceMap tempID = abs (fromIntegral (hash (fragment,usages))) where
    Just (Slice _ _ fragment tempUsages) = Map.lookup tempID tempSliceMap
    usages = map (replaceUsageID (computeHash tempSliceMap)) tempUsages

replaceUsageID :: (TempID -> SliceID) -> Usage -> Usage
replaceUsageID f (Usage qualification usedName (OtherSlice tempID)) =
    (Usage qualification usedName (OtherSlice (f tempID)))
replaceUsageID _ usage = usage

primitiveModule :: Symbol -> [ModuleName]
primitiveModule symbol = do
    let modulename = symbolModule symbol
    guard (modulename `elem` primitiveModules)
    return modulename

symbolUsedName :: Symbol -> UsedName
symbolUsedName (Constructor _ constructorname typename) = constructorNameUsed typename constructorname
symbolUsedName symbol
    | isValue symbol = valueNameUsed (symbolName symbol)
    | otherwise = typeNameUsed (symbolName symbol)

isValue :: Symbol -> Bool
isValue symbol = case symbol of
    Value {} -> True
    Method {} -> True
    Selector {} -> True
    Constructor {} -> True
    _ -> False

valueNameUsed :: Name -> UsedName
valueNameUsed (Name.Ident name) = ValueIdentifier (pack name)
valueNameUsed (Name.Symbol name) = ValueOperator (pack name)

typeNameUsed :: Name -> UsedName
typeNameUsed (Name.Ident name) = TypeIdentifier (pack name)
typeNameUsed (Name.Symbol name) = TypeOperator (pack name)

constructorNameUsed :: Name -> Name -> UsedName
constructorNameUsed (Name.Ident typename) (Name.Ident constructorname) =
    ConstructorIdentifier (pack typename) (pack constructorname)
constructorNameUsed (Name.Ident typename) (Name.Symbol constructorname) =
    ConstructorOperator (pack typename) (pack constructorname)
constructorNameUsed (Name.Symbol typename) (Name.Ident constructorname) =
    ConstructorIdentifier (pack typename) (pack constructorname)
constructorNameUsed (Name.Symbol typename) (Name.Symbol constructorname) =
    ConstructorOperator (pack typename) (pack constructorname)

data Dependency =
    UsesSymbol (Maybe ModuleName) Symbol |
    Signature |
    UsesInstance (Maybe ModuleName) |
    Fixity
        deriving (Eq,Ord,Show)
