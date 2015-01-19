{-# LANGUAGE OverloadedStrings #-}
module Fragnix.DeclarationSlices where

import Fragnix.Declaration (
    Declaration(Declaration),Genre(TypeSignature,ClassInstance,InfixFixity))
import Fragnix.Slice (
    Slice(Slice),SliceID,Language(Language),Fragment(Fragment),
    Usage(Usage),UsedName(..),Reference(OtherSlice,Primitive))

import Language.Haskell.Names (
    Symbol(Constructor,Value,Method,Selector,Class,Data,NewType,symbolName,symbolModule))
import qualified Language.Haskell.Exts as Name (
    Name(Ident,Symbol))
import Language.Haskell.Exts (
    ModuleName(ModuleName),prettyExtension,Name,prettyPrint,
    Extension(EnableExtension),KnownExtension(Safe,CPP,Trustworthy))

import Data.Graph.Inductive (
    Node,buildGr,scc,lab,lsuc,labNodes,insEdges,insNodes,empty)
import Data.Graph.Inductive.PatriciaTree (
    Gr)

import Control.Monad (guard)
import Control.Applicative ((<|>))
import Data.Text (pack)
import Data.Char (isDigit)
import Data.Map (Map)
import qualified Data.Map as Map (lookup,fromList,fromListWith)
import qualified Data.Set as Set (fromList,member)
import Data.Maybe (maybeToList,fromJust,listToMaybe)
import Data.Hashable (hash)
import Data.List (nub,(\\))


-- | Extract all slices from the given list of declarations.
declarationSlices :: [Declaration] -> [Slice]
declarationSlices declarations = slices where
    sccs = labNodes (sccGraph (declarationGraph declarations))
    sliceBindingsMap = sliceBindings sccs
    sliceInstancesMap = sliceInstances sccs
    slices = hashSlices (map (buildTempSlice sliceBindingsMap sliceInstancesMap) sccs)


-- | Build a Map from symbol to Node that binds this symbol.
sliceBindings :: [(Node,[Declaration])] -> Map Symbol Node
sliceBindings fragmentNodes = Map.fromList (do
    (node,declarations) <- fragmentNodes
    Declaration _ _ _ boundsymbols _ <- declarations
    boundsymbol <- boundsymbols
    return (boundsymbol,node))


-- | Build a Map from data or class symbol to instance temporary ID
-- Prefer the class symbol if both are present
-- Only include classes and types bound in this graph
sliceInstances :: [(Node,[Declaration])] -> Map Symbol [Node]
sliceInstances fragmentNodes = Map.fromListWith (++) (do

    let graphSymbols = Set.fromList (do
            (_,declarations) <- fragmentNodes
            Declaration _ _ _ boundSymbols _ <- declarations
            boundSymbols)

    (node,declarations) <- fragmentNodes
    Declaration ClassInstance _ _ _ mentionedsymbols <- declarations

    let classSymbol = do
            classSymbolMentionedLast <- listToMaybe (reverse (filter isClass (map fst mentionedsymbols)))
            guard (Set.member classSymbolMentionedLast graphSymbols)
            return classSymbolMentionedLast
        typeSymbol = do
            typeSymbolMentionedFirst <- listToMaybe (filter isType (map fst mentionedsymbols))
            guard (Set.member typeSymbolMentionedFirst graphSymbols)
            return typeSymbolMentionedFirst
    symbol <- maybeToList (classSymbol <|> typeSymbol)

    return (symbol,[node]))


-- | Create a dependency graph between all declarations in the given list. Dependency edges
-- come primarily from when a declaration mentions a symbol another declaration binds. But also
-- from signatures, fixities and instances.
declarationGraph :: [Declaration] -> Gr Declaration Dependency
declarationGraph declarations =
    insEdges (signatureedges ++ usedsymboledges ++ fixityEdges) (
        insNodes declarationnodes empty) where
    declarationnodes = zip [-1,-2..] declarations
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
    fixityEdges = do
        (fixitynode,Declaration InfixFixity _ _ _ mentionedsymbols) <- declarationnodes
        mentionedsymbol <- map fst mentionedsymbols
        bindingnode <- maybeToList (Map.lookup mentionedsymbol boundmap)
        return (bindingnode,fixitynode,Fixity)


-- | Build a graph of strongly connected components from a given graph.
sccGraph :: Gr a b -> Gr [a] b
sccGraph graph = buildGr (do
    let sccnodes = zip [-1,-2..] (scc graph)
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


-- | Take a graph where each node corresponds to a
-- list of declarations that from a strongly connected component. Return a list of slices
-- tripled with the symbols it binds and the mentioned symbols that could not be resolved
-- inside the graph. The slices have negative IDs starting from -1.
buildTempSlice :: Map Symbol Node -> Map Symbol [Node] -> (Node,[Declaration]) -> Slice
buildTempSlice tempEnvironment instanceMap (node,declarations) =
    Slice tempID language fragments (mentionedUsages ++ instanceUsages) where
        tempID = fromIntegral node

        language = Language (nub (do
            Declaration _ ghcextensions _ _ _ <- declarations
            ghcextension <- ghcextensions
            -- disregard the Safe and CPP extensions
            guard (not (ghcextension `elem` map EnableExtension [Safe,Trustworthy,CPP]))
            return (pack (prettyExtension ghcextension))))

        fragments = Fragment (do
            Declaration _ _ ast _ _ <- arrange declarations
            return ast)

        -- A usage for every mentioned symbol
        mentionedUsages = nub (do
            Declaration _ _ _ _ mentionedsymbols <- declarations
            (mentionedsymbol,maybequalification) <- mentionedsymbols

            let maybeQualificationText = fmap (pack . prettyPrint) maybequalification
                usedName = symbolUsedName mentionedsymbol
                -- Look up reference to other slice from this graph
                -- if it fails the symbol must be from the environment
                maybeReference = case Map.lookup mentionedsymbol tempEnvironment of
                    -- If the symbol is from the environment it is either builtin or refers to an existing slice
                    Nothing -> Just (moduleReference (symbolModule (mentionedsymbol)))
                    -- If the symbol is from this fragment we generate no reference
                    Just referenceNode -> if referenceNode == node
                        then Nothing
                        else Just (OtherSlice (fromIntegral referenceNode))
                
            reference <- maybeToList maybeReference

            return (Usage maybeQualificationText usedName reference))

        -- We want every class to import all its instances
        -- For builtin classes we want the data type to import the instances
        instanceUsages = do
            Declaration _ _ _ boundSymbols _ <- declarations
            boundSymbol <- boundSymbols
            instanceSliceTempID <- concat (maybeToList (Map.lookup boundSymbol instanceMap))
            return (Usage Nothing Instance (OtherSlice (fromIntegral instanceSliceTempID)))


-- | Arrange a list of declarations so that the signature is directly above the corresponding
-- binding declaration
arrange :: [Declaration] -> [Declaration]
arrange declarations = arrangements ++ (declarations \\ arrangements) where
    arrangements = nub (concatMap findBinding signatures)
    findBinding signature@(Declaration _ _ _ _ mentionedsymbols) = do
        let bindings = do
                mentionedsymbol@(Value _ _) <- map fst mentionedsymbols
                binding@(Declaration _ _ _ boundsymbols _) <- declarations
                guard (mentionedsymbol `elem` boundsymbols)
                return binding
        [signature] ++ bindings
    signatures = do
        signature@(Declaration TypeSignature _ _ _ _) <- declarations
        return signature


-- | We abuse module names to either refer to builtin modules or to a slice.
-- If the module name refers to a slice it consists entirely of digits.
moduleReference :: ModuleName -> Reference
moduleReference (ModuleName moduleName)
    | all isDigit moduleName = OtherSlice (read moduleName)
    | otherwise = Primitive (pack moduleName)


-- | A temporary ID before slices can be hashed.
type TempID = Integer

-- | Compute the hashes for the given list of slices and return slices where every ID is
-- its hash.
hashSlices :: [Slice] -> [Slice]
hashSlices tempSlices = map (replaceSliceID (computeHash tempSliceMap)) tempSlices where
    tempSliceMap = sliceMap tempSlices

-- | Build up a map from temporary ID to corresponding slice for better lookup.
sliceMap :: [Slice] -> Map TempID Slice
sliceMap tempSlices = Map.fromList (do
    tempSlice@(Slice tempSliceID _ _ _) <- tempSlices
    return (tempSliceID,tempSlice))

-- | Replace every occurence of a temporary ID with the final ID.
replaceSliceID :: (TempID -> SliceID) -> Slice -> Slice
replaceSliceID f (Slice tempID language fragment usages) = Slice (f tempID) language fragment (map (replaceUsageID f) usages)

-- | Hash the slice with the given temporary ID.
computeHash :: Map TempID Slice -> TempID -> SliceID
computeHash tempSliceMap tempID = abs (fromIntegral (hash (fragment,usages,language))) where
    Just (Slice _ language fragment tempUsages) = Map.lookup tempID tempSliceMap
    usages = map (replaceUsageID (computeHash tempSliceMap)) tempUsagesWithoutInstances
    tempUsagesWithoutInstances = do
        usage@(Usage _ usedName _) <- tempUsages
        guard (not (usedName == Instance))
        return usage

replaceUsageID :: (TempID -> SliceID) -> Usage -> Usage
replaceUsageID f (Usage qualification usedName (OtherSlice tempID))
    | tempID < 0 = (Usage qualification usedName (OtherSlice (f tempID)))
replaceUsageID _ usage = usage

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

isClass :: Symbol -> Bool
isClass symbol = case symbol of
    Class {} -> True
    _ -> False

isType :: Symbol -> Bool
isType symbol = case symbol of
    Data {} -> True
    NewType {} -> True
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
    Fixity
        deriving (Eq,Ord,Show)
