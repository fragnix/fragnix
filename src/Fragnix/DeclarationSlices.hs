{-# LANGUAGE OverloadedStrings #-}
module Fragnix.DeclarationSlices where

import Fragnix.Declaration (
    Declaration(Declaration),
    Genre(TypeSignature,ClassInstance,InfixFixity,DerivingInstance,FamilyInstance,ForeignImport))
import Fragnix.Slice (
    Slice(Slice),SliceID,Language(Language),Fragment(Fragment),
    Use(Use),UsedName(..),Name(Identifier,Operator),Reference(OtherSlice,Builtin),
    InstanceID)

import Language.Haskell.Names (
    Symbol(Constructor,Value,Method,Selector,Class,Data,NewType,TypeFam,DataFam,
        symbolName,symbolModule))
import qualified Language.Haskell.Exts as Name (
    Name(Ident,Symbol))
import Language.Haskell.Exts (
    ModuleName(ModuleName),prettyExtension,prettyPrint,
    Extension(EnableExtension,UnknownExtension),
    KnownExtension(Safe,CPP,Trustworthy))

import Data.Graph.Inductive (
    scc,lab,insEdges,insNodes,empty)
import Data.Graph.Inductive.PatriciaTree (
    Gr)

import Control.Monad (guard, unless, forM_)
import Control.Monad.Trans.State.Strict (State, execState, get, put)
import Data.Text (pack)
import Data.Char (isDigit)
import Data.Map (Map)
import qualified Data.Map as Map (lookup,fromList,fromListWith,(!),map,keys)
import qualified Data.Set as Set (fromList,member)
import Data.Maybe (maybeToList,fromJust,listToMaybe)
import Data.Hashable (hash)
import Data.List (nub,(\\))


-- | Extract all slices from the given list of declarations. Also return a map
-- from a symbol to the sliceID of the slice that binds it now.
declarationSlices :: [Declaration] -> ([Slice],Map Symbol SliceID)
declarationSlices declarations = (slices,symbolSlices) where

    fragmentNodes = fragmentSCCs (declarationGraph declarations)

    sliceBindingsMap = sliceBindings fragmentNodes
    constructorMap = sliceConstructors fragmentNodes
    classMap = sliceClasses fragmentNodes
    typeMap = sliceTypes fragmentNodes
    instanceMap = classAndTypeInstances fragmentNodes

    tempSlicesWithoutInstances = map (buildTempSlice sliceBindingsMap constructorMap) fragmentNodes
    tempSliceMap = sliceMap tempSlicesWithoutInstances
    tempSlices = map (addInstances classMap typeMap instanceMap tempSliceMap) tempSlicesWithoutInstances
    tempSliceIDMap = sliceIDMap tempSliceMap
    slices = map (replaceSliceID ((Map.!) tempSliceIDMap)) tempSlices
    symbolSlices = Map.map (\tempID -> tempSliceIDMap Map.! tempID) sliceBindingsMap


-- | Build a Map from symbol to temporary ID that binds this symbol.
sliceBindings :: [(TempID,[Declaration])] -> Map Symbol TempID
sliceBindings fragmentNodes = Map.fromList (do
    (tempID,declarations) <- fragmentNodes
    Declaration _ _ _ boundsymbols _ <- declarations
    boundsymbol <- boundsymbols
    return (boundsymbol,tempID))


-- | Create a map from symbol to all its constructors.
sliceConstructors :: [(TempID,[Declaration])] -> Map Symbol [Symbol]
sliceConstructors fragmentNodes = Map.fromListWith (++) (do
    (_,declarations) <- fragmentNodes
    Declaration _ _ _ boundSymbols _ <- declarations
    constructor@(Constructor _ _ typeName) <- boundSymbols
    typeSymbol <- boundSymbols
    guard (isType typeSymbol)
    guard (symbolName typeSymbol == typeName)
    return (typeSymbol,[constructor]))


-- | Create a dependency graph between all declarations in the given list. Dependency edges
-- come primarily from when a declaration mentions a symbol another declaration binds. But also
-- from signatures, fixities and type and data family instances.
declarationGraph :: [Declaration] -> Gr Declaration Dependency
declarationGraph declarations =
    insEdges (signatureedges ++ usedsymboledges ++ fixityEdges ++ familyInstanceEdges) (
        insNodes declarationnodes empty) where

    -- A list of pairs of node ID and declaration
    declarationnodes = zip [-1,-2..] declarations

    -- A map from bound symbol to node ID
    boundmap = Map.fromList (do
        (node,declaration) <- declarationnodes
        let Declaration _ _ _ boundsymbols _ = declaration
        boundsymbol <- boundsymbols
        return (boundsymbol,node))

    -- Dependency edges that come from use of a symbol
    usedsymboledges = do
        (node,declaration) <- declarationnodes
        let Declaration _ _ _ _ mentionedsymbols = declaration
        (mentionedsymbol,maybequalification) <- mentionedsymbols
        usednode <- maybeToList (Map.lookup mentionedsymbol boundmap)
        return (node,usednode,UsesSymbol maybequalification mentionedsymbol)

    -- Each declaration should be in the same comilation unit as its type signature
    signatureedges = do
        (signaturenode,Declaration TypeSignature _ _ _ mentionedsymbols) <- declarationnodes
        mentionedsymbol@(Value _ _) <- map fst mentionedsymbols
        declarationnode <- maybeToList (Map.lookup mentionedsymbol boundmap)
        return (declarationnode,signaturenode,Signature)

    -- Each declaration should be in the same compilation unit as its fixity declarations
    fixityEdges = do
        (fixitynode,Declaration InfixFixity _ _ _ mentionedsymbols) <- declarationnodes
        mentionedsymbol <- map fst mentionedsymbols
        bindingnode <- maybeToList (Map.lookup mentionedsymbol boundmap)
        return (bindingnode,fixitynode,Fixity)

    -- Each data or type family should be in the same compilation unit as its instances
    familyInstanceEdges = do
        (familyInstanceNode,Declaration FamilyInstance _ _ _ mentionedSymbols) <- declarationnodes
        mentionedFamily <- take 1 (filter isFamily (map fst mentionedSymbols))
        familyNode <- maybeToList (Map.lookup mentionedFamily boundmap)
        return (familyNode, familyInstanceNode, Family)


-- | A list of strongly connected components from a given graph.
fragmentSCCs :: Gr a b -> [(TempID,[a])]
fragmentSCCs graph = do
    let sccnodes = zip [-1,-2..] (scc graph)
    (sccnode,graphnodes) <- sccnodes
    let scclabels = map (fromJust . lab graph) graphnodes
    return (sccnode,scclabels)


-- | Take a temporary environment, an instance map, a constructor map and a pair of a node and a fragment.
-- Return a slice with a temporary ID that might contain references to temporary IDs.
-- The nodes must have negative IDs starting from -1 to distinguish temporary from permanent
-- slice IDs.
buildTempSlice :: Map Symbol TempID -> Map Symbol [Symbol] -> (TempID,[Declaration]) -> TempSlice
buildTempSlice tempEnvironment constructorMap (node,declarations) =
    Slice tempID language fragments uses instances where
        tempID = fromIntegral node

        language = Language extensions (any isInstance declarations)

        extensions = (nub (do
            Declaration _ ghcextensions _ _ _ <- declarations
            ghcextension <- ghcextensions
            -- disregard the Safe, Trustworthy, CPP and roles extensions
            guard (not (ghcextension `elem` unwantedExtensions))
            return (pack (prettyExtension ghcextension))))

        fragments = Fragment (do
            Declaration _ _ ast _ _ <- arrange declarations
            return ast)

        uses = nub (mentionedUses ++ implicitConstructorUses)

        -- Instances will be added later.
        instances = []

        -- A use for every mentioned symbol
        mentionedUses = nub (do
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

            return (Use maybeQualificationText usedName reference))

        -- If a declaration needs the constructors of used types we add them here.
        implicitConstructorUses = do
            declaration <- declarations
            guard (needsConstructors declaration)
            let Declaration _ _ _ _ mentionedSymbols = declaration
            mentionedSymbol <- map fst mentionedSymbols
            symbolConstructorUses tempEnvironment constructorMap mentionedSymbol


-- | Some declarations implicitly require the constructors for all mentioned
-- types. The declarations that need this are: standalone deriving, foreign
-- imports and any declarations mentioning coerce.
needsConstructors :: Declaration -> Bool
needsConstructors (Declaration DerivingInstance _ _ _ _) =
    True
needsConstructors (Declaration ForeignImport _ _ _ _) =
    True
needsConstructors (Declaration _ _ _ _ mentionedSymbols) =
    any ((==(Name.Ident "coerce")) . symbolName . fst) mentionedSymbols


-- | Given a Map from type symbol to its constructor symbol and  a symbol
-- this functions returns a list of uses for the given symbols constructors.
-- We have two cases. If the given symbol is builtin and a newtype we return
-- a constructor with the same name and a use of "CInt".
-- If the given symbol is from a slice we look up its constructors and return
-- a use for each of those.
symbolConstructorUses :: Map Symbol TempID -> Map Symbol [Symbol] -> Symbol -> [Use]
symbolConstructorUses tempEnvironment constructorMap symbol =
    case Map.lookup symbol tempEnvironment of
        Nothing -> case moduleReference (symbolModule symbol) of
            builtinReference@(Builtin _) -> case symbol of
                NewType _ symbolname -> do
                    let symbolTypeName = fromName symbolname
                        constructorName = ConstructorName symbolTypeName symbolTypeName
                        cIntName = ConstructorName (Identifier "CInt") (Identifier "CInt")
                        cIntReference = Builtin "Foreign.C.Types"
                        cIntUse = Use Nothing cIntName cIntReference
                    [Use Nothing constructorName builtinReference, cIntUse]
                _ -> []
            _ -> []
        Just referenceNode -> do
            constructorSymbol <- concat (maybeToList (Map.lookup symbol constructorMap))
            return (Use Nothing (symbolUsedName constructorSymbol) (OtherSlice (fromIntegral referenceNode)))


-- | Some extensions are already handled or cannot be handled by us.
unwantedExtensions :: [Extension]
unwantedExtensions =
    map EnableExtension [Safe,Trustworthy,CPP] ++
    [UnknownExtension "RoleAnnotations"]

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
    | otherwise = Builtin (pack moduleName)


-- | A temporary ID before slices can be hashed.
type TempID = Integer

-- | A Slice with a temporary ID that may use slices with
-- temporary IDs.
type TempSlice = Slice


-- | Build up a map from temporary ID to corresponding slice for better lookup.
sliceMap :: [TempSlice] -> Map TempID TempSlice
sliceMap tempSlices = Map.fromList (do
    tempSlice@(Slice tempSliceID _ _ _ _) <- tempSlices
    return (tempSliceID,tempSlice))


-- | Associate every temporary ID with the final ID.
sliceIDMap :: Map TempID TempSlice -> Map TempID SliceID
sliceIDMap tempSliceMap = Map.fromList (do
    tempID <- Map.keys tempSliceMap
    return (tempID,computeHash tempSliceMap tempID))


-- | Assign to each temporary ID the list of class symbols the corresponding
-- temporary slice binds.
sliceClasses :: [(TempID,[Declaration])] -> Map TempID [Symbol]
sliceClasses fragmentNodes = Map.fromListWith (++) (do
    (tempID,declarations) <- fragmentNodes
    Declaration _ _ _ boundSymbols _ <- declarations
    boundSymbol <- boundSymbols
    guard (isClass boundSymbol)
    return (tempID,[boundSymbol]))


-- | Assign to each temporary ID the list of type symbols the corresponding
-- temporary slice binds.
sliceTypes :: [(TempID,[Declaration])] -> Map TempID [Symbol]
sliceTypes fragmentNodes = Map.fromListWith (++) (do
    (tempID,declarations) <- fragmentNodes
    Declaration _ _ _ boundSymbols _ <- declarations
    boundSymbol <- boundSymbols
    guard (isType boundSymbol)
    return (tempID,[boundSymbol]))


-- | Build a Map that assigns to every pair of class and type a list
-- of instances for that class and type.
classAndTypeInstances :: [(TempID,[Declaration])] -> Map (Symbol,Symbol) [TempID]
classAndTypeInstances fragmentNodes = Map.fromListWith (++) (do

    let graphSymbols = Set.fromList (do
            (_,declarations) <- fragmentNodes
            Declaration _ _ _ boundSymbols _ <- declarations
            boundSymbols)

    (tempID,declarations) <- fragmentNodes
    declaration <- declarations
    guard (isInstance declaration)
    let Declaration _ _ _ _ mentionedsymbols = declaration

    let maybeClassSymbol = do
            classSymbolMentionedLast <- listToMaybe (reverse (filter isClass (map fst mentionedsymbols)))
            guard (Set.member classSymbolMentionedLast graphSymbols)
            return classSymbolMentionedLast
        maybeTypeSymbol = do
            typeSymbolMentionedFirst <- listToMaybe (filter isType (map fst mentionedsymbols))
            guard (Set.member typeSymbolMentionedFirst graphSymbols)
            return typeSymbolMentionedFirst

    classSymbol <- maybeToList maybeClassSymbol
    typeSymbol <- maybeToList maybeTypeSymbol

    return ((classSymbol,typeSymbol),[tempID]))


-- | Find and add instance IDs for relevant instance to a given temporary
-- slice.
addInstances ::
    Map TempID [Symbol] ->
    Map TempID [Symbol] ->
    Map (Symbol,Symbol) [InstanceID] ->
    Map TempID TempSlice ->
    TempSlice -> TempSlice
addInstances classMap typeMap instanceMap tempSliceMap (Slice tempID language fragment tempUses _) =
    Slice tempID language fragment tempUses instances where
        usedTempIDs = transitivelyUsedTempIDs tempSliceMap tempID
        relevantClasses = do
            usedTempID <- usedTempIDs
            concat (maybeToList (Map.lookup usedTempID classMap))
        relevantTypes = do
            usedTempID <- usedTempIDs
            concat (maybeToList (Map.lookup usedTempID typeMap))
        instances = do
            relevantClass <- relevantClasses
            relevantType <- relevantTypes
            concat (maybeToList (Map.lookup (relevantClass,relevantType) instanceMap))


-- | Given a Map from temporary ID to corresponding slice and a temporary ID
-- find all transitively used temporary IDs.
transitivelyUsedTempIDs :: Map TempID TempSlice -> TempID -> [TempID]
transitivelyUsedTempIDs tempSliceMap tempID =
    execState (transitivelyUsedTempIDsStatefully tempSliceMap tempID) []


-- | Statefully recurse over used temporary IDs and add the to a list.
transitivelyUsedTempIDsStatefully :: Map TempID TempSlice -> TempID -> State [TempID] ()
transitivelyUsedTempIDsStatefully tempSliceMap tempID = do
    seenTempIDs <- get
    unless (elem tempID seenTempIDs) (do
        put (tempID : seenTempIDs)
        let usedTempIDs = do
                Slice _ _ _ uses _ <- maybeToList (Map.lookup tempID tempSliceMap)
                Use _ _ (OtherSlice usedTempID) <- uses
                return usedTempID
        forM_ usedTempIDs (transitivelyUsedTempIDsStatefully tempSliceMap))


-- | Hash the slice with the given temporary ID.
computeHash :: Map TempID TempSlice -> TempID -> SliceID
computeHash tempSliceMap tempID = abs (fromIntegral (hash (fragment,uses,language))) where
    Just (Slice _ language fragment tempUses _) = Map.lookup tempID tempSliceMap
    uses = map (replaceUseID (computeHash tempSliceMap)) tempUses

-- | Replace every occurence of a temporary ID in the given slice with the final ID.
replaceSliceID :: (TempID -> SliceID) -> TempSlice -> Slice
replaceSliceID f (Slice tempID language fragment uses instances) =
    Slice (f tempID) language fragment (map (replaceUseID f) uses) (map f instances)

-- | Replace every occurence of a temporary ID in the given use with the final ID.
-- A temporary as opposed to a slice ID is smaller than zero.
replaceUseID :: (TempID -> SliceID) -> Use -> Use
replaceUseID f (Use qualification usedName (OtherSlice tempID))
    | tempID < 0 = (Use qualification usedName (OtherSlice (f tempID)))
replaceUseID _ use = use

symbolUsedName :: Symbol -> UsedName
symbolUsedName (Constructor _ constructorName typeName) =
    ConstructorName (fromName typeName) (fromName constructorName)
symbolUsedName symbol
    | isValue symbol = ValueName (fromName (symbolName symbol))
    | otherwise = TypeName (fromName (symbolName symbol))

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
    DataFam {} -> True
    _ -> False

isFamily :: Symbol -> Bool
isFamily symbol = case symbol of
    TypeFam {} -> True
    DataFam {} -> True
    _ -> False

isInstance :: Declaration -> Bool
isInstance (Declaration ClassInstance _ _ _ _) = True
isInstance (Declaration DerivingInstance _ _ _ _) = True
isInstance _ = False

fromName :: Name.Name -> Name
fromName (Name.Ident name) = Identifier (pack name)
fromName (Name.Symbol name) = Operator (pack name)

data Dependency =
    UsesSymbol (Maybe ModuleName) Symbol |
    Signature |
    Fixity |
    Family
        deriving (Eq,Ord,Show)
