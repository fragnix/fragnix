{-# LANGUAGE OverloadedStrings #-}
module Fragnix.DeclarationSlices where

import Fragnix.Declaration (
    Declaration(Declaration),
    Genre(TypeSignature,TypeClassInstance,InfixFixity,DerivingInstance,FamilyInstance,ForeignImport))
import Fragnix.Slice (
    Slice(Slice),SliceID,Language(Language),Fragment(Fragment),
    Use(Use),UsedName(..),Name(Identifier,Operator),Reference(OtherSlice,Builtin),
    InstanceID,Instance(Instance),
    InstancePart(OfClass,OfClassForBuiltinType,ForType,ForTypeOfBuiltinClass))

import Language.Haskell.Names (
    Symbol(Constructor,Value,Method,Selector,Class,Type,Data,NewType,TypeFam,DataFam,
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

import Control.Monad (guard)
import Data.Text (pack)
import Data.Char (isDigit)
import Data.Map (Map)
import qualified Data.Map as Map (
    lookup,fromList,fromListWith,(!),map,keys)
import Data.Maybe (maybeToList,listToMaybe,fromJust)
import Data.Hashable (hash)
import Data.List (nub,(\\))


-- | Extract all slices from the given list of declarations. Also return a map
-- from a symbol to the sliceID of the slice that binds it now.
declarationSlices :: [Declaration] -> ([Slice],Map Symbol SliceID)
declarationSlices declarations = (slices,symbolSlices) where

    fragmentNodes = fragmentSCCs (declarationGraph declarations)

    sliceBindingsMap = sliceBindings fragmentNodes

    constructorMap = sliceConstructors fragmentNodes
    instanceTempIDList = instanceTempIDs sliceBindingsMap fragmentNodes

    tempSlices = map
        (buildTempSlice sliceBindingsMap constructorMap instanceTempIDList)
        fragmentNodes
    tempSliceMap = sliceMap tempSlices

    tempSliceIDMap = sliceIDMap tempSliceMap
    slices = map (replaceSliceID ((Map.!) tempSliceIDMap)) tempSlices
    symbolSlices = Map.map (\tempID -> tempSliceIDMap Map.! tempID) sliceBindingsMap


-- | Build a Map from symbol to temporary ID that binds this symbol. If
-- a symbol is builtin there is not entry in this Map.
sliceBindings :: [(TempID,[Declaration])] -> Map Symbol TempID
sliceBindings fragmentNodes = Map.fromList (do
    (tempID,declarations) <- fragmentNodes
    Declaration _ _ _ boundsymbols _ <- declarations
    boundsymbol <- boundsymbols
    return (boundsymbol,tempID))


-- | Create a map from symbol to a list of its constructors.
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
-- from signatures, fixities and type and data family instances. The strongly conncected
-- components of this graph are our compilation units.
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


-- | Take a temporary environment, a constructor map and a pair of a node and a fragment.
-- Return a slice with a temporary ID that might contain references to temporary IDs.
-- The nodes must have negative IDs starting from -1 to distinguish temporary from permanent
-- slice IDs. The resulting temporary slice does not have its required instances resolved.
buildTempSlice ::
    Map Symbol TempID ->
    Map Symbol [Symbol] ->
    [(InstanceID,Maybe TempID,Maybe TempID)] ->
    (TempID,[Declaration]) ->
    TempSlice
buildTempSlice tempEnvironment constructorMap instanceTempIDList (node,declarations) =
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
                    -- Else the symbol is from another slice
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

        -- Add instances for this type or class.
        instances = classInstances ++ typeInstances

        -- Class instances are instance slices that have this slice as their class
        -- Some may be for builtin types
        classInstances = do
            (instanceID,Just classTempID,maybeTypeTempID) <- instanceTempIDList
            guard (tempID == classTempID)
            case maybeTypeTempID of
                Nothing -> return (Instance OfClassForBuiltinType instanceID)
                Just _ -> return (Instance OfClass instanceID)

        -- Type instances are instance slices that have this slice as their type
        -- Some may be of builtin classes
        typeInstances = do
            (instanceID,maybeClassTempID,Just typeTempID) <- instanceTempIDList
            guard (tempID == typeTempID)
            case maybeClassTempID of
                Nothing -> return (Instance ForTypeOfBuiltinClass instanceID)
                Just _ -> return (Instance ForType instanceID)



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


-- | Given a temporary environment and a Map from type symbol to its constructor
-- symbol and a symbol this function finds the given symbol's constructors and
-- returns a list of use references for them.
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


-- | Given a map from symbol to ID of the slice that binds that symbol and
-- a list of pairs of a temporaty ID and a framgent. Produces a list of triples
-- of an instance ID and maybe a the ID of a class and maybe the ID of a type.
-- If the class or type of the instance are not in the list of fragments there
-- is no class or type ID. This means they are either builtin or in a preexisting
-- slice.
instanceTempIDs ::
    Map Symbol TempID ->
    [(TempID,[Declaration])] ->
    [(InstanceID,Maybe TempID,Maybe TempID)]
instanceTempIDs sliceBindingsMap fragmentNodes = do
    (instanceID,maybeClassSymbol,maybeTypeSymbol) <- instanceSymbols fragmentNodes
    let maybeClassTempID = case maybeClassSymbol of
            Nothing -> Nothing
            Just classSymbol -> Map.lookup classSymbol sliceBindingsMap
        maybeTypeTempID = case maybeTypeSymbol of
            Nothing -> Nothing
            Just typeSymbol -> Map.lookup typeSymbol sliceBindingsMap
    return (instanceID,maybeClassTempID,maybeTypeTempID)


-- | A list of triples of the instance, its class symbol and its type symbol.
-- For builtin types like '(,)' we cannot provide a symbol and have to return
-- 'Nothing'.
instanceSymbols :: [(TempID,[Declaration])] -> [(InstanceID,Maybe Symbol,Maybe Symbol)]
instanceSymbols fragmentNodes = do

    (instanceTempID,declarations) <- fragmentNodes
    declaration <- declarations
    guard (isInstance declaration)
    let Declaration _ _ _ _ mentionedsymbols = declaration

    let maybeClassSymbol =
            listToMaybe (reverse (filter isClass (map fst mentionedsymbols)))
        maybeTypeSymbol =
            listToMaybe (filter isType (map fst mentionedsymbols))

    return (instanceTempID,maybeClassSymbol,maybeTypeSymbol)



-- | A temporary ID before slices can be hashed.
type TempID = Integer

-- | A temporary ID for instance slices.
type InstanceTempID = Integer

-- | A Slice with a temporary ID that may use slices with
-- temporary IDs.
type TempSlice = Slice


-- | Build up a map from temporary ID to corresponding slice for better lookup.
sliceMap :: [TempSlice] -> Map TempID TempSlice
sliceMap tempSlices = Map.fromList (do
    tempSlice@(Slice tempSliceID _ _ _ _) <- tempSlices
    return (tempSliceID,tempSlice))


-- | Associate every temporary ID with the final ID.
-- We hash slices in two steps: First without taking instances into account
-- and then with taking instances into account. To prevent cycles in the
-- second step we lookup the instance hash from the first step instead of
-- recursing.
sliceIDMap :: Map TempID TempSlice -> Map TempID SliceID
sliceIDMap tempSliceMap = Map.fromList (do
    let instanceIDMap = Map.fromList (do
            tempID <- Map.keys tempSliceMap
            return (tempID,computeHashWithoutInstances tempSliceMap tempID))
    tempID <- Map.keys tempSliceMap
    return (tempID,computeHashWithInstances tempSliceMap instanceIDMap tempID))


-- | Hash the slice with the given temporary ID.
-- Includes instances into the hash.
-- If we encounter an instance we do not recurse. We look up the instance's
-- ID in the given instance ID Map.
computeHashWithInstances :: Map TempID TempSlice -> Map InstanceTempID InstanceID -> TempID -> SliceID
computeHashWithInstances tempSliceMap instanceIDMap tempID =
    abs (fromIntegral (hash (fragment,uses,language,instances))) where
        Just (Slice _ language fragment tempUses tempInstances) =
            Map.lookup tempID tempSliceMap
        uses =
            map (replaceUseID (computeHashWithInstances tempSliceMap instanceIDMap)) tempUses
        instances =
            map (replaceInstanceID lookupInstanceTempID) tempInstances
        lookupInstanceTempID instanceTempID = case Map.lookup instanceTempID instanceIDMap of
            Nothing -> error "computeHashWithInstances: instance temporary ID not found"
            Just instanceID -> instanceID


-- | Hash the slice with the given temporary ID but ignore instances.
computeHashWithoutInstances :: Map TempID TempSlice -> TempID -> SliceID
computeHashWithoutInstances tempSliceMap tempID =
    abs (fromIntegral (hash (fragment,uses,language))) where
        Just (Slice _ language fragment tempUses _) =
            Map.lookup tempID tempSliceMap
        uses =
            map (replaceUseID (computeHashWithoutInstances tempSliceMap)) tempUses


-- | Replace every occurence of a temporary ID in the given slice with the final ID.
replaceSliceID :: (TempID -> SliceID) -> TempSlice -> Slice
replaceSliceID f (Slice tempID language fragment uses instances) =
    Slice (f tempID) language fragment (map (replaceUseID f) uses) (map (replaceInstanceID f) instances)

replaceInstanceID :: (TempID -> SliceID) -> Instance -> Instance
replaceInstanceID f (Instance instancePart instanceID) =
    Instance instancePart (f instanceID)

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
    Type {} -> True
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
isInstance (Declaration TypeClassInstance _ _ _ _) = True
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
