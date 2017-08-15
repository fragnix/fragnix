{-# LANGUAGE OverloadedStrings #-}
module Fragnix.DeclarationSlices where

import Fragnix.Declaration (
    Declaration(Declaration),
    Genre(TypeSignature,TypeClassInstance,InfixFixity,DerivingInstance,FamilyInstance,ForeignImport))
import Fragnix.Slice (
    Slice(Slice),SliceID,Language(Language),Fragment(Fragment),
    Use(Use),UsedName(..),Name(Identifier,Operator),Reference(OtherSlice,Builtin),
    InstanceID,Instance(Instance),
    InstancePart(OfThisClass,OfThisClassForUnknownType,ForThisType,ForThisTypeOfUnknownClass))

import Language.Haskell.Names (
    Symbol(Constructor,Value,Method,Selector,Class,Type,Data,NewType,TypeFam,DataFam,
        symbolName,symbolModule))
import qualified Language.Haskell.Exts as Name (
    Name(Ident,Symbol))
import Language.Haskell.Exts (
    ModuleName(ModuleName),prettyExtension,prettyPrint,
    Extension(EnableExtension,UnknownExtension),
    KnownExtension(Safe,CPP,Trustworthy))

import Data.Graph (buildG,scc)
import Data.Tree (flatten)

import Control.Monad (guard)
import Data.Foldable (for_)
import Data.Traversable (for)
import Control.Monad.Trans.State (State,execState,get,put)
import Data.Text (pack)
import Data.Char (isDigit)
import Data.Map (Map)
import qualified Data.Map as Map (
    lookup,fromList,fromListWith,(!),map,keys,
    empty,insert)
import Data.Maybe (maybeToList,listToMaybe)
import Data.Hashable (hash)
import Data.List (nub,(\\))


-- | Extract all slices from the given list of declarations. Also return a map
-- from a symbol to the sliceID of the slice that binds it now.
declarationSlices :: [Declaration] -> ([Slice],Map Symbol SliceID)
declarationSlices declarations = (slices,symbolSlices) where

    fragmentNodes = declarationSCCs declarations

    tempSliceList = tempSlices fragmentNodes
    tempSliceMap = sliceMap tempSliceList

    sliceIDMap = hashSlices2 tempSliceMap
    slices = map (replaceSliceID ((Map.!) sliceIDMap)) tempSliceList
    symbolSlices = Map.map (\tempID -> sliceIDMap Map.! tempID) (sliceBindings fragmentNodes)


-- | Create a dependency graph between all declarations in the given list. Then find and
-- returns the strongly connected components of this graph.
-- Edges in this graph come primarily from when a declaration mentions a symbol
-- another declaration binds. But also from signatures, fixities and
-- type and data family instances. The strongly conncected components of this
-- graph are our compilation units.
declarationSCCs :: [Declaration] -> [(TempID,[Declaration])]
declarationSCCs declarations = zip [-1,-2..] declarationSCCList where

    declarationSCCList = map (map lookupVertex . flatten) (scc declarationGraph)

    declarationGraph = buildG (0, length declarations - 1) dependencyEdges

    declarationNodes = zip [0..] declarations

    dependencyEdges =
        usedSymbolEdges ++ signatureEdges ++ fixityEdges ++ familyInstanceEdges

    lookupVertex vertex = vertexDeclarationMap Map.! vertex

    vertexDeclarationMap = Map.fromList declarationNodes

    -- A map from bound symbol to node ID
    boundMap = Map.fromList (do
        (node,declaration) <- declarationNodes
        let Declaration _ _ _ boundsymbols _ = declaration
        boundsymbol <- boundsymbols
        return (boundsymbol,node))

    -- Dependency edges that come from use of a symbol
    usedSymbolEdges = do
        (node,declaration) <- declarationNodes
        let Declaration _ _ _ _ mentionedsymbols = declaration
        mentionedsymbol <- map fst mentionedsymbols
        usednode <- maybeToList (Map.lookup mentionedsymbol boundMap)
        return (node,usednode)

    -- Each declaration should be in the same comilation unit as its type signature
    signatureEdges = do
        (signaturenode,Declaration TypeSignature _ _ _ mentionedsymbols) <- declarationNodes
        mentionedsymbol@(Value _ _) <- map fst mentionedsymbols
        declarationnode <- maybeToList (Map.lookup mentionedsymbol boundMap)
        return (declarationnode,signaturenode)

    -- Each declaration should be in the same compilation unit as its fixity declarations
    fixityEdges = do
        (fixitynode,Declaration InfixFixity _ _ _ mentionedsymbols) <- declarationNodes
        mentionedsymbol <- map fst mentionedsymbols
        bindingnode <- maybeToList (Map.lookup mentionedsymbol boundMap)
        return (bindingnode,fixitynode)

    -- Each data or type family should be in the same compilation unit as its instances
    familyInstanceEdges = do
        (familyInstanceNode,Declaration FamilyInstance _ _ _ mentionedSymbols) <- declarationNodes
        mentionedFamily <- take 1 (filter isFamily (map fst mentionedSymbols))
        familyNode <- maybeToList (Map.lookup mentionedFamily boundMap)
        return (familyNode, familyInstanceNode)


-- | Given a list of declaration nodes builds a list of temporary
-- slices. One for each declaration node.
tempSlices :: [(TempID,[Declaration])] -> [TempSlice]
tempSlices fragmentNodes = map build fragmentNodes where
    build = buildTempSlice sliceBindingsMap constructorMap instanceTempIDList
    sliceBindingsMap = sliceBindings fragmentNodes
    constructorMap = sliceConstructors fragmentNodes
    instanceTempIDList = instanceTempIDs sliceBindingsMap fragmentNodes


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

        language = Language extensions

        extensions = (nub (do
            Declaration _ ghcextensions _ _ _ <- declarations
            ghcextension <- ghcextensions
            -- disregard the Safe, Trustworthy, CPP and roles extensions
            guard (not (ghcextension `elem` unwantedExtensions))
            return (pack (prettyExtension ghcextension))))

        fragments = Fragment (do
            Declaration _ _ ast _ _ <- arrange declarations
            return ast)

        uses = nub (mentionedUses ++ implicitConstructorUses ++ cTypesConstructorUses)

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

        -- Foreign imports need to have constructors of C types in scope.
        cTypesConstructorUses = do
            Declaration ForeignImport _ _ _ _ <- declarations
            let cIntName = ConstructorName (Identifier "CInt") (Identifier "CInt")
                cSizeName = ConstructorName (Identifier "CSize") (Identifier "CSize")
                cTypesReference = Builtin "Foreign.C.Types"
                cOffName = ConstructorName (Identifier "COff") (Identifier "COff")
                posixTypesReference = Builtin "System.Posix.Types"
            [Use Nothing cIntName cTypesReference,
             Use Nothing cSizeName cTypesReference,
             Use Nothing cOffName posixTypesReference]

        -- Add instances for this type or class.
        instances = classInstances ++ typeInstances

        -- Class instances are instance slices that have this slice as their class
        -- Some may be for builtin types
        classInstances = do
            (instanceID,Just classTempID,maybeTypeTempID) <- instanceTempIDList
            guard (tempID == classTempID)
            case maybeTypeTempID of
                Nothing -> return (Instance OfThisClassForUnknownType instanceID)
                Just _ -> return (Instance OfThisClass instanceID)

        -- Type instances are instance slices that have this slice as their type
        -- Some may be of builtin classes
        typeInstances = do
            (instanceID,maybeClassTempID,Just typeTempID) <- instanceTempIDList
            guard (tempID == typeTempID)
            case maybeClassTempID of
                Nothing -> return (Instance ForThisTypeOfUnknownClass instanceID)
                Just _ -> return (Instance ForThisType instanceID)



-- | Some declarations implicitly require the constructors for all mentioned
-- types. The declarations that need this are: standalone deriving, foreign
-- imports and any declarations mentioning coerce.
needsConstructors :: Declaration -> Bool
needsConstructors (Declaration DerivingInstance _ _ _ _) =
    True
needsConstructors (Declaration ForeignImport _ _ _ _) =
    True
needsConstructors (Declaration _ _ _ _ mentionedSymbols) =
    any ((==(Name.Ident () "coerce")) . symbolName . fst) mentionedSymbols


-- | Given a temporary environment and a Map from type symbol to its constructor
-- symbol and a symbol this function finds the given symbol's constructors and
-- wraps them in 'Use's.
-- We have two cases. If the given symbol is builtin and a newtype we return
-- a constructor with the same name.
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
                    return (Use Nothing constructorName builtinReference)
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
moduleReference :: ModuleName () -> Reference
moduleReference (ModuleName () moduleName)
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
-- TempIDs are always smaller than zero.
type TempID = Integer

-- | A Slice with a temporary ID that may use slices with
-- temporary IDs.
type TempSlice = Slice

-- | A slice ID after the first round of hashing.
type Hash1ID = Integer


-- | Build up a map from temporary ID to corresponding slice for better lookup.
sliceMap :: [TempSlice] -> Map TempID TempSlice
sliceMap tempSliceList = Map.fromList (do
    tempSlice@(Slice tempSliceID _ _ _ _) <- tempSliceList
    return (tempSliceID,tempSlice))


-- | Associate every temporary ID with its slice ID. This slice ID is
-- a hash of the slice's content. If we include instances in this content
-- we get cycles. To avoid these cycles we hash slices in two steps:
-- First without taking instances into account and then with taking instances
-- into account.
-- To prevent cycles in the second step we lookup the instance hash from
-- the first hashing step instead of recursing.
hashSlices2 :: Map TempID TempSlice -> Map TempID SliceID
hashSlices2 tempSliceMap = execState (
    for_ tempIDs (hashSlice2 tempSliceMap hash1IDMap)) Map.empty where
        tempIDs = Map.keys tempSliceMap
        hash1IDMap = hashSlices1 tempSliceMap


-- | Compute the second hash for the temporary slice with the given temporary ID.
hashSlice2 ::
    Map TempID TempSlice ->
    Map TempID Hash1ID ->
    TempID ->
    State (Map TempID SliceID) SliceID
hashSlice2 tempSliceMap hash1IDMap tempID = do
    sliceIDMap <- get
    case Map.lookup tempID sliceIDMap of
        Just sliceID -> return sliceID
        Nothing -> do
            let Just (Slice _ language fragment tempUses tempInstances) =
                    Map.lookup tempID tempSliceMap
            uses <- for tempUses (hashUse2 tempSliceMap hash1IDMap)
            let instances = map (
                    replaceInstanceID (\instanceTempID -> hash1IDMap Map.! instanceTempID))
                    tempInstances
                sliceID = abs (fromIntegral (hash (language,fragment,uses,instances)))
            put (Map.insert tempID sliceID sliceIDMap)
            return sliceID


-- | If the given Use references a temporary ID replace that temporary ID by
-- the final slice ID.
hashUse2 ::
    Map TempID TempSlice ->
    Map TempID Hash1ID ->
    Use ->
    State (Map TempID SliceID) Use
hashUse2 tempSliceMap hash1IDMap use = case use of
    Use qualification usedName (OtherSlice tempID) -> do
        if tempID < 0
            then do
                sliceID <- hashSlice2 tempSliceMap hash1IDMap tempID
                return (Use qualification usedName (OtherSlice sliceID))
            else do
                return use
    _ -> return use


-- | Given a list from temporary ID to temporary Slice computes a Map
-- from temporary ID to the first hash. This first hash does not include
-- type class instances.
hashSlices1 :: Map TempID TempSlice -> Map TempID Hash1ID
hashSlices1 tempSliceMap = execState (
    for_ tempIDs (hashSlice1 tempSliceMap)) Map.empty where
        tempIDs = Map.keys tempSliceMap

-- | Compute the first hash for the given temporary ID using a cache.
hashSlice1 :: Map TempID TempSlice -> TempID -> State (Map TempID Hash1ID) Hash1ID
hashSlice1 tempSliceMap tempID = do
    hash1IDMap <- get
    case Map.lookup tempID hash1IDMap of
        Just hash1ID -> return hash1ID
        Nothing -> do
            let Just (Slice _ language fragment tempUses _) =
                    Map.lookup tempID tempSliceMap
            uses <- for tempUses (hashUse1 tempSliceMap)
            let hash1ID = abs (fromIntegral (hash (language,fragment,uses)))
            put (Map.insert tempID hash1ID hash1IDMap)
            return hash1ID

-- | If the given use references a temporary slice via a temporary ID
-- then replace that reference by the first hash of that temporary slice.
hashUse1 :: Map TempID TempSlice -> Use -> State (Map TempID Hash1ID) Use
hashUse1 tempSliceMap use = case use of
    Use qualification usedName (OtherSlice tempID) -> do
        if tempID < 0
            then do
                hash1ID <- hashSlice1 tempSliceMap tempID
                return (Use qualification usedName (OtherSlice hash1ID))
            else do
                return use
    _ -> return use

-- | Replace every occurence of a temporary ID in the given slice with the final ID.
replaceSliceID :: (TempID -> SliceID) -> TempSlice -> Slice
replaceSliceID f (Slice tempID language fragment uses instances) =
    Slice (f tempID) language fragment (map (replaceUseID f) uses) (map (replaceInstanceID f) instances)

-- | Replace every occurence of a temporary ID in the given use with the final ID.
-- A temporary as opposed to a slice ID is smaller than zero.
replaceUseID :: (TempID -> SliceID) -> Use -> Use
replaceUseID f (Use qualification usedName (OtherSlice tempID))
    | tempID < 0 = (Use qualification usedName (OtherSlice (f tempID)))
replaceUseID _ use = use

-- | Replace every occurence of a temporary ID in the given Instance with the final
-- ID. A temporary ID is smaller than zero.
replaceInstanceID :: (TempID -> SliceID) -> Instance -> Instance
replaceInstanceID f (Instance instancePart instanceID)
    | instanceID < 0 = Instance instancePart (f instanceID)
replaceInstanceID _ instanc = instanc


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

fromName :: Name.Name () -> Name
fromName (Name.Ident () name) = Identifier (pack name)
fromName (Name.Symbol () name) = Operator (pack name)

