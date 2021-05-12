{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Fragnix.DeclarationLocalSlices
  ( declarationLocalSlices
  , fragmentLocalSlices
  , declarationSCCs
  ) where

import qualified Fragnix.Core.ForeignSlice as FS (ForeignSlice (..))

import Fragnix.Declaration
    (Declaration (Declaration),
    Genre (DerivingInstance, FamilyInstance, ForeignImport, InfixFixity, TypeClassInstance, TypeSignature))
import Fragnix.LocalSlice
    (LocalInstance (LocalInstance), LocalInstanceID,
    LocalReference (Builtin, ForeignSlice, OtherLocalSlice, OtherSlice),
    LocalSlice (LocalSlice), LocalSliceID (LocalSliceID), LocalUse (LocalUse))
import Fragnix.Slice
    (Fragment (Fragment),
    InstancePart (ForThisType, ForThisTypeOfUnknownClass, OfThisClass, OfThisClassForUnknownType),
    Language (Language), Name (Identifier, Operator), UsedName (..),
    moduleNameReference)
import qualified Fragnix.Slice as Slice (Reference (Builtin, OtherSlice))

import Language.Haskell.Exts
    (Extension (EnableExtension, UnknownExtension),
    KnownExtension (CPP, Safe, Trustworthy), ModuleName (ModuleName),
    prettyExtension, prettyPrint)
import qualified Language.Haskell.Exts as Name (Name (Ident, Symbol))
import Language.Haskell.Names
    (Symbol (Class, Constructor, Data, DataFam, Method, NewType, Selector, Type, TypeFam, Value, symbolModule, symbolName))

import Data.Graph (buildG, scc)
import Data.Tree (flatten)

import Control.Monad (guard)
import Data.List (nub, (\\))
import Data.Map (Map)
import qualified Data.Map as Map (fromList, fromListWith, lookup, (!))
import Data.Maybe (listToMaybe, maybeToList)
import Data.Text (pack)


-- | Extract all slices from the given list of declarations. Also return a map
-- from a symbol to the sliceID of the slice that binds it now.
declarationLocalSlices :: [Declaration] -> [FS.ForeignSlice] -> ([LocalSlice], Map Symbol LocalSliceID)
declarationLocalSlices declarations foreignSlices = let

    fragmentNodes = declarationSCCs declarations

    tempSliceList = fragmentLocalSlices fragmentNodes foreignSlices

    symbolTempIDs = sliceBindings fragmentNodes

    in (tempSliceList, symbolTempIDs)


-- | Create a dependency graph between all declarations in the given list. Then
-- find and returns the strongly connected components of this graph. Edges in
-- this graph come primarily from when a declaration mentions a symbol another
-- declaration binds. But also from signatures, fixities and type and data
-- family instances. The strongly conncected components of this graph are our
-- compilation units.
declarationSCCs :: [Declaration] -> [(LocalSliceID,[Declaration])]
declarationSCCs declarations = zip localSliceIDs declarationSCCList where

    localSliceIDs = map (LocalSliceID . pack . show) [-1 :: Integer,-2..]

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

    -- Each data or type family should be in the same compilation unit as its localInstances
    familyInstanceEdges = do
        (familyInstanceNode,Declaration FamilyInstance _ _ _ mentionedSymbols) <- declarationNodes
        mentionedFamily <- take 1 (filter isFamily (map fst mentionedSymbols))
        familyNode <- maybeToList (Map.lookup mentionedFamily boundMap)
        return (familyNode, familyInstanceNode)


-- | Given a list of declaration nodes builds a list of local
-- slices. One for each declaration node.
fragmentLocalSlices :: [(LocalSliceID,[Declaration])] -> [FS.ForeignSlice] -> [LocalSlice]
fragmentLocalSlices fragmentNodes foreignSlices = map build fragmentNodes where
    build = buildTempSlice sliceBindingsMap constructorMap instanceLocalIDs foreignSlices
    sliceBindingsMap = sliceBindings fragmentNodes
    constructorMap = sliceConstructors fragmentNodes
    instanceLocalIDs = instanceLocalSliceIDs sliceBindingsMap fragmentNodes


-- | Build a Map from symbol to local ID that binds this symbol. If
-- a symbol is builtin there is no entry in this Map.
sliceBindings :: [(LocalSliceID,[Declaration])] -> Map Symbol LocalSliceID
sliceBindings fragmentNodes = Map.fromList (do
    (localSliceID,declarations) <- fragmentNodes
    Declaration _ _ _ boundsymbols _ <- declarations
    boundsymbol <- boundsymbols
    return (boundsymbol,localSliceID))


-- | Create a map from symbol to a list of its constructors.
sliceConstructors :: [(LocalSliceID,[Declaration])] -> Map Symbol [Symbol]
sliceConstructors fragmentNodes = Map.fromListWith (++) (do
    (_,declarations) <- fragmentNodes
    Declaration _ _ _ boundSymbols _ <- declarations
    constructor@(Constructor _ _ typename) <- boundSymbols
    typeSymbol <- boundSymbols
    guard (isType typeSymbol)
    guard (symbolName typeSymbol == typename)
    return (typeSymbol,[constructor]))


-- | Take a local environment, a constructor Map and a pair of a local ID and a
-- fragment. Return a local slice. The resulting local slice does not yet have
-- have its required local instances resolved.
buildTempSlice ::
    Map Symbol LocalSliceID ->
    Map Symbol [Symbol] ->
    [(LocalInstanceID,Maybe LocalSliceID,Maybe LocalSliceID)] ->
    [FS.ForeignSlice] ->
    (LocalSliceID,[Declaration]) ->
    LocalSlice
buildTempSlice localEnvironment constructorMap instanceLocalIDs foreignSlices (node,declarations) =
    LocalSlice localSliceID language fragments localUses localInstances where
        localSliceID = node

        language = Language extensions

        extensions = nub (do
            Declaration _ ghcextensions _ _ _ <- declarations
            ghcextension <- ghcextensions
            -- disregard the Safe, Trustworthy, CPP and roles extensions
            guard (not (ghcextension `elem` unwantedExtensions))
            return (pack (prettyExtension ghcextension)))

        fragments = Fragment (do
            Declaration _ _ ast _ _ <- arrange declarations
            return ast)

        localUses = nub (mentionedUses ++ implicitConstructorUses ++ cTypesConstructorUses ++ foreignUses)

        -- A use for every mentioned symbol
        mentionedUses = nub (do
            Declaration _ _ _ _ mentionedsymbols <- declarations
            (mentionedsymbol,maybequalification) <- mentionedsymbols

            let maybeQualificationText = fmap (pack . prettyPrint) maybequalification
                usedName = symbolUsedName mentionedsymbol
                -- Look up reference to other slice from this graph
                -- if it fails the symbol must be from the environment
                maybeReference = case Map.lookup mentionedsymbol localEnvironment of
                    -- If the symbol is from the environment it is either builtin or refers to an existing slice
                    Nothing -> Just (moduleLocalReference (symbolModule mentionedsymbol))
                    -- If the symbol is from this fragment we generate no reference
                    Just referenceNode -> if referenceNode == node
                        then Nothing
                    -- Else the symbol is from another slice
                        else Just (OtherLocalSlice referenceNode)

            reference <- maybeToList maybeReference

            return (LocalUse maybeQualificationText usedName reference))

        -- If a declaration needs the constructors of used types we add them here.
        implicitConstructorUses = do
            declaration <- declarations
            guard (needsConstructors declaration)
            let Declaration _ _ _ _ mentionedSymbols = declaration
            mentionedSymbol <- map fst mentionedSymbols
            symbolConstructorUses localEnvironment constructorMap mentionedSymbol

        -- Foreign imports need to have constructors of C types in scope.
        cTypesConstructorUses = do
            Declaration ForeignImport _ _ _ _ <- declarations
            let cIntName = ConstructorName (Identifier "CInt") (Identifier "CInt")
                cSizeName = ConstructorName (Identifier "CSize") (Identifier "CSize")
                cTypesReference = Builtin "Foreign.C.Types"
                cOffName = ConstructorName (Identifier "COff") (Identifier "COff")
                posixTypesReference = Builtin "System.Posix.Types"
            [LocalUse Nothing cIntName cTypesReference,
             LocalUse Nothing cSizeName cTypesReference,
             LocalUse Nothing cOffName posixTypesReference]

        foreignUses = do
            Declaration ForeignImport _ _ _ mentionedSymbols <- declarations
            (mentionedsymbol,_) <- mentionedSymbols
            let usedName = symbolUsedName mentionedsymbol
            FS.ForeignSlice{FS.sliceID} <- foreignSlices
            return (LocalUse Nothing usedName (ForeignSlice sliceID))

        -- Add localInstances for this type or class.
        localInstances = classInstances ++ typeInstances

        -- Class localInstances are instance slices that have this slice as their class
        -- Some may be for builtin types
        classInstances = do
            (instanceID,Just classTempID,maybeTypeLocalID) <- instanceLocalIDs
            guard (localSliceID == classTempID)
            case maybeTypeLocalID of
                Nothing -> return (LocalInstance OfThisClassForUnknownType instanceID)
                Just _ -> return (LocalInstance OfThisClass instanceID)

        -- Type localInstances are instance slices that have this slice as their type
        -- Some may be of builtin classes
        typeInstances = do
            (instanceID,maybeClassLocalID,Just typeTempID) <- instanceLocalIDs
            guard (localSliceID == typeTempID)
            case maybeClassLocalID of
                Nothing -> return (LocalInstance ForThisTypeOfUnknownClass instanceID)
                Just _ -> return (LocalInstance ForThisType instanceID)



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


-- | Finds the given symbol's constructors and wraps them in 'LocalUse's. It
-- needs a local environment and a Map from type symbol to its constructor
-- symbols. We have two cases. If the given symbol is builtin and a newtype we
-- return a constructor with the same name. If the given symbol is from a slice
-- we look up its constructors and return a use for each of those.
symbolConstructorUses :: Map Symbol LocalSliceID -> Map Symbol [Symbol] -> Symbol -> [LocalUse]
symbolConstructorUses localEnvironment constructorMap symbol =
    case Map.lookup symbol localEnvironment of
        Nothing -> case moduleLocalReference (symbolModule symbol) of
            builtinReference@(Builtin _) -> case symbol of
                NewType _ symbolname -> do
                    let symbolTypeName = fromName symbolname
                        constructorname = ConstructorName symbolTypeName symbolTypeName
                    return (LocalUse Nothing constructorname builtinReference)
                _ -> []
            _ -> []
        Just referenceNode -> do
            constructorSymbol <- concat (maybeToList (Map.lookup symbol constructorMap))
            return (LocalUse Nothing (symbolUsedName constructorSymbol) (OtherLocalSlice referenceNode))


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


moduleLocalReference :: ModuleName () -> LocalReference
moduleLocalReference (ModuleName () moduleName) =
  case moduleNameReference moduleName of
    Slice.Builtin originalModule -> Builtin originalModule
    Slice.OtherSlice sliceID     -> OtherSlice sliceID

-- | Finds for each instance maybe the local ID of the class it is of and maybe
-- the local ID of the type it is for. Needs a map from symbol to ID of the
-- slice that binds that symbol and a list of pairs of a local ID and a
-- framgent. Produces a list of triples of an instance ID and maybe a the ID of
-- a class and maybe the ID of a type. If the class or type of the instance are
-- not in the list of fragments there is no class or type ID. This means they
-- are either builtin or in a preexisting slice.
instanceLocalSliceIDs ::
    Map Symbol LocalSliceID ->
    [(LocalSliceID,[Declaration])] ->
    [(LocalInstanceID,Maybe LocalSliceID,Maybe LocalSliceID)]
instanceLocalSliceIDs sliceBindingsMap fragmentNodes = do
    (instanceID,maybeClassSymbol,maybeTypeSymbol) <- instanceSymbols fragmentNodes
    let maybeClassLocalID = case maybeClassSymbol of
            Nothing          -> Nothing
            Just classSymbol -> Map.lookup classSymbol sliceBindingsMap
        maybeTypeLocalID = case maybeTypeSymbol of
            Nothing         -> Nothing
            Just typeSymbol -> Map.lookup typeSymbol sliceBindingsMap
    return (instanceID,maybeClassLocalID,maybeTypeLocalID)


-- | A list of triples of the instance, its class symbol and its type symbol.
-- For builtin types like '(,)' we cannot provide a symbol and have to return
-- 'Nothing'.
instanceSymbols :: [(LocalSliceID,[Declaration])] -> [(LocalInstanceID,Maybe Symbol,Maybe Symbol)]
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


symbolUsedName :: Symbol -> UsedName
symbolUsedName (Constructor _ constructorname typename) =
    ConstructorName (fromName typename) (fromName constructorname)
symbolUsedName symbol
    | isValue symbol = ValueName (fromName (symbolName symbol))
    | otherwise = TypeName (fromName (symbolName symbol))

isValue :: Symbol -> Bool
isValue symbol = case symbol of
    Value {}       -> True
    Method {}      -> True
    Selector {}    -> True
    Constructor {} -> True
    _              -> False

isClass :: Symbol -> Bool
isClass symbol = case symbol of
    Class {} -> True
    _        -> False

isType :: Symbol -> Bool
isType symbol = case symbol of
    Type {}    -> True
    Data {}    -> True
    NewType {} -> True
    DataFam {} -> True
    _          -> False

isFamily :: Symbol -> Bool
isFamily symbol = case symbol of
    TypeFam {} -> True
    DataFam {} -> True
    _          -> False

isInstance :: Declaration -> Bool
isInstance (Declaration TypeClassInstance _ _ _ _) = True
isInstance (Declaration DerivingInstance _ _ _ _)  = True
isInstance _                                       = False

fromName :: Name.Name () -> Name
fromName (Name.Ident () name)  = Identifier (pack name)
fromName (Name.Symbol () name) = Operator (pack name)
