module Fragnix.SliceCompiler where

import Fragnix.Slice (
    Slice(Slice),SliceID,Language(Language),Fragment(Fragment),Use(Use),
    Reference(OtherSlice,Builtin),
    UsedName(ValueName,TypeName,ConstructorName),Name(Identifier,Operator),
    InstanceID,Instance(Instance),
    InstancePart(OfClass,OfClassForBuiltinType,ForType,ForTypeOfBuiltinClass),
    readSliceDefault)

import Prelude hiding (writeFile)

import Language.Haskell.Exts.Syntax (
    Module(Module),ModuleName(ModuleName),ModulePragma(LanguagePragma),
    Decl(InstDecl,DataDecl,GDataDecl,PatBind,FunBind,ForImp,DerivDecl,TypeSig,TypeInsDecl,DataInsDecl,GDataInsDecl),
    Name(Ident,Symbol),QName(UnQual),
    ExportSpec(EThingAll),
    ImportDecl(ImportDecl,importSrc,importModule),ImportSpec(IVar,IAbs,IThingWith),
    CName(ConName),Namespace(NoNamespace))
import Language.Haskell.Exts.SrcLoc (noLoc)
import Language.Haskell.Exts.Parser (
    parseModuleWithMode,ParseMode(parseFilename,extensions,fixities),defaultParseMode,
    fromParseResult)
import Language.Haskell.Exts.Fixity (baseFixities)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.Extension (parseExtension)

import Data.Text.IO (writeFile)
import Data.Text (Text,pack,unpack)

import System.FilePath ((</>),(<.>))
import System.Directory (createDirectoryIfMissing,doesFileExist)
import System.Process (rawSystem)
import System.Exit (ExitCode)
import Control.Exception (SomeException,catch,evaluate)

import Control.Monad.Trans.State.Strict (StateT,execStateT,get,put,State,execState)
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import qualified Data.Map as Map (fromList,lookup,(!))
import qualified Data.Set as Set (fromList,union,intersection,toList)
import Control.Monad (forM,forM_,unless)
import Data.Maybe (mapMaybe, isJust, maybeToList, fromMaybe)
import Data.List (partition)
import Data.Char (isDigit)


-- | Compile the slice with the given slice ID to an executable program.
-- Assumes that the slice contains a declaration for some 'main :: IO ()'.
sliceCompilerMain :: SliceID -> IO ExitCode
sliceCompilerMain sliceID = do
    putStrLn "Generating compilation units ..."
    writeSliceModules sliceID
    putStrLn "Invoking GHC ..."
    rawSystem "ghc" [
        "-o","main",
        "-ifragnix/temp/compilationunits",
        "-main-is",sliceModuleName sliceID,
        sliceModulePath sliceID]


-- | Compile the slice with the given slice ID. Set verbosity to zero and
-- turn all warnings off.
sliceCompiler :: SliceID -> IO ExitCode
sliceCompiler sliceID = do
    writeSliceModules sliceID
    rawSystem "ghc" ["-v0","-w","-ifragnix/temp/compilationunits",sliceModulePath sliceID]


-- | Generate and write all modules necessary to compile the slice with the given ID.
writeSliceModules :: SliceID -> IO ()
writeSliceModules sliceID = do
    createDirectoryIfMissing True sliceModuleDirectory
    (slices, instances) <- loadSlicesTransitive sliceID
    let sliceModules = map (sliceModule relevantInstancesMap) slices
        sliceHSBoots = map bootModule sliceModules
        instanceSliceModules = map (sliceModule relevantInstancesMap) instances
        instanceSliceHSBoots = map bootInstanceModule instanceSliceModules
        relevantInstancesMap = relevantInstances (slices ++ instances)
    forM_ (sliceModules ++ instanceSliceModules) writeModule
    forM_ (sliceHSBoots ++ instanceSliceHSBoots) writeHSBoot


-- | Given a slice generate the corresponding module.
sliceModule :: Map SliceID [InstanceID] -> Slice -> Module
sliceModule relevantInstancesMap (Slice sliceID language fragment uses _) =
    let Fragment declarations = fragment
        decls = map (parseDeclaration sliceID ghcextensions) declarations
        moduleName = ModuleName (sliceModuleName sliceID)
        Language ghcextensions _ = language
        languagepragmas = [Ident "NoImplicitPrelude"] ++ (map (Ident . unpack) ghcextensions)
        pragmas = [LanguagePragma noLoc languagepragmas]
        imports =
            map useImport uses ++
            map instanceImport (relevantInstancesMap Map.! sliceID)
        -- We need an export list to export the data family even
        -- though a slice only contains the data family instance
        exports = dataFamilyInstanceExports decls imports
    in Module noLoc moduleName pragmas Nothing exports imports decls


-- | Reparse a declaration from its textual representation in a slice.
parseDeclaration :: SliceID -> [Text] -> Text -> Decl
parseDeclaration sliceID ghcextensions declaration = decl where
    Module _ _ _ _ _ _ [decl] = fromParseResult (parseModuleWithMode parseMode (unpack declaration))
    parseMode = defaultParseMode {
        parseFilename = show sliceID,
        extensions = map (parseExtension . unpack) ghcextensions,
        fixities = Just baseFixities}


-- | Import decl for a given use of a symbol.
useImport :: Use -> ImportDecl
useImport (Use maybeQualification usedName symbolSource) =
    let moduleName = case symbolSource of
            OtherSlice sliceID -> ModuleName (sliceModuleName sliceID)
            Builtin originalModule -> ModuleName (unpack originalModule)
        qualified = isJust maybeQualification
        maybeAlias = fmap (ModuleName . unpack) maybeQualification
        importSpec = case usedName of
            ValueName name -> [IVar (toName name)]
            TypeName name -> [IAbs NoNamespace (toName name)]
            ConstructorName typeName name ->
                [IThingWith (toName typeName) [ConName (toName name)]]
        toName (Identifier name) = Ident (unpack name)
        toName (Operator name) = Symbol (unpack name)

    in ImportDecl noLoc moduleName qualified False False Nothing maybeAlias (Just (False,importSpec))


-- | Import Decl for an instance from the given SliceID. Instance imports
-- are source imports.
instanceImport :: SliceID -> ImportDecl
instanceImport sliceID =
    ImportDecl noLoc (ModuleName (sliceModuleName sliceID)) False True False Nothing Nothing Nothing


-- | We export every type that we import in a data family instance slice
dataFamilyInstanceExports :: [Decl] -> [ImportDecl] -> Maybe [ExportSpec]
dataFamilyInstanceExports [DataInsDecl _ _ _ _ _] imports = Just (do
    ImportDecl _ _ _ _ _ _ _ (Just (False,[IAbs _ typeName])) <- imports
    return (EThingAll (UnQual typeName)))
dataFamilyInstanceExports [GDataInsDecl _ _ _ _ _ _] imports = Just (do
    ImportDecl _ _ _ _ _ _ _ (Just (False,[IAbs _ typeName])) <- imports
    return (EThingAll (UnQual typeName)))
dataFamilyInstanceExports _ _ = Nothing


-- | Create a boot module from an ordinary module.
bootModule :: Module -> Module
bootModule (Module srcloc moduleName pragmas warnings exports imports decls) = 
    Module srcloc moduleName pragmas warnings exports bootImports bootDecls where
        bootImports = mapMaybe bootImport imports
        bootDecls = concatMap bootDecl decls


-- | Create a boot module for an instance module
-- WORKAROUND: We can not put data family constructors into hs-boot files.
-- https://ghc.haskell.org/trac/ghc/ticket/8441
-- hs-boot files contain source imports for constructors.
-- We remove constructor imports from hs-boot files that contain only instances.
bootInstanceModule :: Module -> Module
bootInstanceModule (Module srcloc moduleName pragmas warnings exports imports decls) =
    Module srcloc moduleName pragmas warnings exports bootImports bootDecls where
        bootImports = mapMaybe bootInstanceImport imports
        bootDecls = concatMap bootDecl decls


-- | Remove all source imports and make all other imports source except those
-- from builtin modules. The removal of source imports means that the
-- ALLINSTANCES module will not be imported.
bootImport :: ImportDecl -> Maybe ImportDecl
bootImport importDecl
    | importSrc importDecl = Nothing
    | isSliceModule (importModule importDecl) = Just (importDecl {importSrc = True})
    | otherwise = Just importDecl


-- | Remove all source imports and make all other imports source except those
-- from builtin modules. The removal of source imports means that the
-- ALLINSTANCES module will not be imported. Also remove constructor imports.
bootInstanceImport :: ImportDecl -> Maybe ImportDecl
bootInstanceImport importDecl
    | importSrc importDecl = Nothing
    | isConstructorImport importDecl = Nothing
    | isSliceModule (importModule importDecl) = Just (importDecl {importSrc = True})
    | otherwise = Just importDecl


-- | Remove instance bodies, make derived instances into bodyless instance decls,
-- remove value bindings and generate a type signature for foreign imports.
bootDecl :: Decl -> [Decl]
bootDecl (InstDecl srcloc overlap typeVars context classname types _) =
    [InstDecl srcloc overlap typeVars context classname types []]
bootDecl (DataDecl srcloc dataOrNew context dataname typeVars constructors _) =
    [DataDecl srcloc dataOrNew context dataname typeVars constructors []]
bootDecl (GDataDecl srcloc dataOrNew context name typeVars kind constructors _) =
    [GDataDecl srcloc dataOrNew context name typeVars kind constructors []]
bootDecl (DerivDecl srcloc overlap typeVars context classname types) =
    [InstDecl srcloc overlap typeVars context classname types []]
bootDecl (PatBind _ _ _ _) =
    []
bootDecl (FunBind _) =
    []
bootDecl (ForImp srcloc _ _ _ name typ) =
    [TypeSig srcloc [name] typ]
bootDecl (TypeInsDecl _ _ _) =
    []
bootDecl (DataInsDecl _ _ _ _ _) =
    []
bootDecl (GDataInsDecl _ _ _ _ _ _) =
    []
bootDecl decl = [decl]


-- | Given a list of slices, find for each slice the list of relevant
-- instances. An instance is relevant for a slice if one of the
-- following three conditions holds:
--     * The slice uses the instances class and type
--     * The slice uses the instances class and the type is builtin
--     * The slice uses the instances type and the class is builtin
relevantInstances :: [Slice] -> Map SliceID [InstanceID]
relevantInstances slices = Map.fromList (do
    let sliceMap = Map.fromList (do
            slice@(Slice sliceID _ _ _ _) <- slices
            return (sliceID,slice))
    Slice sliceID _ _ _ _ <- slices
    let usedInstances = do
            usedSliceID <- transitivelyUsedSlices sliceMap sliceID
            Slice _ _ _ _ instances <- maybeToList (Map.lookup usedSliceID sliceMap)
            instances
        usedClassInstanceIDs = Set.fromList (do
            Instance OfClass instanceID <- usedInstances
            return instanceID)
        usedClassInstanceBuiltinTypeIDs = Set.fromList (do
            Instance OfClassForBuiltinType instanceID <- usedInstances
            return instanceID)
        usedTypeInstanceIDs = Set.fromList (do
            Instance ForType instanceID <- usedInstances
            return instanceID)
        usedTypeInstanceBuiltinClassIDs = Set.fromList (do
            Instance ForTypeOfBuiltinClass instanceID <- usedInstances
            return instanceID)
        instanceIDs = Set.toList (Set.union
            (Set.union usedClassInstanceBuiltinTypeIDs usedTypeInstanceBuiltinClassIDs)
            (Set.intersection usedClassInstanceIDs usedTypeInstanceIDs))
    return (sliceID,instanceIDs))


-- | Takes a 'Map' from slice ID to Slice and a slice ID. Returns the list of
-- slices the slice with the given slice ID transitively uses.
transitivelyUsedSlices :: Map SliceID Slice -> SliceID -> [SliceID]
transitivelyUsedSlices sliceMap sliceID =
    execState (transitivelyUsedSlicesStatefully sliceMap sliceID) []


-- | Finds the list of transitively used slices. Statefully keeps the list of
-- already visited slices in a list.
transitivelyUsedSlicesStatefully :: Map SliceID Slice -> SliceID -> State [SliceID] ()
transitivelyUsedSlicesStatefully sliceMap sliceID = do
    seenSliceIDs <- get
    unless (elem sliceID seenSliceIDs) (do
        put (sliceID : seenSliceIDs)
        let slice = fromMaybe sliceNotFoundError (Map.lookup sliceID sliceMap)
            sliceNotFoundError = error "transitivelyUsedSlicesStatefully: slice not found"
        forM_ (usedSliceIDs slice ++ sliceInstanceIDs slice) (
            transitivelyUsedSlicesStatefully sliceMap))


-- | Directory for generated modules
sliceModuleDirectory :: FilePath
sliceModuleDirectory = "fragnix" </> "temp" </> "compilationunits"

-- | The path where we put a generated module for GHC to find it.
modulePath :: Module -> FilePath
modulePath (Module _ (ModuleName moduleName) _ _ _ _ _) =
    sliceModuleDirectory </> moduleName <.> "hs"

-- | The path where we put a generated boot module for GHC to find it.
moduleHSBootPath :: Module -> FilePath
moduleHSBootPath (Module _ (ModuleName moduleName) _ _ _ _ _) =
    sliceModuleDirectory </> moduleName <.> "hs-boot"

-- | The path for the module generated for the slice with the given ID
sliceModulePath :: SliceID -> FilePath
sliceModulePath sliceID = sliceModuleDirectory </> sliceModuleName sliceID <.> "hs"

-- | The name we give to the module generated for a slice with the given ID.
sliceModuleName :: SliceID -> String
sliceModuleName sliceID = "F" ++ show sliceID

-- | The module name of the module that reexports all instances.
allInstancesModuleName :: ModuleName
allInstancesModuleName = ModuleName "ALLINSTANCES"

-- | Is the module name from a fragnix generated module
isSliceModule :: ModuleName -> Bool
isSliceModule (ModuleName ('F':rest)) = all isDigit rest
isSliceModule _ = False

-- | Does the import import a constructor
isConstructorImport :: ImportDecl -> Bool
isConstructorImport (ImportDecl _ _ _ _ _ _ _ (Just (False,[IThingWith _ _]))) = True
isConstructorImport _ = False


writeModule :: Module -> IO ()
writeModule modul = writeFileStrictIfMissing (modulePath modul) (prettyPrint modul)


writeHSBoot :: Module -> IO ()
writeHSBoot modul = writeFileStrictIfMissing (moduleHSBootPath modul) (addRoleAnnotation (prettyPrint modul))


writeAllInstancesModule :: Module -> IO ()
writeAllInstancesModule modul = writeFileStrict (modulePath modul) (prettyPrint modul)


writeAllInstancesHSBoot :: Module -> IO ()
writeAllInstancesHSBoot modul = writeFileStrict (moduleHSBootPath modul) (prettyPrint modul)


-- | Write out the given string to the given file path but only if
-- there is no file at the given path.
writeFileStrictIfMissing :: FilePath -> String -> IO ()
writeFileStrictIfMissing filePath content = do
    exists <- doesFileExist filePath
    unless exists (writeFileStrict filePath content)


-- | Write the given string to the given file path but completely evaluate it before
-- doing so and print any exceptions.
writeFileStrict :: FilePath -> String -> IO ()
writeFileStrict filePath content = (do
    evaluatedContent <- evaluate (pack content)
    writeFile filePath evaluatedContent)
        `catch` (print :: SomeException -> IO ())


-- | Given a slice ID load all slices and all instance slices nedded
-- for compilation.
loadSlicesTransitive :: SliceID -> IO ([Slice], [Slice])
loadSlicesTransitive sliceID = do
    sliceIDs <- loadSliceIDsTransitive sliceID
    slices <- forM sliceIDs readSliceDefault
    return (partitionInstances slices)


-- | Given a list of slices partition it into non-instance
-- slices and instance slices.
partitionInstances :: [Slice] -> ([Slice], [Slice])
partitionInstances = partition (not . isInstance)


-- | Given a slice ID find all IDs of all the slices needed
-- for compilation.
loadSliceIDsTransitive :: SliceID -> IO [SliceID]
loadSliceIDsTransitive sliceID = execStateT (loadSliceIDsStateful sliceID) []


-- | Given a slice ID load all IDs of all the slices needed for
-- compilation. Keep track of visited slice IDs to avoid loops.
loadSliceIDsStateful :: SliceID -> StateT [SliceID] IO ()
loadSliceIDsStateful sliceID = do
    seenSliceIDs <- get
    unless (elem sliceID seenSliceIDs) (do
        put (sliceID : seenSliceIDs)
        slice <- liftIO (readSliceDefault sliceID)
        let recursiveSliceIDs = usedSliceIDs slice
            recursiveInstanceSliceIDs = sliceInstanceIDs slice
        forM_ recursiveSliceIDs loadSliceIDsStateful
        forM_ recursiveInstanceSliceIDs loadSliceIDsStateful)


usedSliceIDs :: Slice -> [SliceID]
usedSliceIDs (Slice _ _ _ uses _) = do
    Use _ _ (OtherSlice sliceID) <- uses
    return sliceID

sliceInstanceIDs :: Slice -> [InstanceID]
sliceInstanceIDs (Slice _ _ _ _ instances) = do
    Instance _ instanceID <- instances
    return instanceID

isInstance :: Slice -> Bool
isInstance (Slice _ (Language _ True) _ _ _) = True
isInstance _ = False

doesSliceModuleExist :: SliceID -> IO Bool
doesSliceModuleExist sliceID = doesFileExist (sliceModulePath sliceID)


-- | Sadly GHC generates different role annotations in hs-boot files than
-- in regular modules.
-- See ghc.haskell.org/trac/ghc/ticket/9204
-- Will be fixed in GHC 7.12
addRoleAnnotation :: String -> String
addRoleAnnotation code = case Map.lookup (last (lines code)) roleAnnotations of
    Nothing -> code
    Just roleAnnotation -> unlines [
        "{-# LANGUAGE RoleAnnotations #-}",
        code,
        roleAnnotation]

roleAnnotations :: Map String String
roleAnnotations = Map.fromList [
    ("data UArray i e = UArray !i !i !Int ByteArray#",
        "type role UArray representational phantom"),
    ("data StorableArray i e = StorableArray !i !i Int !(ForeignPtr e)",
        "type role StorableArray representational phantom"),
    ("data STUArray s i e = STUArray !i !i !Int (MutableByteArray# s)",
        "type role STUArray nominal representational phantom"),
    ("newtype IOUArray i e = IOUArray (STUArray RealWorld i e)",
        "type role IOUArray representational phantom"),
    ("newtype Constant a b = Constant{getConstant :: a}",
        "type role Constant representational phantom"),
    ("                           {-# UNPACK #-} !(ForeignPtr a)",
        "type role MVector phantom phantom"),
    ("                           {-# UNPACK #-} !(MutableByteArray s)",
        "type role MVector nominal phantom"),
    ("                       {-# UNPACK #-} !(ForeignPtr a)",
        "type role Vector phantom"),
    ("                       {-# UNPACK #-} !ByteArray",
        "type role Vector phantom"),
    ("newtype Vault s = Vault (Map Unique Any)",
        "type role Vault phantom"),
    ("newtype Tagged s b = Tagged{unTagged :: b}",
        "type role Tagged phantom representational"),
    ("newtype Tagged2 (s :: * -> *) b = Tagged2{unTagged2 :: b}",
        "type role Tagged2 phantom representational"),
    ("newtype Tagged (s :: * -> *) = Tagged{unTagged :: Int}",
        "type role Tagged phantom")]

