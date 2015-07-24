module Fragnix.SliceCompiler where

import Fragnix.Slice (
    Slice(Slice),SliceID,Language(Language),Fragment(Fragment),Use(Use),
    Reference(OtherSlice,Builtin),
    UsedName(ValueName,TypeName,ConstructorName),Name(Identifier,Operator),
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

import Control.Monad.Trans.State.Strict (StateT, execStateT, get, put)
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import qualified Data.Map as Map(fromList,lookup)
import Control.Monad (forM,forM_,unless)
import Data.Maybe (mapMaybe)
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
    let sliceModules = map sliceModule slices
        sliceHSBoots = map bootModule sliceModules
        instanceSliceModules = map sliceModule instances
        instanceSliceHSBoots = map bootInstanceModule instanceSliceModules
    forM_ (sliceModules ++ instanceSliceModules ++ [allInstancesModule instances]) writeModule
    forM_ (sliceHSBoots ++ instanceSliceHSBoots ++ [bootModule (allInstancesModule instances)]) writeHSBoot


-- | Given a slice generate the corresponding module.
sliceModule :: Slice -> Module
sliceModule (Slice sliceID language fragment uses _) =
    let Fragment declarations = fragment
        decls = map (parseDeclaration sliceID ghcextensions) declarations
        moduleName = ModuleName (sliceModuleName sliceID)
        Language ghcextensions = language
        languagepragmas = [Ident "NoImplicitPrelude"] ++ (map (Ident . unpack) ghcextensions)
        pragmas = [LanguagePragma noLoc languagepragmas]
        imports = map useImport uses ++ [allInstancesImport]
        -- We need an export list to export the data family even
        -- though a slice only contains the data family instance
        exports = dataFamilyInstanceExports decls imports
    in Module noLoc moduleName pragmas Nothing exports imports decls


-- | Reparse a declrations from its textual representation in a slice.
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
        qualified = maybe False (const True) maybeQualification
        maybeAlias = fmap (ModuleName . unpack) maybeQualification
        importSpec = case usedName of
            ValueName name -> [IVar NoNamespace (toName name)]
            TypeName name -> [IAbs (toName name)]
            ConstructorName typeName name ->
                [IThingWith (toName typeName) [ConName (toName name)]]
        toName (Identifier name) = Ident (unpack name)
        toName (Operator name) = Symbol (unpack name)

    in ImportDecl noLoc moduleName qualified False False Nothing maybeAlias (Just (False,importSpec))


-- | Import Decl for an instance from the given SliceID.
instanceImport :: SliceID -> ImportDecl
instanceImport sliceID =
    ImportDecl noLoc (ModuleName (sliceModuleName sliceID)) False False False Nothing Nothing Nothing


-- | Import the special module that reexports all instances.
allInstancesImport :: ImportDecl
allInstancesImport =
    ImportDecl noLoc allInstancesModuleName False True False Nothing Nothing Nothing


-- | We export every type that we import in a data family instance slice
dataFamilyInstanceExports :: [Decl] -> [ImportDecl] -> Maybe [ExportSpec]
dataFamilyInstanceExports [DataInsDecl _ _ _ _ _] imports = Just (do
    ImportDecl _ _ _ _ _ _ _ (Just (False,[IAbs typeName])) <- imports
    return (EThingAll (UnQual typeName)))
dataFamilyInstanceExports [GDataInsDecl _ _ _ _ _ _] imports = Just (do
    ImportDecl _ _ _ _ _ _ _ (Just (False,[IAbs typeName])) <- imports
    return (EThingAll (UnQual typeName)))
dataFamilyInstanceExports _ _ = Nothing


-- | Given a list of instance slices generates a module that imports
-- (and therefore reexports) all those instances.
allInstancesModule :: [Slice] -> Module
allInstancesModule slices =
    Module noLoc allInstancesModuleName [] Nothing Nothing instanceImports [] where
        instanceImports = do
            Slice sliceID _ _ _ _ <- slices
            return (instanceImport sliceID)


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
writeModule modul = writeFileStrict (modulePath modul) (prettyPrint modul)


writeHSBoot :: Module -> IO ()
writeHSBoot modul = writeFileStrict (moduleHSBootPath modul) (addRoleAnnotation (prettyPrint modul))


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
    (sliceIDs, instanceSliceIDs) <- loadSliceIDsTransitive sliceID
    slices <- forM sliceIDs readSliceDefault
    instances <- forM instanceSliceIDs readSliceDefault
    return (slices, instances)


-- | Given a slice ID find all IDs of slices and instances needed
-- for compilation. The first list in the pair are the slices the second
-- list are the instances.
loadSliceIDsTransitive :: SliceID -> IO ([SliceID],[SliceID])
loadSliceIDsTransitive sliceID = execStateT (loadSliceIDsStateful sliceID) ([],[])


-- | Given a slice ID of a non-instance slice load all IDs of slices
-- and instances needed for
-- compilation. Keep track of visited slice IDs to avoid loops.
loadSliceIDsStateful :: SliceID -> StateT ([SliceID],[SliceID]) IO ()
loadSliceIDsStateful sliceID = do
    (seenSliceIDs, seenInstanceSliceIDs) <- get
    unless (elem sliceID seenSliceIDs) (do
        put (sliceID : seenSliceIDs, seenInstanceSliceIDs)
        recurseIntoSlice sliceID)

-- | Given a slice ID of an instances slice find all IDs of slices and
-- instances needed for compilation. Keep track of alread visited slice
-- IDs to avoid loops.
loadInstanceSliceIDsStateful :: SliceID -> StateT ([SliceID],[SliceID]) IO ()
loadInstanceSliceIDsStateful instanceSliceID = do
    (seenSliceIDs, seenInstanceSliceIDs) <- get
    unless (elem instanceSliceID seenInstanceSliceIDs) (do
        put (seenSliceIDs, instanceSliceID : seenInstanceSliceIDs)
        recurseIntoSlice instanceSliceID)

-- | After we have made sure that we have not yet visited a slice ID and
-- after we have added it to a list of already visited slices we recurse
-- into slices that this slice uses and instances that this slice brings
-- into scope.
recurseIntoSlice :: SliceID -> StateT ([SliceID],[SliceID]) IO ()
recurseIntoSlice sliceID = do
    slice <- liftIO (readSliceDefault sliceID)
    let recursiveSliceIDs = usedSlices slice
        recursiveInstanceSliceIDs = instanceSlices slice
    forM_ recursiveSliceIDs loadSliceIDsStateful
    forM_ recursiveInstanceSliceIDs loadInstanceSliceIDsStateful

usedSlices :: Slice -> [SliceID]
usedSlices (Slice _ _ _ uses _) = [sliceID | Use _ _ (OtherSlice sliceID) <- uses]

instanceSlices :: Slice -> [SliceID]
instanceSlices (Slice _ _ _ _ instances) = instances

doesSliceModuleExist :: SliceID -> IO Bool
doesSliceModuleExist sliceID = doesFileExist (sliceModulePath sliceID)


-- | Sadly GHC generated different role annotations in hs-boot files from
-- the ones generated in regular modules.
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
        "type role Vault phantom")]

