module Fragnix.SliceCompiler
  ( writeSliceModules
  , invokeGHCMain
  ) where

import Fragnix.Slice (
    Slice(Slice),SliceI(SliceI),SliceID,Language(Language),Fragment(Fragment),Use(Use),
    Reference(OtherSlice,Builtin),
    UsedName(ValueName,TypeName,ConstructorName),Name(Identifier,Operator),
    InstanceID,Instance(Instance),
    readSlice)
import Fragnix.SliceInstanceResolution ( resolveSlices)

import Prelude hiding (writeFile)

import Language.Haskell.Exts.Syntax (
    Module(Module),ModuleHead(ModuleHead),
    ModuleName(ModuleName),ModulePragma(LanguagePragma),
    Decl(InstDecl,DataDecl,GDataDecl,PatBind,FunBind,
         ForImp,DerivDecl,TypeSig,TypeInsDecl,DataInsDecl,GDataInsDecl),
    Name(Ident,Symbol),QName(UnQual),
    ExportSpecList(ExportSpecList),ExportSpec(EThingWith),EWildcard(EWildcard),
    ImportSpecList(ImportSpecList),ImportDecl(ImportDecl,importSrc,importModule),
    ImportSpec(IVar,IAbs,IThingWith),
    CName(ConName),Namespace(NoNamespace))
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

import Control.Monad.Trans.State.Strict (StateT,execStateT,get,put)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM,forM_,unless)
import Data.Maybe (mapMaybe, isJust)
import Data.Char (isDigit)


-- | Compile the slice with the given slice ID. Set verbosity to zero and
-- turn all warnings off.
-- Assumes that all necessary compilation units have been written to disk.
invokeGHC :: SliceID -> IO ExitCode
invokeGHC sliceID = rawSystem "ghc" [
    "-v0","-w",
    "-ifragnix/temp/compilationunits",
    sliceModulePath sliceID]

-- | Invoke GHC to compile the slice with the given ID. The slice with the
-- given ID has to contain a definition for 'main :: IO ()'.
-- Asssumes that all necessary compilation units have been generated.
invokeGHCMain :: SliceID -> IO ExitCode
invokeGHCMain sliceID = rawSystem "ghc" ([
    "-o","main",
    "-ifragnix/temp/compilationunits",
    "-main-is",sliceModuleName sliceID,
    "-Ifragnix/include"] ++
    (map (\filename -> "fragnix/cbits/" ++ filename) cFiles) ++ [
    "-lpthread","-lz","-lutil",
    sliceModulePath sliceID])


-- | Generate and write all modules necessary to compile the slice with the given ID.
writeSliceModules :: SliceID -> IO ()
writeSliceModules sliceID = do
    createDirectoryIfMissing True sliceModuleDirectory
    slices <- loadSlicesTransitive sliceID
    let slicesI = resolveSlices slices
        sliceModules = map sliceModule slicesI
        sliceHSBoots = map bootModule sliceModules
    forM_ sliceModules writeModule
    forM_ sliceHSBoots writeHSBoot

-- | Given a slice generate the corresponding module.
sliceModule :: SliceI -> Module ()
sliceModule (SliceI (Slice sliceID language fragment uses _) instanceIDs) =
    let Fragment declarations = fragment
        moduleHead = ModuleHead () moduleName Nothing maybeExportSpecList
        pragmas = [LanguagePragma () languagepragmas]
        imports =
            map useImport uses ++
            map instanceImport instanceIDs
        decls = map (parseDeclaration sliceID ghcextensions) declarations
        moduleName = ModuleName () (sliceModuleName sliceID)
        -- We need an export list to export the data family even
        -- though a slice only contains the data family instance
        maybeExportSpecList = dataFamilyInstanceExports decls imports
        languagepragmas =
            [Ident () "NoImplicitPrelude"] ++
            (map (Ident () . unpack) ghcextensions)
        Language ghcextensions = language
    in Module () (Just moduleHead) pragmas imports decls


-- | Reparse a declaration from its textual representation in a slice.
parseDeclaration :: SliceID -> [Text] -> Text -> Decl ()
parseDeclaration sliceID ghcextensions declaration = fmap (const ()) decl where
    -- Workaround because HSE couldn't parse all standalone declarations.
    Module _ _ _ _ [decl] = fromParseResult (parseModuleWithMode parseMode (unpack declaration))
    parseMode = defaultParseMode {
        parseFilename = show sliceID,
        extensions = map (parseExtension . unpack) ghcextensions,
        fixities = Just baseFixities}


-- | Import decl for a given use of a symbol.
useImport :: Use -> ImportDecl ()
useImport (Use maybeQualification usedName symbolSource) =
    let moduleName = case symbolSource of
            OtherSlice sliceID -> ModuleName () (sliceModuleName sliceID)
            Builtin originalModule -> ModuleName () (unpack originalModule)
        qualified = isJust maybeQualification
        maybeAlias = fmap (ModuleName () . unpack) maybeQualification
        importSpec = case usedName of
            ValueName name ->
                IVar () (toName name)
            TypeName name ->
                IAbs () (NoNamespace ()) (toName name)
            ConstructorName typeName name ->
                IThingWith () (toName typeName) [ConName () (toName name)]
        importSpecList = Just (ImportSpecList () False [importSpec])
        toName (Identifier name) = Ident () (unpack name)
        toName (Operator name) = Symbol () (unpack name)

    in ImportDecl () moduleName qualified False False Nothing maybeAlias importSpecList


-- | Import Decl for an instance from the given SliceID. Instance imports
-- are source imports.
instanceImport :: SliceID -> ImportDecl ()
instanceImport sliceID =
    ImportDecl () (ModuleName () (sliceModuleName sliceID)) False True False Nothing Nothing Nothing


-- | We export every type that we import in a data family instance slice
dataFamilyInstanceExports :: [Decl a] -> [ImportDecl ()] -> Maybe (ExportSpecList ())
dataFamilyInstanceExports [DataInsDecl _ _ _ _ _] imports =
    Just (ExportSpecList () (do
        ImportDecl _ _ _ _ _ _ _ (Just importSpecList) <- imports
        ImportSpecList _ False [IAbs _ _ typeName] <- [importSpecList]
        return (EThingWith () (EWildcard () 0) (UnQual () typeName) [])))
dataFamilyInstanceExports [GDataInsDecl _ _ _ _ _ _] imports =
    Just (ExportSpecList () (do
        ImportDecl _ _ _ _ _ _ _ (Just importSpecList) <- imports
        ImportSpecList _ False [IAbs _ _ typeName] <- [importSpecList]
        return (EThingWith () (EWildcard () 0) (UnQual () typeName) [])))
dataFamilyInstanceExports _ _ =
    Nothing


-- | Create a boot module from an ordinary module.
bootModule :: Module a -> Module a
bootModule (Module annotation maybeModuleHead pragmas imports decls) =
    Module annotation maybeModuleHead pragmas bootImports bootDecls where
        bootImports = mapMaybe bootImport imports
        bootDecls = concatMap bootDecl decls
bootModule _ = error "XML module not supported."


-- | Remove all source imports and make all other imports source except those
-- from builtin modules. The removal of source imports means that no instance
-- modules will be imported.
bootImport :: ImportDecl a -> Maybe (ImportDecl a)
bootImport importDecl
    | importSrc importDecl = Nothing
    | isSliceModule (importModule importDecl) = Just (importDecl {importSrc = True})
    | otherwise = Just importDecl


-- | Remove instance bodies, make derived instances into bodyless instance decls,
-- remove value bindings and generate a type signature for foreign imports.
bootDecl :: Decl a -> [Decl a]
bootDecl (InstDecl annotation overlap instRule _) =
    [InstDecl annotation overlap instRule Nothing]
bootDecl (DataDecl annotation dataOrNew context declHead constructors _) =
    [DataDecl annotation dataOrNew context declHead constructors []]
bootDecl (GDataDecl annotation dataOrNew context declHead kind constructors _) =
    [GDataDecl annotation dataOrNew context declHead kind constructors []]
bootDecl (DerivDecl annotation _ overlap instRule) = -- Ignoring the deriving strategy.
    [InstDecl annotation overlap instRule Nothing]
bootDecl (PatBind _ _ _ _) =
    []
bootDecl (FunBind _ _) =
    []
bootDecl (ForImp srcloc _ _ _ name typ) =
    [TypeSig srcloc [name] typ]
bootDecl (TypeInsDecl _ _ _) =
    []
bootDecl (DataInsDecl _ _ _ _ _) =
    []
bootDecl (GDataInsDecl _ _ _ _ _ _) =
    []
bootDecl decl =
    [decl]



-- | Directory for generated modules
sliceModuleDirectory :: FilePath
sliceModuleDirectory = "fragnix" </> "temp" </> "compilationunits"

-- | The path where we put a generated module for GHC to find it.
modulePath :: Module a -> FilePath
modulePath (Module _ (Just (ModuleHead _ (ModuleName _ moduleName) _ _)) _ _ _) =
    sliceModuleDirectory </> moduleName <.> "hs"
modulePath (Module _ Nothing _ _ _) =
    sliceModuleDirectory </> "Main" <.> "hs"
modulePath _ =
    error "XML module not supported."

-- | The path where we put a generated boot module for GHC to find it.
moduleHSBootPath :: Module a -> FilePath
moduleHSBootPath (Module _ (Just (ModuleHead _ (ModuleName _ moduleName) _ _)) _ _ _) =
    sliceModuleDirectory </> moduleName <.> "hs-boot"
moduleHSBootPath (Module _ Nothing _ _ _) =
    sliceModuleDirectory </> "Main" <.> "hs-boot"
moduleHSBootPath _ =
    error "XML module not supported."

-- | The path for the module generated for the slice with the given ID
sliceModulePath :: SliceID -> FilePath
sliceModulePath sliceID = sliceModuleDirectory </> sliceModuleName sliceID <.> "hs"

-- | The name we give to the module generated for a slice with the given ID.
sliceModuleName :: SliceID -> String
sliceModuleName sliceID = "F" ++ unpack sliceID

-- | The module name of the module that reexports all instances.
allInstancesModuleName :: ModuleName ()
allInstancesModuleName = ModuleName () "ALLINSTANCES"

-- | Is the module name from a fragnix generated module
isSliceModule :: ModuleName a -> Bool
isSliceModule (ModuleName _ ('F':rest)) = all isDigit rest
isSliceModule _ = False


writeModule :: Module a -> IO ()
writeModule modul = writeFileStrictIfMissing (modulePath modul) (prettyPrint modul)


writeHSBoot :: Module a -> IO ()
writeHSBoot modul = writeFileStrictIfMissing (moduleHSBootPath modul) (prettyPrint modul)


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
loadSlicesTransitive :: SliceID -> IO [Slice]
loadSlicesTransitive sliceID = do
    sliceIDs <- loadSliceIDsTransitive sliceID
    forM sliceIDs readSlice


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
        slice <- liftIO (readSlice sliceID)
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

doesSliceModuleExist :: SliceID -> IO Bool
doesSliceModuleExist sliceID = doesFileExist (sliceModulePath sliceID)


-- | List of gathered C-files.
cFiles :: [FilePath]
cFiles = [
    "ancilData.c",
    "conv.c",
    "fnv.c",
    "HsNet.c",
    "HsUnix.c",
    "itoa.c",
    "primitive-memops.c",
    "text-helper.c",
    "zlib-helper.c",
    "cbits.c",
    "execvpe.c",
    "fpstring.c",
    "HsTime.c",
    "HsUnixCompat.c",
    "myfree.c",
    "runProcess.c",
    "timeUtils.c"]
