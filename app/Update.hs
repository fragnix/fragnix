{-# LANGUAGE OverloadedStrings #-}
module Update
  ( update
  , UpdateCommand
  , updateParser
  ) where

import Fragnix.Slice (
  Slice(..), SliceID, Fragment(..), Reference(OtherSlice, Builtin),
  readSlice, writeSlice, moduleNameReference, sliceIDModuleName, loadSliceIDsTransitive)
import Fragnix.Update (
  apply, diff, getUpdates,
  createUpdate, writeUpdate, findUpdateFuzzy)
import Fragnix.HashLocalSlices (hashLocalSlices)
import Fragnix.Environment (loadEnvironment, persistEnvironment)
import Fragnix.Paths (environmentPath, slicesPath)
import Fragnix.Core.Update

import Options.Applicative (
  Parser, subparser, command, info, progDesc, metavar, helper, (<**>), argument, str)
import Language.Haskell.Exts.Syntax (ModuleName(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Map (Map)
import qualified Data.Map as Map (
  lookup, fromList, toList, elems, empty, intersectionWith)
import qualified Data.Set as Set (
  empty)
import Control.Monad.Trans.State.Strict (execStateT, evalState, runState)

import Control.Monad (forM, forM_, guard)
import Data.Maybe (fromMaybe)
import Data.List (nub)
import Language.Haskell.Names (Environment, Symbol(..))


data UpdateCommand
  = List
  | Create Text SliceID SliceID
  | Apply UpdateID
  | Inspect UpdateID
  | Diff FilePath FilePath Text

update :: UpdateCommand -> IO ()
update List = do
  Text.putStrLn "Available updates:"
  updates <- getUpdates
  forM_ updates $ \updt -> do
    Text.putStr (Text.justifyLeft 25 ' ' (updateID updt))
    Text.putStrLn (updateDescription updt)
update (Create description sliceID1 sliceID2) = do
  let persistedUpdate = createUpdate description [(sliceID1, sliceID2)]
  writeUpdate persistedUpdate
update (Apply desc) = do
  maybeUpdate <- findUpdateFuzzy desc
  case maybeUpdate of
    Nothing -> putStrLn "Couldn't find an update matching the description."
    Just upd -> applyUpdate upd
update (Inspect desc) = do
  maybeUpdate <- findUpdateFuzzy desc
  case maybeUpdate of
    Nothing -> putStrLn "Couldn't find an update matching the description."
    Just upd -> do
      Text.putStrLn $ "Description: " <> updateDescription upd
      Text.putStrLn $ "Hash:        " <> updateID upd
      Text.putStrLn $ " ---"
      forM_ (updateContent upd) $ \(sliceId, sliceId') -> do
        Text.putStrLn $ "Replacing content from slice: " <> sliceId <> "\n"
        (Slice _ _ (Fragment code) _ _) <- readSlice slicesPath sliceId
        Text.putStrLn (Text.unlines code)
        Text.putStrLn $ "By content from slice:        " <> sliceId' <> "\n"
        (Slice _ _ (Fragment code') _ _) <- readSlice slicesPath sliceId'
        Text.putStrLn (Text.unlines code')
        Text.putStrLn $ " ---"
update (Diff environmentPath1 environmentPath2 description) = do
  environment1 <- loadEnvironment environmentPath1
  environment2 <- loadEnvironment environmentPath2
  sliceIDs1 <- loadEnvironmentSliceIDs environment1
  sliceIDs2 <- loadEnvironmentSliceIDs environment2
  let sliceIDs = nub (sliceIDs1 ++ sliceIDs2)
  slices <- forM sliceIDs (readSlice slicesPath)
  let slicesMap = sliceMap slices
  let updt = nub (concat (Map.elems (Map.intersectionWith (\symbols1 symbols2 -> do
        symbol1 <- symbols1
        symbol2 <- symbols2
        guard (symbolName symbol1 == symbolName symbol2)
        let ModuleName () moduleName1 = symbolModule symbol1
        let ModuleName () moduleName2 = symbolModule symbol2
        OtherSlice sliceID1 <- [moduleNameReference moduleName1]
        OtherSlice sliceID2 <- [moduleNameReference moduleName2]
        evalState (diff slicesMap sliceID1 sliceID2) Set.empty) environment1 environment2)))
  let persistedUpdate = createUpdate description updt
  writeUpdate persistedUpdate

applyUpdate :: PersistedUpdate -> IO ()
applyUpdate persistedUpdate = do
  putStrLn ("Applying update: " <> (Text.unpack $ updateDescription persistedUpdate))
  let PersistedUpdate _ _ updt = persistedUpdate
  environment <- loadEnvironment environmentPath
  sliceIDs <- loadEnvironmentSliceIDs environment
  slices <- forM sliceIDs (readSlice slicesPath)
  let slicesMap = sliceMap slices
  let (sliceIDLocalSliceIDMap, localSlices) =
        flip runState [] (flip execStateT Map.empty (do
          forM_ slices (\(Slice sliceId _ _ _ _) -> apply slicesMap updt sliceId)))
  let (localSliceIDMap, newSlices) = hashLocalSlices localSlices
  let derivedUpdate = do
        (sliceId, localSliceID) <- Map.toList sliceIDLocalSliceIDMap
        let newSliceID = fromMaybe (error "no") (Map.lookup localSliceID localSliceIDMap)
        return (sliceId, newSliceID)
  let environment' = updateEnvironment (updt ++ derivedUpdate) environment
  forM_ newSlices (\slice -> writeSlice slicesPath slice)
  persistEnvironment environmentPath environment'

loadEnvironmentSliceIDs :: Environment -> IO [SliceID]
loadEnvironmentSliceIDs environment = do
  let sliceIDs = do
        symbols <- Map.elems environment
        symbol <- symbols
        let ModuleName () moduleName = symbolModule symbol
        OtherSlice sliceId <- [moduleNameReference moduleName]
        return sliceId
  loadSliceIDsTransitive slicesPath sliceIDs


-- | Build up a map from slice ID to corresponding slice for better lookup.
sliceMap :: [Slice] -> Map SliceID Slice
sliceMap slices = Map.fromList (do
    slice@(Slice sliceId _ _ _ _) <- slices
    return (sliceId, slice))

-- | Apply all the replacements contained in the update to the environment.
updateEnvironment :: Update -> Environment -> Environment
updateEnvironment updt environment = (fmap . fmap) updateSymbol environment
  where
    updateSymbol :: Symbol -> Symbol
    updateSymbol symbol = case symbolModule symbol of
      ModuleName () moduleName -> case moduleNameReference moduleName of
        OtherSlice sliceId -> case lookup sliceId updt of
          Nothing -> symbol
          Just sliceID' -> symbol { symbolModule = ModuleName () (sliceIDModuleName sliceID') }
        Builtin _ -> symbol

updateParser :: Parser UpdateCommand
updateParser = subparser (mconcat [
    command "list"   (info (listParser   <**> helper) (progDesc "List all available updates.")),
    command "create" (info (createParser <**> helper) (progDesc "Create a new update.")),
    command "apply"  (info (applyParser  <**> helper) (progDesc "Apply the specified update.")),
    command "inspect"  (info (inspectParser  <**> helper) (progDesc "Inspect the specified update.")),
    command "diff"  (info (diffParser    <**> helper) (progDesc "Diff the two environments, producing an update with the name."))])
  where
    listParser = pure List
    createParser = Create
      <$> argument str (metavar "DESC")
      <*> argument str (metavar "SLICEID")
      <*> argument str (metavar "SLICEID")
    applyParser = Apply <$> argument str (metavar "ID/DESC")
    inspectParser = Inspect <$> argument str (metavar "ID/DESC")
    diffParser = Diff
      <$> argument str (metavar "ENVPATH")
      <*> argument str (metavar "ENVPATH")
      <*> argument str (metavar "DESC")
