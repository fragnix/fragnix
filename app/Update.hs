{-# LANGUAGE OverloadedStrings #-}
module Update
  ( update
  , UpdateCommand
  , updateParser
  ) where

import Fragnix.Slice (
  Slice(Slice), SliceID,
  readSlice, writeSlice, moduleNameSliceID, loadSliceIDsTransitive)
import Fragnix.Update (
  Update, apply, diff, getUpdates, PersistedUpdate(..),
  UpdateID, createUpdate, writeUpdate, findUpdateFuzzy)
import Fragnix.LocalSlice (
  LocalSlice(LocalSlice))
import Fragnix.HashLocalSlices (hashLocalSlices)
import Fragnix.Environment (loadEnvironment, persistEnvironment)
import Fragnix.Paths (environmentPath, slicesPath)

import Options.Applicative (
  Parser, subparser, command, info, progDesc, metavar, helper, (<**>), argument, str)
import Language.Haskell.Exts.Syntax (ModuleName(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Map (Map)
import qualified Data.Map as Map (
  lookup, fromList, toList, elems, empty, intersectionWith)
import Control.Monad.Trans.State.Strict (execState)

import Control.Monad (forM, forM_, guard)
import Data.Maybe (fromMaybe, maybeToList)
import Data.List (nub)
import Language.Haskell.Names (Environment, Symbol(..))


data UpdateCommand
  = List
  | Create Text SliceID SliceID
  | Apply UpdateID
  | Diff FilePath FilePath Text

update :: UpdateCommand -> IO ()
update List = do
  Text.putStrLn "Available updates:"
  updates <- getUpdates
  forM_ updates $ \update -> do
    Text.putStr (Text.justifyLeft 25 ' ' (updateID update))
    Text.putStrLn (updateDescription update)
update (Create description sliceID1 sliceID2) = do
  let persistedUpdate = createUpdate description [(sliceID1, sliceID2)]
  writeUpdate persistedUpdate
update (Apply desc) = do
  maybeUpdate <- findUpdateFuzzy desc
  case maybeUpdate of
    Nothing -> putStrLn "Couldn't find an update matching the description."
    Just upd -> applyUpdate upd
update (Diff environmentPath1 environmentPath2 description) = do
  environment1 <- loadEnvironment environmentPath1
  environment2 <- loadEnvironment environmentPath2
  sliceIDs1 <- loadEnvironmentSliceIDs environment1
  sliceIDs2 <- loadEnvironmentSliceIDs environment2
  let sliceIDs = nub (sliceIDs1 ++ sliceIDs2)
  slices <- forM sliceIDs (readSlice slicesPath)
  let slicesMap = sliceMap slices
  let update = nub (concat (Map.elems (Map.intersectionWith (\symbols1 symbols2 -> do
        symbol1 <- symbols1
        symbol2 <- symbols2
        guard (symbolName symbol1 == symbolName symbol2)
        let ModuleName () moduleName1 = symbolModule symbol1
        let ModuleName () moduleName2 = symbolModule symbol2
        sliceID1 <- maybeToList (moduleNameSliceID moduleName1)
        sliceID2 <- maybeToList (moduleNameSliceID moduleName2)
        diff slicesMap sliceID1 sliceID2) environment1 environment2)))
  let persistedUpdate = createUpdate description update
  writeUpdate persistedUpdate

applyUpdate :: PersistedUpdate -> IO ()
applyUpdate persistedUpdate = do
  putStrLn ("Applying update: " <> (Text.unpack $ updateDescription persistedUpdate))
  let PersistedUpdate _ _ update = persistedUpdate
  environment <- loadEnvironment environmentPath
  sliceIDs <- loadEnvironmentSliceIDs environment
  slices <- forM sliceIDs (readSlice slicesPath)
  let slicesMap = sliceMap slices
  let sliceIDLocalSliceMap = flip execState Map.empty (do
        forM_ slices (\(Slice sliceID _ _ _ _) -> apply slicesMap update sliceID))
  let localSlices = Map.elems sliceIDLocalSliceMap
  let (localSliceIDMap, newSlices) = hashLocalSlices localSlices
  let derivedUpdate = do
        (sliceID, LocalSlice localSliceID _ _ _ _) <- Map.toList sliceIDLocalSliceMap
        let newSliceID = fromMaybe (error "no") (Map.lookup localSliceID localSliceIDMap)
        return (sliceID, newSliceID)
  let environment' = updateEnvironment (update ++ derivedUpdate) environment
  forM_ newSlices (\slice -> writeSlice slicesPath slice)
  persistEnvironment environmentPath environment'

loadEnvironmentSliceIDs :: Environment -> IO [SliceID]
loadEnvironmentSliceIDs environment = do
  let sliceIDs = do
        symbols <- Map.elems environment
        symbol <- symbols
        let ModuleName () moduleName = symbolModule symbol
        maybeToList (moduleNameSliceID moduleName)
  loadSliceIDsTransitive slicesPath sliceIDs


-- | Build up a map from slice ID to corresponding slice for better lookup.
sliceMap :: [Slice] -> Map SliceID Slice
sliceMap slices = Map.fromList (do
    slice@(Slice sliceID _ _ _ _) <- slices
    return (sliceID, slice))

-- | Apply all the replacements contained in the update to the environment.
updateEnvironment :: Update -> Environment -> Environment
updateEnvironment update environment = (fmap . fmap) updateSymbol environment
  where
    updateSymbol :: Symbol -> Symbol
    updateSymbol s = let sliceID = case (symbolModule s) of (ModuleName _ n) -> Text.pack (tail n) in
          case lookup sliceID update of
            Nothing -> s
            Just sliceID' -> s { symbolModule = ModuleName () ('F' : Text.unpack sliceID') }



updateParser :: Parser UpdateCommand
updateParser = subparser (mconcat [
    command "list"   (info (listParser   <**> helper) (progDesc "List all available updates.")),
    command "create" (info (createParser <**> helper) (progDesc "Create a new update.")),
    command "apply"  (info (applyParser  <**> helper) (progDesc "Apply the specified update.")),
    command "diff"  (info (diffParser    <**> helper) (progDesc "Diff the two environments, producing an update with the name."))])
  where
    listParser = pure List
    createParser = Create
      <$> argument str (metavar "DESC")
      <*> argument str (metavar "SLICEID")
      <*> argument str (metavar "SLICEID")
    applyParser = Apply <$> argument str (metavar "ID/DESC")
    diffParser = Diff
      <$> argument str (metavar "ENVPATH")
      <*> argument str (metavar "ENVPATH")
      <*> argument str (metavar "DESC")
