{-# LANGUAGE OverloadedStrings #-}
module Update
  ( update
  , UpdateCommand
  , updateParser
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Control.Monad (forM_)
import Fragnix.Slice (SliceID)
import Fragnix.Update (getUpdates, PersistedUpdate(..), UpdateID, applyUpdate, createUpdate, writeUpdate, findUpdateFuzzy)

import Options.Applicative (
  ParserInfo, Parser, execParser,
  subparser, command, info, infoOption, long, help,
  progDesc, header, metavar, helper, (<**>),
  many, strArgument, flag, auto, fullDesc, argument, str)

data UpdateCommand
  = List
  | Create Text SliceID SliceID
  | Apply UpdateID

update :: UpdateCommand -> IO ()
update List = do
  Text.putStrLn "Available updates:"
  updates <- getUpdates
  forM_ updates $ \update -> do
    Text.putStr (Text.justifyLeft 25 ' ' (updateID update))
    Text.putStrLn (updateDescription update)
update (Create desc sliceID sliceID') = do
  let update = createUpdate desc [(sliceID, sliceID')]
  writeUpdate update
update (Apply desc) = do
  maybeUpdate <- findUpdateFuzzy desc
  case maybeUpdate of
    Nothing -> putStrLn "Couldn't find an update matching the description."
    Just upd -> applyUpdate upd


updateParser :: Parser UpdateCommand
updateParser = subparser (mconcat [
    command "list"   (info (listParser   <**> helper) (progDesc "List all available updates.")),
    command "create" (info (createParser <**> helper) (progDesc "Create a new update.")),
    command "apply"  (info (applyParser  <**> helper) (progDesc "Apply the specified update."))])
  where
    listParser = pure List
    createParser = Create
      <$> argument str (metavar "DESC")
      <*> argument str (metavar "SLICEID")
      <*> argument str (metavar "SLICEID")
    applyParser = Apply <$> argument str (metavar "ID/DESC")
