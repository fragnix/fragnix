module Main where

import Update (update, UpdateCommand, updateParser)
import Build (
  build,ShouldPreprocess(DoPreprocess,NoPreprocess),
  ShouldDist(ShouldCompile,ShouldDist))
import CreateEnv (createEnv)
import Show (showInformation)
import Get (get)
import qualified Init (init)
import Utils (IDType(EnvID, SliceID))
import Paths_fragnix
import Data.Version (showVersion)
import Data.Text (pack)

import Options.Applicative (
  ParserInfo, Parser, execParser,
  subparser, command, info, infoOption, long, short, help, value,
  progDesc, header, metavar, helper, (<**>),
  many, strArgument, strOption, flag, auto, fullDesc, switch)


data Command
  = Build ShouldPreprocess [FilePath]
  | CreateEnv
  | Update UpdateCommand
  | Dist FilePath ShouldPreprocess [FilePath]
  | Get IDType String Bool
  | Show IDType String
  | Init


commandParserInfo :: ParserInfo Command
commandParserInfo =
  info (commandParser  <**> helper <**> versionOption) (fullDesc <> header "fragnix - fragment-based code distribution")

commandParser :: Parser Command
commandParser = subparser (mconcat [
    command "build" (info (buildParser <**> helper) (progDesc "Build all Haskell files in the given files and directories and their subdirectories.")),
    command "create-env" (info (createEnvParser <**> helper) (progDesc "Create the builtin environment.")),
    command "update" (info ((Update <$> updateParser) <**> helper) (progDesc "List all available updates.")),
    command "dist" (info (distParser <**> helper) (progDesc "Serialize the environment that correspond to the given targets.")),
    command "show" (info (showParser <**> helper) (progDesc "Show fragment information and metadata.")),
    command "get" (info (getParser <**> helper) (progDesc "Download a slice/environment into project.")),
    command "init" (info (initParser <**> helper) (progDesc "Initialize project folder structure and builtin environment."))])

versionOption :: Parser (a -> a)
versionOption = infoOption versionText (long "version" <> help "Show version")
  where
    versionText = "Version " <> showVersion version

buildParser :: Parser Command
buildParser = Build <$>
  flag NoPreprocess DoPreprocess (long "preprocess" <> help "Run preprocessor on source files") <*>
  many (strArgument (metavar "TARGET"))

createEnvParser :: Parser Command
createEnvParser = pure CreateEnv

distParser :: Parser Command
distParser = Dist <$>
  strOption (short 'o' <> value "dist" <> metavar "DIR" <> help "Write output to directory DIR") <*>
  flag NoPreprocess DoPreprocess (long "preprocess" <> help "Run preprocessor on source files") <*>
  many (strArgument (metavar "TARGET"))

showParser :: Parser Command
showParser = Show <$>
  flag SliceID EnvID (long "env" <> help "Treat ID as environment.") <*>
  strArgument (metavar "ID")

initParser :: Parser Command
initParser = pure Init

getParser :: Parser Command
getParser = Get <$>
  flag SliceID EnvID (long "env" <> help "Treat ID as environment.") <*>
  strArgument (metavar "NO-DEPS") <*>
  switch (long "no-deps" <> help "Don't download dependencies.")
  


main :: IO ()
main = do
   command <- execParser commandParserInfo
   case command of
     Build shouldPreprocess paths -> build ShouldCompile shouldPreprocess paths
     CreateEnv -> createEnv
     Update cmd -> update cmd
     Dist outputDirectory shouldPreprocess paths -> build (ShouldDist outputDirectory) shouldPreprocess paths
     Show idType id -> showInformation idType id
     Get idType id nodeps -> get idType id nodeps
     Init -> Init.init

