module Main where

import Update (update, UpdateCommand, updateParser)
import Build (
  build,ShouldPreprocess(DoPreprocess,NoPreprocess),
  ShouldDist(ShouldCompile,ShouldDist))
import CreateEnv (createEnv)
import GraphViz (visualize)
import Paths_fragnix
import Data.Version (showVersion)

import Options.Applicative (
  ParserInfo, Parser, execParser,
  subparser, command, info, infoOption, long, short, help, value,
  progDesc, header, metavar, helper, (<**>),
  many, strArgument, strOption, flag, auto, fullDesc)


data Command
  = Build ShouldPreprocess [FilePath]
  | CreateEnv
  | Update UpdateCommand
  | Visualize
  | Dist FilePath ShouldPreprocess [FilePath]

commandParserInfo :: ParserInfo Command
commandParserInfo =
  info (commandParser  <**> helper <**> versionOption) (fullDesc <> header "fragnix - fragment-based code distribution")

commandParser :: Parser Command
commandParser = subparser (mconcat [
    command "build" (info (buildParser <**> helper) (progDesc "Build all Haskell files in the given files and directories and their subdirectories.")),
    command "create-env" (info (createEnvParser <**> helper) (progDesc "Create the builtin environment.")),
    command "update" (info ((Update <$> updateParser) <**> helper) (progDesc "List all available updates.")),
    command "visualize" (info (visualizeParser <**> helper) (progDesc "Visualize the current environment.")),
    command "dist" (info (distParser <**> helper) (progDesc "Serialize the environment that correspond to the given targets."))])

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


visualizeParser :: Parser Command
visualizeParser = pure Visualize

distParser :: Parser Command
distParser = Dist <$>
  strOption (short 'o' <> value "dist" <> metavar "DIR" <> help "Write output to directory DIR") <*>
  flag NoPreprocess DoPreprocess (long "preprocess" <> help "Run preprocessor on source files") <*>
  many (strArgument (metavar "TARGET"))


main :: IO ()
main = do
   command <- execParser commandParserInfo
   case command of
     Build shouldPreprocess paths -> build ShouldCompile shouldPreprocess paths
     CreateEnv -> createEnv
     Update cmd -> update cmd
     Visualize -> visualize
     Dist outputDirectory shouldPreprocess paths -> build (ShouldDist outputDirectory) shouldPreprocess paths

