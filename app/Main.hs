module Main where

import Build (build)
import Paths_fragnix
import Data.Version (showVersion)

import Options.Applicative (
  ParserInfo, Parser, execParser,
  subparser, command, info, infoOption, long, help,
  progDesc, header, metavar, helper, (<**>),
  many, argument, str, auto, fullDesc)


data Command = Build [FilePath]

commandParserInfo :: ParserInfo Command
commandParserInfo =
  info (commandParser  <**> helper <**> versionOption) (fullDesc <> header "fragnix - fragment-based code distribution")

commandParser :: Parser Command
commandParser = subparser (mconcat [
  command "build" (info (buildParser <**> helper) (
    progDesc "Build the given list of modules."))
  ])

versionOption :: Parser (a -> a)
versionOption = infoOption versionText (long "version" <> help "Show version")
  where
    versionText = "Version " <> showVersion version

buildParser :: Parser Command
buildParser = Build <$> many (argument str (metavar "TARGET"))


main :: IO ()
main = do
   command <- execParser commandParserInfo
   case command of
     Build modulePaths -> build modulePaths
