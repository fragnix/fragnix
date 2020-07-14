module Main where

import Build (build)

import Options.Applicative (
  ParserInfo, Parser, execParser,
  subparser, command, info,
  progDesc, header, metavar, helper, (<**>),
  many, argument, str, auto, fullDesc)


data Command = Build [FilePath]

commandParserInfo :: ParserInfo Command
commandParserInfo =
  info (commandParser  <**> helper) (fullDesc <> header "fragnix - fragment-based code distribution")

commandParser :: Parser Command
commandParser = subparser (mconcat [
  command "build" (info (buildParser <**> helper) (
    progDesc "Build the given list of modules."))
  ])

buildParser :: Parser Command
buildParser = Build <$> many (argument str (metavar "TARGET"))


main :: IO ()
main = do
   command <- execParser commandParserInfo
   case command of
     Build modulePaths -> build modulePaths
