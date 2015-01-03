{-# LINE 1 "./Test/Framework/Runners/TestPattern.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                                   






                                    






                          






                                






                          






                                






                        






                                






                      






                        






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Test/Framework/Runners/TestPattern.hs" #-}
{-# LINE 1 "./Test/Framework/Runners/TestPattern.hs" #-}
module Test.Framework.Runners.TestPattern (
        TestPattern, parseTestPattern, testPatternMatches
    ) where

import Test.Framework.Utilities

import Text.Regex.Posix.Wrap
import Text.Regex.Posix.String()

import Data.List


data Token = SlashToken
           | WildcardToken
           | DoubleWildcardToken
           | LiteralToken Char
           deriving (Eq)

tokenize :: String -> [Token]
tokenize ('/':rest)     = SlashToken : tokenize rest
tokenize ('*':'*':rest) = DoubleWildcardToken : tokenize rest
tokenize ('*':rest)     = WildcardToken : tokenize rest
tokenize (c:rest)       = LiteralToken c : tokenize rest
tokenize []             = []


data TestPatternMatchMode = TestMatchMode
                          | PathMatchMode

data TestPattern = TestPattern {
        tp_categories_only :: Bool,
        tp_negated :: Bool,
        tp_match_mode :: TestPatternMatchMode,
        tp_tokens :: [Token]
    }

instance Read TestPattern where
    readsPrec _ string = [(parseTestPattern string, "")]

parseTestPattern :: String -> TestPattern
parseTestPattern string = TestPattern {
        tp_categories_only = categories_only,
        tp_negated = negated,
        tp_match_mode = match_mode,
        tp_tokens = tokens''
    }
  where
    tokens = tokenize string
    (negated, tokens')
      | (LiteralToken '!'):rest <- tokens = (True, rest)
      | otherwise                         = (False, tokens)
    (categories_only, tokens'')
      | (prefix, [SlashToken]) <- splitAt (length tokens' - 1) tokens' = (True, prefix)
      | otherwise                                                      = (False, tokens')
    match_mode
      | SlashToken `elem` tokens = PathMatchMode
      | otherwise                = TestMatchMode


testPatternMatches :: TestPattern -> [String] -> Bool
testPatternMatches test_pattern path = not_maybe $ any (=~ tokens_regex) things_to_match
  where
    not_maybe | tp_negated test_pattern = not
              | otherwise               = id
    path_to_consider | tp_categories_only test_pattern = dropLast 1 path
                     | otherwise                       = path
    tokens_regex = buildTokenRegex (tp_tokens test_pattern)
    
    things_to_match = case tp_match_mode test_pattern of
        -- See if the tokens match any single path component
        TestMatchMode -> path_to_consider
        -- See if the tokens match any prefix of the path
        PathMatchMode -> map pathToString $ inits path_to_consider


buildTokenRegex :: [Token] -> String
buildTokenRegex [] = []
buildTokenRegex (token:tokens) = concat (firstTokenToRegex token : map tokenToRegex tokens)
  where
    firstTokenToRegex SlashToken = "^"
    firstTokenToRegex other = tokenToRegex other
      
    tokenToRegex SlashToken = "/"
    tokenToRegex WildcardToken = "[^/]*"
    tokenToRegex DoubleWildcardToken = "*"
    tokenToRegex (LiteralToken lit) = regexEscapeChar lit

regexEscapeChar :: Char -> String
regexEscapeChar c | c `elem` "\\*+?|{}[]()^$." = '\\' : [c]
                  | otherwise                  = [c]

pathToString :: [String] -> String
pathToString path = "/" ++ concat (intersperse "/" path)
