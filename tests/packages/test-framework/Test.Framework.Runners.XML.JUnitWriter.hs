{-# LINE 1 "./Test/Framework/Runners/XML/JUnitWriter.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                                   






                                    






                          






                                






                          






                                






                        






                                






                      






                        






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Test/Framework/Runners/XML/JUnitWriter.hs" #-}
{-# LINE 1 "./Test/Framework/Runners/XML/JUnitWriter.hs" #-}
module Test.Framework.Runners.XML.JUnitWriter (
        RunDescription(..),
        serialize,



    ) where

import Test.Framework.Core (TestName)
import Test.Framework.Runners.Core (RunTest(..), FinishedTest)

import Data.List  ( intercalate )
import Data.Maybe ( fromMaybe )
import Text.XML.Light ( ppTopElement, unqual, unode
                      , Attr(..), Element(..) )


-- | An overall description of the test suite run.  This is currently
-- styled after the JUnit xml.  It contains records that are not yet
-- used, however, it provides a sensible structure to populate as we
-- are able, and the serialiazation code behaves as though these are
-- filled.
data RunDescription = RunDescription {
    errors :: Int -- ^ The number of tests that triggered error
                  -- conditions (unanticipated failures)
  , failedCount :: Int        -- ^ Count of tests that invalidated stated assertions.
  , skipped :: Maybe Int      -- ^ Count of tests that were provided but not run.
  , hostname :: Maybe String  -- ^ The hostname that ran the test suite.
  , suiteName :: String       -- ^ The name of the test suite.
  , testCount :: Int          -- ^ The total number of tests provided.
  , time :: Double            -- ^ The total execution time for the test suite.
  , timeStamp :: Maybe String -- ^ The time stamp that identifies when this run happened.
  , runId :: Maybe String     -- ^ Included for completness w/ junit.
  , package :: Maybe String   -- ^ holdover from Junit spec. Could be
                              -- used to specify the module under test.
  , tests :: [FinishedTest]   -- ^ detailed description and results for each test run.
  } deriving (Show)


-- | Serializes a `RunDescription` value to a `String`.
serialize :: Bool -> RunDescription -> String
serialize nested = ppTopElement . toXml nested

-- | Maps a `RunDescription` value to an XML Element
toXml :: Bool -> RunDescription -> Element
toXml nested runDesc = unode "testsuite" (attrs, morph_cases (tests runDesc))
  where
    morph_cases | nested    = map morphNestedTestCase
                | otherwise = concatMap (morphFlatTestCase [])

    -- | Top-level attributes for the first @testsuite@ tag.
    attrs :: [Attr]
    attrs = map (\(x,f)->Attr (unqual x) (f runDesc)) fields
    fields = [ ("errors",    show . errors)
             , ("failures",  show . failedCount)
             , ("skipped",   fromMaybe "" . fmap show . skipped)
             , ("hostname",  fromMaybe "" . hostname)
             , ("name",      id . suiteName)
             , ("tests",     show . testCount)
             , ("time",      show . time)
             , ("timestamp", fromMaybe "" . timeStamp)
             , ("id",        fromMaybe "" . runId)
             , ("package",   fromMaybe "" . package)
             ]

morphFlatTestCase :: [String] -> FinishedTest -> [Element]
morphFlatTestCase path (RunTestGroup gname testList)
  = concatMap (morphFlatTestCase (gname:path)) testList
morphFlatTestCase path (RunTest tName _ res) = [morphOneTestCase cName tName res]
  where cName | null path = "<none>"
              | otherwise = intercalate "." (reverse path)

morphNestedTestCase :: FinishedTest -> Element
morphNestedTestCase (RunTestGroup gname testList) =
  unode "testsuite" (attrs, map morphNestedTestCase testList)
  where attrs = [ Attr (unqual "name") gname ]
morphNestedTestCase (RunTest tName _ res) = morphOneTestCase "" tName res

morphOneTestCase :: String -> TestName -> (String, Bool) -> Element
morphOneTestCase cName tName (tout, pass) = case pass of
  True  -> unode "testcase" caseAttrs
  False -> unode "testcase" (caseAttrs, unode "failure" (failAttrs, tout))
  where caseAttrs = [ Attr (unqual "name") tName
                    , Attr (unqual "classname") cName
                    , Attr (unqual "time") ""
                    ]
        failAttrs = [ Attr (unqual "message") ""
                    , Attr (unqual "type") ""
                    ]
