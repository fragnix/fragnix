{-# LINE 1 "./Test/Framework/Runners/XML.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                                   






                                    






                          






                                






                          






                                






                        






                                






                      






                        






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Test/Framework/Runners/XML.hs" #-}
{-# LINE 1 "./Test/Framework/Runners/XML.hs" #-}
module Test.Framework.Runners.XML (
        produceReport
    ) where

import Test.Framework.Runners.Statistics       ( testCountTotal, TestStatistics(..) )
import Test.Framework.Runners.Core             ( FinishedTest )
import Test.Framework.Runners.XML.JUnitWriter  ( RunDescription(..), serialize )

import Data.Time.Format    ( formatTime )
import Data.Time.LocalTime ( getZonedTime )

import System.Locale       ( defaultTimeLocale )

import Network.HostName    ( getHostName )


produceReport :: Bool -> TestStatistics -> [FinishedTest] -> IO String
produceReport nested test_statistics fin_tests = fmap (serialize nested) $ mergeResults test_statistics fin_tests


-- | Generates a description of the complete test run, given some
-- initial over-all test statistics and the list of tests that was
-- run.
--
-- This is only specific to the XML code because the console output
-- @Runner@ doesn't need this level of detail to produce summary
-- information, and the per-test details are generated during
-- execution.
--
-- This could be done better by using a State monad in the notifier
-- defined within `issueTests`.
mergeResults :: TestStatistics -> [FinishedTest] -> IO RunDescription
mergeResults test_statistics fin_tests = do
  host <- getHostName
  theTime <- getZonedTime
  return RunDescription {
            errors = 0                  -- not yet available
          , failedCount = testCountTotal (ts_failed_tests test_statistics) -- this includes errors
          , skipped = Nothing           -- not yet applicable
          , hostname = Just host
          , suiteName = "test-framework tests" -- not yet available
          , testCount = testCountTotal (ts_total_tests test_statistics)
          , time = 0.0                  -- We don't currently measure the test run time.
          , timeStamp = Just $ formatTime defaultTimeLocale "%a %B %e %k:%M:%S %Z %Y" theTime -- e.g. Thu May  6 22:09:10 BST 2010
          , runId = Nothing             -- not applicable
          , package = Nothing           -- not yet available
          , tests = fin_tests
          }
