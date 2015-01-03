{-# LINE 1 "./Test/Framework/Runners/Console/Run.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                                   






                                    






                          






                                






                          






                                






                        






                                






                      






                        






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Test/Framework/Runners/Console/Run.hs" #-}
{-# LINE 1 "./Test/Framework/Runners/Console/Run.hs" #-}
module Test.Framework.Runners.Console.Run (
        showRunTestsTop
    ) where

import Test.Framework.Core
import Test.Framework.Improving
import Test.Framework.Runners.Console.Colors
import Test.Framework.Runners.Console.ProgressBar
import Test.Framework.Runners.Console.Statistics
import Test.Framework.Runners.Console.Utilities
import Test.Framework.Runners.Core
import Test.Framework.Runners.Statistics
import Test.Framework.Runners.TimedConsumption
import Test.Framework.Utilities

import System.Console.ANSI
import System.IO

import Text.PrettyPrint.ANSI.Leijen

import Data.Monoid (mempty)

import Control.Arrow (second, (&&&))
import Control.Monad (unless)


showRunTestsTop :: Bool -> Bool -> [RunningTest] -> IO [FinishedTest]
showRunTestsTop isplain hide_successes running_tests = (if isplain then id else hideCursorDuring) $ do
    -- Show those test results to the user as we get them. Gather statistics on the fly for a progress bar
    let test_statistics = initialTestStatistics (totalRunTestsList running_tests)
    (test_statistics', finished_tests) <- showRunTests isplain hide_successes 0 test_statistics running_tests
    
    -- Show the final statistics
    putStrLn ""
    putDoc $ possiblyPlain isplain $ showFinalTestStatistics test_statistics'
    
    return finished_tests


-- This code all /really/ sucks.  There must be a better way to seperate out the console-updating
-- and the improvement-traversing concerns - but how?
showRunTest :: Bool -> Bool -> Int -> TestStatistics -> RunningTest -> IO (TestStatistics, FinishedTest)
showRunTest isplain hide_successes indent_level test_statistics (RunTest name test_type (SomeImproving improving_result)) = do
    let progress_bar = testStatisticsProgressBar test_statistics
    (property_text, property_suceeded) <- showImprovingTestResult isplain hide_successes indent_level name progress_bar improving_result
    return (updateTestStatistics (\count -> adjustTestCount test_type count mempty) property_suceeded test_statistics, RunTest name test_type (property_text, property_suceeded))
showRunTest isplain hide_successes indent_level test_statistics (RunTestGroup name tests) = do
    putDoc $ (indent indent_level (text name <> char ':')) <> linebreak
    fmap (second $ RunTestGroup name) $ showRunTests isplain hide_successes (indent_level + 2) test_statistics tests

showRunTests :: Bool -> Bool -> Int -> TestStatistics -> [RunningTest] -> IO (TestStatistics, [FinishedTest])
showRunTests isplain hide_successes indent_level = mapAccumLM (showRunTest isplain hide_successes indent_level)


testStatisticsProgressBar :: TestStatistics -> Doc
testStatisticsProgressBar test_statistics = progressBar (colorPassOrFail no_failures) terminal_width (Progress run_tests total_tests)
  where
    run_tests   = testCountTotal (ts_run_tests test_statistics)
    total_tests = testCountTotal (ts_total_tests test_statistics)
    no_failures = ts_no_failures test_statistics
    -- We assume a terminal width of 80, but we can't make the progress bar 80 characters wide.  Why?  Because if we
    -- do so, when we write the progress bar out Windows will move the cursor onto the next line!  By using a slightly
    -- smaller width we prevent this from happening.  Bit of a hack, but it does the job.
    terminal_width = 79


showImprovingTestResult :: TestResultlike i r => Bool -> Bool -> Int -> String -> Doc -> (i :~> r) -> IO (String, Bool)
showImprovingTestResult isplain hide_successes indent_level test_name progress_bar improving = do
    -- Consume the improving value until the end, displaying progress if we are not in "plain" mode
    (result, success) <- if isplain then return $ improvingLast improving'
                                    else showImprovingTestResultProgress (return ()) indent_level test_name progress_bar improving'
    unless (success && hide_successes) $ do
        let (result_doc, extra_doc) | success   = (brackets $ colorPass (text result), empty)
                                    | otherwise = (brackets (colorFail (text "Failed")), text result <> linebreak)
        
        -- Output the final test status and a trailing newline
        putTestHeader indent_level test_name (possiblyPlain isplain result_doc)
        -- Output any extra information that may be required, e.g. to show failure reason
        putDoc extra_doc

    return (result, success)
  where
    improving' = bimapImproving show (show &&& testSucceeded) improving

showImprovingTestResultProgress :: IO () -> Int -> String -> Doc -> (String :~> (String, Bool)) -> IO (String, Bool)
showImprovingTestResultProgress erase indent_level test_name progress_bar improving = do
    -- Update the screen every every 200ms
    improving_list <- consumeListInInterval 200000 (consumeImproving improving)
    case listToMaybeLast improving_list of
        Nothing         -> do -- 200ms was somehow not long enough for a single result to arrive: try again!
            showImprovingTestResultProgress erase indent_level test_name progress_bar improving
        Just improving' -> do -- Display that new improving value to the user
            showImprovingTestResultProgress' erase indent_level test_name progress_bar improving'

showImprovingTestResultProgress' :: IO () -> Int -> String -> Doc -> (String :~> (String, Bool)) -> IO (String, Bool)
showImprovingTestResultProgress' erase _ _ _ (Finished result) = do
    erase
    -- There may still be a progress bar on the line below the final test result, so 
    -- remove it as a precautionary measure in case this is the last test in a group
    -- and hence it will not be erased in the normal course of test display.
    putStrLn ""
    clearLine
    cursorUpLine 1
    return result
showImprovingTestResultProgress' erase indent_level test_name progress_bar (Improving intermediate rest) = do
    erase
    putTestHeader indent_level test_name (brackets (text intermediate))
    putDoc progress_bar
    hFlush stdout
    showImprovingTestResultProgress (cursorUpLine 1 >> clearLine) indent_level test_name progress_bar rest

possiblyPlain :: Bool -> Doc -> Doc
possiblyPlain True  = plain
possiblyPlain False = id

putTestHeader :: Int -> String -> Doc -> IO ()
putTestHeader indent_level test_name result = putDoc $ (indent indent_level (text test_name <> char ':' <+> result)) <> linebreak
