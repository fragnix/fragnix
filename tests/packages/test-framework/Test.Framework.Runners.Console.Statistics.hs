{-# LINE 1 "./Test/Framework/Runners/Console/Statistics.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                                   






                                    






                          






                                






                          






                                






                        






                                






                      






                        






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Test/Framework/Runners/Console/Statistics.hs" #-}
{-# LINE 1 "./Test/Framework/Runners/Console/Statistics.hs" #-}
module Test.Framework.Runners.Console.Statistics (
        showFinalTestStatistics
    ) where

import Test.Framework.Runners.Statistics
import Test.Framework.Runners.Console.Colors
import Test.Framework.Runners.Console.Table

import Text.PrettyPrint.ANSI.Leijen

import Data.List


-- | Displays statistics as a string something like this:
--
-- @
--        Properties Total
-- Passed 9          9
-- Failed 1          1
-- Total  10         10
-- @
showFinalTestStatistics :: TestStatistics -> Doc
showFinalTestStatistics ts = renderTable $ [Column label_column] ++ (map Column test_type_columns) ++ [Column total_column]
  where
    test_types = sort $ testCountTestTypes (ts_total_tests ts)
    
    label_column      = [TextCell empty,              TextCell (text "Passed"),                        TextCell (text "Failed"),                  TextCell (text "Total")]
    total_column      = [TextCell (text "Total"),     testStatusTotal colorPass ts_passed_tests,       testStatusTotal colorFail ts_failed_tests, testStatusTotal (colorPassOrFail (ts_no_failures ts)) ts_total_tests]
    test_type_columns = [ [TextCell (text test_type), testStat colorPass (countTests ts_passed_tests), testStat colorFail failures,               testStat (colorPassOrFail (failures <= 0)) (countTests ts_total_tests)]
                        | test_type <- test_types
                        , let countTests = testCountForType test_type . ($ ts)
                              failures   = countTests ts_failed_tests ]
    
    testStatusTotal color status_accessor = TextCell (coloredNumber color (testCountTotal (status_accessor ts)))
    testStat color number = TextCell (coloredNumber color number)

coloredNumber :: (Doc -> Doc) -> Int -> Doc
coloredNumber color number
  | number == 0 = number_doc
  | otherwise   = color number_doc
  where
    number_doc = text (show number)
