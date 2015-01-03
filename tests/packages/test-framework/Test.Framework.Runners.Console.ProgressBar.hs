{-# LINE 1 "./Test/Framework/Runners/Console/ProgressBar.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                                   






                                    






                          






                                






                          






                                






                        






                                






                      






                        






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Test/Framework/Runners/Console/ProgressBar.hs" #-}
{-# LINE 1 "./Test/Framework/Runners/Console/ProgressBar.hs" #-}
module Test.Framework.Runners.Console.ProgressBar (
        Progress(..), progressBar
    ) where

import Text.PrettyPrint.ANSI.Leijen hiding (width)


data Progress = Progress Int Int

progressBar :: (Doc -> Doc) -> Int -> Progress -> Doc
progressBar color width (Progress count total) = char '[' <> progress_doc <> space_doc <> char ']'
  where
    -- The available width takes account of the enclosing brackets
    available_width = width - 2
    characters_per_tick = fromIntegral available_width / fromIntegral total :: Double
    progress_chars = round (characters_per_tick * fromIntegral count)
    space_chars = available_width - progress_chars
    progress_doc = color (text (reverse (take progress_chars ('>' : repeat '='))))
    space_doc = text (replicate space_chars ' ')
