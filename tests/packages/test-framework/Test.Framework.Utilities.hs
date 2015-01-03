{-# LINE 1 "./Test/Framework/Utilities.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                                   






                                    






                          






                                






                          






                                






                        






                                






                      






                        






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Test/Framework/Utilities.hs" #-}
{-# LINE 1 "./Test/Framework/Utilities.hs" #-}
module Test.Framework.Utilities where

import Control.Arrow (first, second)

import Data.Function (on)
import Data.Maybe
import Data.Monoid
import Data.List (intercalate)


newtype K a = K { unK :: a }


secondsToMicroseconds :: Num a => a -> a
secondsToMicroseconds = (1000000*)

microsecondsToPicoseconds :: Num a => a -> a
microsecondsToPicoseconds = (1000000*)

listToMaybeLast :: [a] -> Maybe a
listToMaybeLast = listToMaybe . reverse

mappendBy :: Monoid b => (a -> b) -> a -> a -> b
mappendBy f = mappend `on` f

orElse :: Maybe a -> a -> a
orElse = flip fromMaybe

onLeft :: (a -> c) -> (a, b) -> (c, b)
onLeft = first

onRight :: (b -> c) -> (a, b) -> (a, c)
onRight = second

-- | Like 'unlines', but does not append a trailing newline if there
-- is at least one line.  For example:
--
-- > unlinesConcise ["A", "B"] == "A\nB"
-- > unlinesConcise [] == ""
--
-- Whereas:
--
-- > unlines ["A", "B"] == "A\nB\n"
-- > unlines [] == ""
--
-- This is closer to the behaviour of 'unwords', which does not append
-- a trailing space.
unlinesConcise :: [String] -> String
unlinesConcise = intercalate "\n"

mapAccumLM :: Monad m => (acc -> x -> m (acc, y)) -> acc -> [x] -> m (acc, [y])
mapAccumLM _ acc [] = return (acc, [])
mapAccumLM f acc (x:xs) = do
    (acc', y) <- f acc x
    (acc'', ys) <- mapAccumLM f acc' xs
    return (acc'', y:ys)

padRight :: Int -> String -> String
padRight desired_length s = s ++ replicate (desired_length - length s) ' '

dropLast :: Int -> [a] -> [a]
dropLast n = reverse . drop n . reverse
