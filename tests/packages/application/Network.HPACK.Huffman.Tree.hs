{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Network/HPACK/Huffman/Tree.hs" #-}
{-# LANGUAGE BangPatterns #-}

module Network.HPACK.Huffman.Tree (
  -- * Huffman decoding
    HTree(..)
  , eosInfo
  , toHTree
  , showTree
  , printTree
  , flatten
  ) where

import Control.Arrow (second)
import Data.List (partition)
import Network.HPACK.Huffman.Bit
import Network.HPACK.Huffman.Params

----------------------------------------------------------------

type EOSInfo = Maybe Int

-- | Type for Huffman decoding.
data HTree = Tip
             !EOSInfo            -- EOS info from 1
             {-# UNPACK #-} !Int -- Decoded value. Essentially Word8
           | Bin
             !EOSInfo            -- EOS info from 1
             {-# UNPACK #-} !Int -- Sequence no from 0
             !HTree              -- Left
             !HTree              -- Right
           deriving Show

eosInfo :: HTree -> EOSInfo
eosInfo (Tip mx _)     = mx
eosInfo (Bin mx _ _ _) = mx

----------------------------------------------------------------

showTree :: HTree -> String
showTree = showTree' ""

showTree' :: String -> HTree -> String
showTree' _    (Tip _ i)     = show i ++ "\n"
showTree' pref (Bin _ n l r) = "No " ++ show n ++ "\n"
                            ++ pref ++ "+ " ++ showTree' pref' l
                            ++ pref ++ "+ " ++ showTree' pref' r
  where
    pref' = "  " ++ pref

printTree :: HTree -> IO ()
printTree = putStr . showTree

----------------------------------------------------------------

-- | Creating 'HTree'.
toHTree :: [Bits] -> HTree
toHTree bs = mark 1 eos $ snd $ build 0 $ zip [0..idxEos] bs
  where
    eos = bs !! idxEos

build :: Int -> [(Int,Bits)] -> (Int, HTree)
build !cnt0 [(v,[])] = (cnt0,Tip Nothing v)
build !cnt0 xs       = let (cnt1,l) = build (cnt0 + 1) fs
                           (cnt2,r) = build cnt1 ts
                       in (cnt2, Bin Nothing cnt0 l r)
  where
    (fs',ts') = partition ((==) F . head . snd) xs
    fs = map (second tail) fs'
    ts = map (second tail) ts'

-- | Marking the EOS path
mark :: Int -> Bits -> HTree -> HTree
mark i []     (Tip Nothing v)     = Tip (Just i) v
mark i (F:bs) (Bin Nothing n l r) = Bin (Just i) n (mark (i+1) bs l) r
mark i (T:bs) (Bin Nothing n l r) = Bin (Just i) n l (mark (i+1) bs r)
mark _ _      _                   = error "mark"

----------------------------------------------------------------

flatten :: HTree -> [HTree]
flatten (Tip _ _)       = []
flatten t@(Bin _ _ l r) = t : (flatten l ++ flatten r)
