module Day7.Advanced where

import Data.List (sort, sortOn)
import qualified Data.Ord
import Day7.Common (Hand (Hand), HandType (FiveOAK), findHandType, parse, score)
import Utils.IO (loadInput)
import Utils.Lists (frequencies)

letterToInt :: Char -> Int
letterToInt 'A' = 14
letterToInt 'K' = 13
letterToInt 'Q' = 12
letterToInt 'J' = 1
letterToInt 'T' = 10
letterToInt d = read [d]

upgradeType :: Hand -> Hand
upgradeType (Hand cards score typ)
  -- Five of a kind can never be upgraded as it's already the highest, also this handles
  -- an edge case where there are no Js in the hand.
  | typ == FiveOAK = Hand cards score FiveOAK
  -- Replace the J's by the highest occurring (non-J) number in the hand.
  | otherwise = Hand cards score $ findHandType $ map (\c -> if c == 1 then replacement else c) cards
  where
    nonJs = sortOn (Data.Ord.Down . snd) $ frequencies $ filter (/= 1) cards
    replacement = fst $ head nonJs

main :: IO ()
main = loadInput >>= print . sum . zipWith (curry (\p -> fst p * score (snd p))) [1 ..] . sort . map upgradeType . parse letterToInt
