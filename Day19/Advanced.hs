module Day19.Advanced where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Day19.Common (Condition (GreaterThan, LessThan), Rating, Workflow (..), WorkflowStep (..), isAccepted, parse, score, solve)
import Utils.IO (loadInput)

type Range = (Int, Int)

findSplits :: Map Char (Set Int) -> WorkflowStep -> Map Char (Set Int)
findSplits r (Conditional c (GreaterThan n) _) = Map.insert c (Set.insert n $ r Map.! c) r
findSplits r (Conditional c (LessThan n) _) = Map.insert c (Set.insert (n - 1) $ r Map.! c) r
findSplits r _ = r

-- run :: Map String Workflow -> Int
run wfs = accepted
  where
    splits = foldl findSplits (Map.fromList $ map (,Set.fromList [0, 4000]) "xmas") $ concatMap steps $ Map.elems wfs
    ranges = Map.map splitsToRanges splits
    accepted = sum [(snd x - fst x + 1) * (snd m - fst m + 1) * (snd a - fst a + 1) * (snd s - fst s + 1) | x <- ranges Map.! 'x', m <- ranges Map.! 'm', a <- ranges Map.! 'a', s <- ranges Map.! 's', isAccepted wfs (Map.fromList [('x', fst x), ('m', fst m), ('a', fst a), ('s', fst s)])]

splitsToRanges :: Set Int -> [Range]
splitsToRanges s = [(a + 1, b) | (a, b) <- zip splits $ tail splits]
  where
    splits = Set.toAscList s

main :: IO ()
main = loadInput >>= print . run . fst . parse
