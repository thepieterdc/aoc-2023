module Day8.Advanced where

import Data.Foldable (fold)
import Data.List (foldl1')
import qualified Data.Map as Map
import Day8.Common (Navigation, parse, walk)
import Utils.IO (loadInput)
import Utils.Lists (endsWith)

run :: Navigation -> [Int]
run nav = map (\s -> walk s (endsWith 'Z') nav) starts
  where
    starts = filter (endsWith 'A') $ Map.keys $ snd nav

main :: IO ()
main = loadInput >>= print . foldl1' lcm . run . parse
