module Day8.Advanced where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Utils.IO (loadInput)
import Utils.Parser (Parser, chars, doParse, some, string, token, until)
import Prelude hiding (until)

type Network = Map String (String, String)

type Navigation = (String, Network)

parse :: String -> Navigation
parse = doParse parser
  where
    parser = do
      insns <- until '\n'
      token '\n'
      network <- some parseEdge
      return (take (length insns - 1) insns, Map.fromList $ filter (not . looping) network)
      where
        -- Preliminary pruning: remove combinations such as (AAA -> (AAA, AAA)) that are not worth storing.
        looping (o, (l, r)) = l == r && o == l

parseEdge :: Parser (String, (String, String))
parseEdge = do
  origin <- chars 3
  string " = ("
  left <- chars 3
  string ", "
  right <- chars 3
  string ")\n"

  return (origin, (left, right))

walk :: Navigation -> Int
walk nav = walk' (filter (\k -> last k == 'A') $ Map.keys (snd nav)) (cycle (fst nav)) (snd nav)
  where
    turn :: Char -> (String, String) -> String
    turn 'L' (l, _) = l
    turn 'R' (_, r) = r

    walk' :: [String] -> String -> Network -> Int
    walk' heres (p : path) map | all (\p -> last p == 'Z') heres = 0
    walk' heres (p : path) m = 1 + walk' (map (\h -> turn p $ fromJust $ Map.lookup h m) heres) path m

main :: IO ()
main = loadInput >>= print . walk . parse
