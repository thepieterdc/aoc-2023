module Day8.Simple where

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

-- walk :: Navigation -> Int
walk nav = walk' "AAA" (cycle (fst nav)) (snd nav)
  where
    turn :: Char -> (String, String) -> String
    turn 'L' (l, _) = l
    turn 'R' (_, r) = r

    walk' :: String -> String -> Network -> Int
    walk' here (p : path) map | here == "ZZZ" = 0
    walk' here (p : path) map = 1 + walk' (turn p $ fromJust $ Map.lookup here map) path map

main :: IO ()
main = loadInput >>= print . walk . parse
