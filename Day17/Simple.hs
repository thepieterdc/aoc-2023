module Day17.Simple where

import Utils.IO (loadInput)

parse :: String -> [[Int]]
parse input = map (map (\c -> read [c] :: Int)) $ lines input

main :: IO ()
main = loadInput >>= print . parse
