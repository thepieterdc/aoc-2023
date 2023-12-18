module Day18.Common where

import Utils.Grid (Coordinate, Direction (..), moves)
import Utils.Parser (Parser, doParse, some)

data Instruction = Instruction {dir :: Direction, amount :: Int} deriving (Eq, Show)

parse :: Parser Instruction -> String -> [Instruction]
parse insParser = doParse (some insParser)

findEdges :: Coordinate -> (Int, [Coordinate]) -> [Instruction] -> (Int, [Coordinate])
findEdges cursor (borderLength, seen) (insn : insns) = findEdges target (borderLength + amount insn, seen ++ [target]) insns
  where
    target = last $ moves (dir insn) (amount insn) cursor
findEdges _ ret [] = ret

shoelace :: [Coordinate] -> Int
shoelace vs = abs (sum $ [fst (vs !! i) * snd (vs !! ((i + 1) `mod` n)) - fst (vs !! ((i + 1) `mod` n)) * snd (vs !! i) | i <- [0 .. n - 1]]) `div` 2
  where
    n = length vs

solve :: (Int, [Coordinate]) -> Int
solve (borderLength, coords) = interiorLength + borderLength
  where
    a = shoelace coords
    interiorLength = a - (borderLength `div` 2) + 1
