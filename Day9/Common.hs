module Day9.Common where

import Utils.Parser (Parser, doParse, eol, integer, some, whitespace, (<|>))

parse :: String -> [[Int]]
parse = doParse parser
  where
    parser = do some parseSeq

parseSeq :: Parser [Int]
parseSeq = parseMore <|> parseLast
  where
    parseMore = do num <- integer; some whitespace; rest <- parseSeq; return $ num : rest
    parseLast = do num <- integer; eol; return [num]

nextNumber :: [Int] -> Int
nextNumber nums | all (== 0) nums = 0
nextNumber nums = last nums + nextNumber diffs
  where
    -- Calculate the pairwise differences.
    diffs = [(nums !! (i + 1)) - (nums !! i) | i <- [0 .. length nums - 2]]
