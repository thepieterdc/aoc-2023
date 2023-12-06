module Day6.Common where

import           Data.List    (sortOn)
import           Data.Maybe   (fromMaybe)
import qualified Data.Set     as Set
import           Utils.Lists  (maybeHead)
import           Utils.Parser (Parser, digits, doParse, integer, some, string,
                               token, whitespace, (<|>))

data Race = Race { time :: Int, distance :: Int } deriving (Eq, Show)

parse :: String -> [Race]
parse = doParse parser where
    parser = do
        string "Time:";
        times <- parseNumbers;
        string "Distance:";
        distances <- parseNumbers;
        return $ [Race t d | (t, d) <- zip times distances]


parseNumbers :: Parser [Int]
parseNumbers = do {parseMore <|> parseLast} where
    parseMore = do {whitespace; num <- integer; rest <- parseNumbers; return (num : rest)}
    parseLast = do {whitespace; num <- integer; token '\n'; return [num]}
