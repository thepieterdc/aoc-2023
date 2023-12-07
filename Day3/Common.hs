module Day3.Common where

import Data.Char (isDigit)
import Data.Set (Set)
import qualified Data.Set as Set
import Utils.Grid (Coordinate)
import Utils.Parser (Parser, digit, doParse, some, spot, token, (<|>))

data Item = Part String | Gear | Symbol | Skip deriving (Eq, Show)

type Line = [Item]

parse :: String -> [Line]
parse = doParse parser
  where
    parser = do some parseLine

parseLine :: Parser Line
parseLine = do items <- some parseItem; token '\n'; return items

parseItem :: Parser Item
parseItem = parsePart <|> parseGear <|> parseSymbol <|> parseSkip
  where
    parseGear = do token '*'; return Gear
    parsePart = do d <- digit; return $ Part [d]
    parseSkip = do token '.'; return Skip
    parseSymbol = do
      spot (\c -> c /= '.' && c /= '*' && c /= '\n' && not (isDigit c))
      return Symbol

mergeWithCoordinates :: Int -> [Line] -> [[(Coordinate, Coordinate, Item)]]
mergeWithCoordinates row (x : xs) = process 1 (head x) (tail x) : mergeWithCoordinates (row + 1) xs
  where
    process :: Int -> Item -> [Item] -> [(Coordinate, Coordinate, Item)]
    process col (Part num) ((Part next) : parts) = process (col + 1) (Part (num ++ next)) parts
    process col (Part num) (next : parts) = ((row, col - length num), (row, col - 1), Part num) : process (col + 1) next parts
    process col (Part num) [] = [((row, col - length num), (row, col - 1), Part num)]
    process col Skip (next : parts) = process (col + 1) next parts
    process col item (next : parts) = ((row, col - 1), (row, col - 1), item) : process (col + 1) next parts
    process _ _ _ = []
mergeWithCoordinates _ [] = []
