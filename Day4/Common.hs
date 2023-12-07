module Day4.Common where

import Data.Set (Set)
import qualified Data.Set as Set
import Utils.Parser
  ( Parser,
    digits,
    doParse,
    some,
    string,
    token,
    whitespace,
    (<|>),
  )

newtype Card = Card {correctNumbers :: Int} deriving (Eq, Show)

-- Parsing

parse :: String -> [Card]
parse = doParse parser
  where
    parser = do some parseDraw

parseDraw :: Parser Card
parseDraw = do
  string "Card"
  whitespace
  digits
  token ':'
  result <- parseNumbers
  string " |"
  bet <- parseNumbers
  token '\n'
  return $ Card $ Set.size (Set.intersection bet result)

parseNumbers :: Parser (Set String)
parseNumbers = parseMore <|> parseLast
  where
    parseMore = do whitespace; num <- digits; Set.insert num <$> parseNumbers
    parseLast = do whitespace; Set.singleton <$> digits
