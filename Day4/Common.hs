module Day4.Common where
import           Data.Set     (Set)
import qualified Data.Set     as Set
import           Utils.Parser (Parser, digits, doParse, some, string, token,
                               whitespace, (<|>))


data Card = Card { identifier :: Int, result :: [Int], bet :: [Int] } deriving (Eq, Show)

-- Parsing

parse :: String -> [Card]
parse = doParse parser where
    parser = do some parseDraw

parseDraw :: Parser Card
parseDraw = do {identifier <- parseIdentifier; result <- parseNumbers; string " |"; bet <- parseNumbers; token '\n'; return $ Card identifier result bet} where
    parseIdentifier = do {string "Card"; whitespace; id <- digits; token ':'; return (read id :: Int)}

parseNumbers :: Parser [Int]
parseNumbers = parseMore <|> parseLast where
    parseMore = do {whitespace; num <- digits; rest <- parseNumbers; return $ (read num :: Int) : rest}
    parseLast = do {whitespace; num <- digits; return [read num :: Int]}

-- Functions

matchingNumbers :: Card -> Int
matchingNumbers card = Set.size (Set.intersection (Set.fromList (bet card)) (Set.fromList (result card)))
