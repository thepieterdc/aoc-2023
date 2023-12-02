module Day2.Common where
import           Prelude      hiding (until)
import           Utils.Parser (Parser, doParse, integer, some, string, token,
                               until, (<|>))

-- Types.

data Cube = Blue | Green | Red deriving (Eq, Show)

type Pick = (Cube, Int)

type Set = [Pick]

data Game = Game { identifier :: Int, sets :: [Set] } deriving (Eq, Show)

-- Parsing.

parse :: String -> [Game]
parse = doParse parser where
    parser = do some parseGame

parseGame :: Parser Game
parseGame = do { until ' '; id <- integer; token ':'; sets <- parseSets; token '\n'; return $ Game id sets}

parseSets :: Parser [Set]
parseSets = parseMore <|> parseLast where
    parseMore = do {set <- parsePicks; token ';'; rest <- parseSets; return $ set : rest}
    parseLast = do {set <- parsePicks; return [set]}

parsePicks :: Parser [Pick]
parsePicks = parseMore <|> parseLast where
    parseMore = do {pick <- parsePick; token ','; rest <- parsePicks; return $ pick : rest}
    parseLast = do {pick <- parsePick; return [pick]}

parsePick :: Parser Pick
parsePick = do { token ' '; amt <- integer; token ' '; colour <- parseColour; return (colour, amt)} where
    parseColour = do {string "blue"; return Blue} <|> do {string "green"; return Green} <|> do {string "red"; return Red}
