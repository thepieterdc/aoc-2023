module Day14.Common where

import Data.List (intercalate, sortOn, transpose)
import Data.List.Split (keepDelimsL, oneOf, split)
import Data.Ord (Down (..))
import Utils.Grid (Grid)
import Utils.Parser (Parser, doParse, eol, some, token, (<|>))

-- Sorting order is inverted here because in Maybe's, Nothing is considered greater than Just.
data Rock = Round | Cube deriving (Eq, Ord, Show)

parse :: String -> Grid (Maybe Rock)
parse = doParse parser

parser :: Parser (Grid (Maybe Rock))
parser = do some parseLine

parseLine :: Parser [Maybe Rock]
parseLine = parseMore <|> parseLast
  where
    parseMore = do rock <- parseRock; rest <- parseLine; return $ rock : rest
    parseLast = do rock <- parseRock; eol; return [rock]

parseRock :: Parser (Maybe Rock)
parseRock = parseCube <|> parseRound <|> parseEmpty
  where
    parseCube = do token '#'; return $ Just Cube
    parseRound = do token 'O'; return $ Just Round
    parseEmpty = do token '.'; return Nothing

score :: Grid (Maybe Rock) -> Int
score g = sum $ zipWith (\i rs -> i * length (filter (== Just Round) rs)) [length g, (length g - 1) .. 1] g

tiltEast :: Grid (Maybe Rock) -> Grid (Maybe Rock)
tiltEast grid = map reverse $ tilt $ map reverse grid

tiltNorth :: Grid (Maybe Rock) -> Grid (Maybe Rock)
tiltNorth grid = transpose $ tilt $ transpose grid

tiltSouth :: Grid (Maybe Rock) -> Grid (Maybe Rock)
tiltSouth grid = reverse $ transpose $ tilt $ transpose $ reverse grid

tiltWest :: Grid (Maybe Rock) -> Grid (Maybe Rock)
tiltWest = tilt

tilt :: Grid (Maybe Rock) -> Grid (Maybe Rock)
tilt = map (\r -> intercalate [] (map (sortOn Data.Ord.Down) $ split (keepDelimsL $ oneOf [Just Cube]) r))
