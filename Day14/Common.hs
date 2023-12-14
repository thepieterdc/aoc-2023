module Day14.Common where

import Data.List (intercalate, sortOn, transpose)
import Data.List.Split (keepDelimsL, oneOf, split)
import Data.Ord (Down (..))
import Utils.Parser (Parser, doParse, eol, some, token, (<|>))

-- Sorting order is inverted here because in Maybe's, Nothing is considered greater than Just.
data Rock = Round | Cube deriving (Eq, Ord, Show)

type Grid = [[Maybe Rock]]

parse :: String -> Grid
parse = doParse parser

parser :: Parser Grid
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

score :: Grid -> Int
score g = sum $ zipWith (\i rs -> i * length (filter (== Just Round) rs)) [length g, (length g - 1) .. 1] g

tiltEast :: Grid -> Grid
tiltEast grid = map reverse $ tilt $ map reverse grid

tiltNorth :: Grid -> Grid
tiltNorth grid = transpose $ tilt $ transpose grid

tiltSouth :: Grid -> Grid
tiltSouth grid = reverse $ transpose $ tilt $ transpose $ reverse grid

tiltWest :: Grid -> Grid
tiltWest = tilt

tilt :: Grid -> Grid
tilt = map (\r -> intercalate [] (map (sortOn Data.Ord.Down) $ split (keepDelimsL $ oneOf [Just Cube]) r))
