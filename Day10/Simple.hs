module Day10.Simple where

import Utils.Grid (Coordinate)
import Utils.IO (loadInput)
import Utils.Lists (mapIdx)
import Utils.Parser (Parser, doParse, eol, some, token, whitespace, (<|>))

data Tile = BendNE | BendNW | BendSE | BendSW | Ground | Horizontal | Start | Vertical deriving (Eq, Show)

type Grid = [[Tile]]

parse :: String -> Grid
parse = doParse parser
  where
    parseRow = do ts <- some parseTile; eol; return ts
    parser = do some parseRow

parseTile :: Parser Tile
parseTile = bendNE <|> bendNW <|> bendSE <|> bendSW <|> ground <|> horiz <|> start <|> vert
  where
    bendNE = do token 'L'; return BendNE
    bendNW = do token 'J'; return BendNW
    bendSE = do token 'F'; return BendSE
    bendSW = do token '7'; return BendSW
    ground = do token '.'; return Ground
    horiz = do token '-'; return Horizontal
    start = do token 'S'; return Start
    vert = do token '|'; return Vertical

findStart :: Int -> Grid -> Coordinate
findStart r (row : rows) = result
  where
    matches = [c | (c, t) <- zip [0 ..] row, t == Start]
    result = if null matches then findStart (r + 1) rows else (r, head matches)
findStart _ [] = error "Start not found"

get :: Coordinate -> Grid -> Tile
get (r, c) grid = grid !! r !! c

move :: Coordinate -> Coordinate -> Tile -> Coordinate
move (prevR, prevC) (r, c) BendNE
  | (prevR, prevC) == (r, c + 1) = (r - 1, c)
  | (prevR, prevC) == (r - 1, c) = (r, c + 1)
  | otherwise = (r, c)
move (prevR, prevC) (r, c) BendNW
  | (prevR, prevC) == (r, c - 1) = (r - 1, c)
  | (prevR, prevC) == (r - 1, c) = (r, c - 1)
  | otherwise = (r, c)
move (prevR, prevC) (r, c) BendSE
  | (prevR, prevC) == (r + 1, c) = (r, c + 1)
  | (prevR, prevC) == (r, c + 1) = (r + 1, c)
  | otherwise = (r, c)
move (prevR, prevC) (r, c) BendSW
  | (prevR, prevC) == (r + 1, c) = (r, c - 1)
  | (prevR, prevC) == (r, c - 1) = (r + 1, c)
  | otherwise = (r, c)
move (prevR, prevC) (r, c) Horizontal
  | (prevR, prevC) == (r, c - 1) = (r, c + 1)
  | (prevR, prevC) == (r, c + 1) = (r, c - 1)
  | otherwise = (r, c)
move _ here Ground = here
move (prevR, prevC) (r, c) Vertical
  | (prevR, prevC) == (r - 1, c) = (r + 1, c)
  | (prevR, prevC) == (r + 1, c) = (r - 1, c)
  | otherwise = (r, c)
move prev c tile = error (show tile ++ show prev ++ show c)

replaceStart :: Coordinate -> Tile -> Grid -> Grid
replaceStart (r, c) t = mapIdx r (mapIdx c (const t))

solve :: Grid -> Int
solve grid = if odd solution then (solution + 1) `div` 2 else solution `div` 2
  where
    start@(sr, sc) = findStart 0 grid

    startPrevious startTile
      | startTile == BendNE = (sr - 1, sc)
      | startTile == BendNW = (sr - 1, sc)
      | startTile == BendSE = (sr + 1, sc)
      | startTile == BendSW = (sr + 1, sc)
      | startTile == Horizontal = (sr, sc - 1)
      | startTile == Vertical = (sr - 1, sc)

    solution = maximum $ map (\t -> walk (replaceStart start t grid) start (startPrevious t) start) [BendNE, BendNW, BendSE, BendSW, Horizontal, Vertical]

walk :: Grid -> Coordinate -> Coordinate -> Coordinate -> Int
walk grid origin previous here
  | wrong = -9999999999999999
  | done = 1
  | otherwise = 1 + walk grid origin here (nr, nc)
  where
    -- Get the tile at the current coordinate
    tile = grid !! fst here !! snd here
    -- Move to the next coordinate
    (nr, nc) = move previous here tile
    -- Validate if the next coordinate is valid
    wrong = (nr, nc) == here
    done = (nr, nc) == here || (nr, nc) == origin || nr < 0 || nr >= length grid || nc < 0 || nc >= length (head grid)

main :: IO ()
main = loadInput >>= print . solve . parse
