module Day10.Common where

import Data.Set (Set)
import qualified Data.Set as Set
import Utils.Grid (Coordinate, euclideanNeighbours)
import Utils.IO (loadInput)
import Utils.Lists (mapIdx)
import Utils.Parser (Parser, doParse, eol, some, token, (<|>))

data Tile = BendNE | BendNW | BendSE | BendSW | Ground | Horizontal | Start | Vertical deriving (Eq, Ord, Show)

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

onGrid :: Grid -> Coordinate -> Bool
onGrid grid (r, c) = r >= 0 && r < length grid && c >= 0 && c < length (head grid)

replacedGrids :: Grid -> [(Tile, Grid)]
replacedGrids grid = map (\t -> (t, replaceStart start t grid)) validCandidates
  where
    start = findStart 0 grid
    valid (r, c) = onGrid grid (r, c) && (r, c) /= start && grid !! r !! c /= Ground

    candidates = [(supportedConnectors start t, t) | t <- [BendNE, BendNW, BendSE, BendSW, Horizontal, Vertical]]
    validCandidates = [t | ((((lr, lc), lefts), ((rr, rc), rights)), t) <- candidates, valid (lr, lc), valid (rr, rc), Set.member (grid !! lr !! lc) lefts, Set.member (grid !! rr !! rc) rights]

replaceStart :: Coordinate -> Tile -> Grid -> Grid
replaceStart (r, c) t = mapIdx r (mapIdx c (const t))

startPrevious :: Coordinate -> Tile -> Coordinate
startPrevious (sr, sc) startTile
  | startTile == BendNE = (sr - 1, sc)
  | startTile == BendNW = (sr - 1, sc)
  | startTile == BendSE = (sr + 1, sc)
  | startTile == BendSW = (sr + 1, sc)
  | startTile == Horizontal = (sr, sc - 1)
  | startTile == Vertical = (sr - 1, sc)

supportedConnectors :: Coordinate -> Tile -> ((Coordinate, Set Tile), (Coordinate, Set Tile))
supportedConnectors (r, c) BendNE = (((r, c + 1), Set.fromList [BendNW, BendSW, Horizontal]), ((r - 1, c), Set.fromList [BendSE, BendSW, Vertical]))
supportedConnectors (r, c) BendNW = (((r, c - 1), Set.fromList [BendNE, BendSE, Horizontal]), ((r - 1, c), Set.fromList [BendSE, BendSW, Vertical]))
supportedConnectors (r, c) BendSE = (((r + 1, c), Set.fromList [BendNE, BendNW, Vertical]), ((r, c + 1), Set.fromList [BendNW, BendSW, Horizontal]))
supportedConnectors (r, c) BendSW = (((r + 1, c), Set.fromList [BendNE, BendNW, Vertical]), ((r, c - 1), Set.fromList [BendNE, BendSE, Horizontal]))
supportedConnectors (r, c) Horizontal = (((r, c - 1), Set.fromList [BendNE, BendSE, Horizontal]), ((r, c + 1), Set.fromList [BendNW, BendSW, Horizontal]))
supportedConnectors (r, c) Vertical = (((r - 1, c), Set.fromList [BendSE, BendSW, Vertical]), ((r + 1, c), Set.fromList [BendNE, BendNW, Vertical]))

walk :: Grid -> Coordinate -> Coordinate -> Coordinate -> [Coordinate]
walk grid origin previous here
  | wrong = []
  | done = [here]
  | otherwise = here : walk grid origin here (nr, nc)
  where
    -- Get the tile at the current coordinate
    tile = grid !! fst here !! snd here
    -- Move to the next coordinate
    (nr, nc) = move previous here tile
    -- Validate if the next coordinate is valid
    wrong = (nr, nc) == here
    done = (nr, nc) == here || (nr, nc) == origin || nr < 0 || nr >= length grid || nc < 0 || nc >= length (head grid)
