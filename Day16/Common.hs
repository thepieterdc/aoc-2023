module Day16.Common where

import Data.Set (Set)
import qualified Data.Set as Set
import Utils.Filtering (countWhere)
import Utils.Grid (Coordinate, Direction (..), Grid, inBounds)

data Mirror = Backward | Forward | SplitH | SplitV deriving (Eq, Show)

count :: Grid ([Direction], Maybe Mirror) -> Int
count = foldr ((+) . countWhere (not . null . fst)) 0

encode :: ([Direction], Maybe Mirror) -> Char
encode (dirs, _)
  | length dirs > 1 = head $ show dirs
  | null dirs = '.'
  | head dirs == East = '>'
  | head dirs == North = '^'
  | head dirs == South = 'v'
  | head dirs == West = '<'

mark :: Direction -> Coordinate -> Grid ([Direction], Maybe Mirror) -> Grid ([Direction], Maybe Mirror)
mark d (r, c) grid = take r grid ++ [take c (grid !! r) ++ [([d], snd $ grid !! r !! c)] ++ drop (c + 1) (grid !! r)] ++ drop (r + 1) grid

parse :: String -> Grid ([Direction], Maybe Mirror)
parse input = map (map (\c -> ([], parse' c))) $ lines input
  where
    parse' :: Char -> Maybe Mirror
    parse' '\\' = Just Backward
    parse' '/' = Just Forward
    parse' '-' = Just SplitH
    parse' '|' = Just SplitV
    parse' '.' = Nothing

reflect :: Coordinate -> Direction -> Maybe Mirror -> [(Coordinate, Direction)]
-- If the cell is empty, the beam continues in the same direction.
reflect (r, c) East Nothing = [((r, c + 1), East)]
reflect (r, c) North Nothing = [((r - 1, c), North)]
reflect (r, c) South Nothing = [((r + 1, c), South)]
reflect (r, c) West Nothing = [((r, c - 1), West)]
-- If the cell contains a vertical split mirror, the beam is split vertically if it was going East or West, and continues in the same direction otherwise.
reflect (r, c) d (Just SplitV) | d == East || d == West = [((r - 1, c), North), ((r + 1, c), South)]
reflect (r, c) North (Just SplitV) = [((r - 1, c), North)]
reflect (r, c) South (Just SplitV) = [((r + 1, c), South)]
-- If the cell contains a horizontal split mirror, the beam is split horizontally if it was going North or South, and continues in the same direction otherwise.
reflect (r, c) d (Just SplitH) | d == North || d == South = [((r, c - 1), West), ((r, c + 1), East)]
reflect (r, c) East (Just SplitH) = [((r, c + 1), East)]
reflect (r, c) West (Just SplitH) = [((r, c - 1), West)]
-- If the cell contains a forward mirror, the beam is reflected 90 degrees.
reflect (r, c) East (Just Forward) = [((r - 1, c), North)]
reflect (r, c) North (Just Forward) = [((r, c + 1), East)]
reflect (r, c) South (Just Forward) = [((r, c - 1), West)]
reflect (r, c) West (Just Forward) = [((r + 1, c), South)]
-- If the cell contains a backward mirror, the beam is reflected 90 degrees.
reflect (r, c) East (Just Backward) = [((r + 1, c), South)]
reflect (r, c) North (Just Backward) = [((r, c - 1), West)]
reflect (r, c) South (Just Backward) = [((r, c + 1), East)]
reflect (r, c) West (Just Backward) = [((r - 1, c), North)]
reflect (r, c) d m = error $ show (r, c) ++ " " ++ show d ++ " " ++ show m

walk :: Set (Coordinate, Direction) -> Grid ([Direction], Maybe Mirror) -> Grid ([Direction], Maybe Mirror)
walk q grid
  | Set.null q = grid
  | otherwise = if alreadyMarked then walk q' grid else walk (Set.union q' nextPositions) marked
  where
    (((r, c), d), q') = Set.deleteFindMin q
    alreadyMarked = d `elem` fst (grid !! r !! c)
    nextPositions = Set.fromList [((r', c'), d') | ((r', c'), d') <- reflect (r, c) d (snd $ grid !! r !! c), inBounds grid (r', c'), d' `notElem` fst (grid !! r' !! c')]
    marked = mark d (r, c) grid
