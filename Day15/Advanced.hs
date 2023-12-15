module Day15.Advanced where

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Day15.Common (hash)
import Utils.IO (loadInput)

type Lens = (String, Int)

data Operation = Add {label :: String, focalLength :: Int} | Remove {label :: String} deriving (Eq, Show)

addToBox :: Lens -> [Lens] -> [Lens]
addToBox lens (l : ls)
  | fst l == fst lens = lens : ls
  | otherwise = l : addToBox lens ls
addToBox lens [] = [lens]

parse :: String -> Operation
parse s
  | last s == '-' = Remove (init s)
  | otherwise = Add (head parts) (read $ last parts)
  where
    parts = splitOn "=" s

process :: Map Int [Lens] -> Operation -> Map Int [Lens]
process m (Add lbl len) = Map.insert box updatedBox m
  where
    box = hash lbl
    boxContents = Map.findWithDefault [] box m
    updatedBox = addToBox (lbl, len) boxContents
process m (Remove lbl) = Map.insert box updatedBox m
  where
    box = hash lbl
    boxContents = Map.findWithDefault [] box m
    updatedBox = [(lbl', len') | (lbl', len') <- boxContents, lbl' /= lbl]

score :: [(Int, [Lens])] -> Int
score boxes = sum $ map (\box -> sum (zipWith (curry (\pl -> (fst box + 1) * fst pl * snd (snd pl))) [1 ..] (snd box))) boxes

main :: IO ()
main = loadInput >>= print . score . Map.assocs . foldl process Map.empty . map parse . splitOn "," . head . lines
