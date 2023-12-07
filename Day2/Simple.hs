module Day2.Simple where

import Day2.Common (Cube (..), Game (..), Set, parse)
import Utils.IO (loadInput)

gameIsPossible :: Game -> Bool
gameIsPossible game = all setIsPossible (sets game)

setIsPossible :: Set -> Bool
setIsPossible ((Blue, amount) : rest) = amount <= 14 && setIsPossible rest
setIsPossible ((Green, amount) : rest) = amount <= 13 && setIsPossible rest
setIsPossible ((Red, amount) : rest) = amount <= 12 && setIsPossible rest
setIsPossible [] = True

main :: IO ()
main = loadInput >>= print . sum . map identifier . filter gameIsPossible . parse
