module Day4.Simple where
import           Day4.Common (Card, matchingNumbers, parse)
import           Utils.IO    (loadInput)

score :: Card -> Int
score card = if matches == 0 then 0 else 2 ^ (matches - 1) where
    matches = matchingNumbers card

main :: IO ()
main = loadInput >>= print . sum . map score . parse
