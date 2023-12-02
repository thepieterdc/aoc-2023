module Day2.Advanced where

import           Day2.Common (Cube (..), Game (..), Set, parse)
import           Prelude     hiding (maximum)
import           Utils.IO    (loadInput)

power :: (Int, Int, Int) -> Int
power (a, b, c) = a * b * c

required :: Game -> (Int, Int, Int)
required game = foldl usages (0, 0, 0) (sets game)

usages :: (Int, Int, Int) -> Set -> (Int, Int, Int)
usages (b, g, r) ((Blue, amt):rest)  = usages (max b amt, g, r) rest
usages (b, g, r) ((Green, amt):rest) = usages (b, max g amt, r) rest
usages (b, g, r) ((Red, amt):rest)   = usages (b, g, max r amt) rest
usages current []                    = current

main :: IO ()
main = loadInput >>= print . sum . map (power . required) . parse
