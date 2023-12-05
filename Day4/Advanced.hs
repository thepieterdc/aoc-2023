module Day4.Advanced where
import           Data.Bifunctor (second)
import           Data.Function  (on)
import           Day4.Common    (Card (correctNumbers), parse)
import           Utils.IO       (loadInput)

cardsWon :: [(Card, Int)] -> Int
cardsWon ((c, multiplier):cs) = multiplier + cardsWon nextCards where
    parts = splitAt (correctNumbers c) cs
    nextCards = map (second (+ multiplier)) (fst parts) ++ snd parts
cardsWon _ = 0

main :: IO ()
main = loadInput >>= print . cardsWon . map (,1) . parse
