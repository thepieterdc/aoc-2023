module Day4.Advanced where
import           Data.Function (on)
import           Day4.Common   (Card (identifier), matchingNumbers, parse)
import           Utils.IO      (loadInput)

process :: [Card] -> Int
process cards = cardsWon cards cards

cardsWon :: [Card] -> [Card] -> Int
cardsWon allCards (c:cs) = 1 + cardsWon allCards extraCards where
    thisScore = matchingNumbers c
    extraCardIds = [(identifier c) .. (identifier c + thisScore - 1)]
    extraCards = map (allCards !!) extraCardIds ++ cs
cardsWon _ _ = 0

main :: IO ()
main = loadInput >>= print . process . parse
