module Day7.Common where

import           Utils.Parser (Parser, chars, doParse, whitespace, integer, some, token)
import qualified Data.Set     as Set
import Data.Char (isDigit)
import Utils.Lists (frequencies)
import Data.List (sortBy)
import Data.Ord (comparing, Down (Down))

type Card = Int

data HandType = FiveOAK Int | FourOAK Int | FullHouse Int Int | ThreeOAK Int | TwoPair Int Int | OnePair Int | HighCard Int deriving (Eq, Show)

instance Ord HandType where
    compare a b = compare (hash a) (hash b) where
        hash (FiveOAK a) = 10000 + a
        hash (FourOAK a) = 9000 + a
        hash (FullHouse a b) = 8000 + a * 10 + b
        hash (ThreeOAK a) = 7000 + a
        hash (TwoPair a b) = 6000 + a * 10 + b
        hash (OnePair a) = 5000 + a
        hash (HighCard a) = 1000 + a

data Hand = Hand { cards :: [Card], score :: Int } deriving (Eq, Show)

parse :: String -> [Hand]
parse = doParse parser where
    parser = do some parseHand

parseHand :: Parser Hand
parseHand = do {cards <- chars 5; whitespace; bet <- integer; token '\n'; return $ Hand (map letterToInt cards) bet}

-- Functions

findHandType :: Hand -> HandType
findHandType h = findHandType' $ sortBy (comparing (Data.Ord.Down . snd)) $ frequencies $ cards h where
    findHandType' :: [(Card, Int)] -> HandType
    -- High card: all cards are distinct.
    findHandType' fs | snd (head fs) == 1 = HighCard (fst $ head fs)
    -- Five of a kind: all cards are equal.
    findHandType' fs | snd (head fs) == 5 = FiveOAK (fst $ head fs)
    -- Four of a kind: four cards are equal.
    findHandType' fs | snd (head fs) == 4 = FourOAK (fst $ head fs)
    -- Full house: three cards are equal + other two cards are equal.
    findHandType' fs | snd (head fs) == 3 && snd (fs !! 1) == 2 = FullHouse (fst $ head fs) (fst (fs !! 1))
    -- Three of a kind: three cards are equal.
    findHandType' fs | snd (head fs) == 3 = ThreeOAK (fst $ head fs)
    -- Two pairs: two cards + two other cards are equal.
    findHandType' fs | snd (head fs) == 2 && snd (fs !! 1) == 2 = TwoPair (fst $ head fs) (fst (fs !! 1))
    -- One pair: two cards are equal.
    findHandType' fs | snd (head fs) == 2 = OnePair (fst $ head fs)
    findHandType' fs = error (show h)

letterToInt :: Char -> Int
letterToInt 'A' = 14
letterToInt 'K' = 13
letterToInt 'Q' = 12
letterToInt 'J' = 11
letterToInt 'T' = 10
letterToInt d | isDigit d = read [d] :: Int
letterToInt d = error [d]
