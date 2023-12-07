module Day7.Common where

import           Utils.Parser (Parser, chars, doParse, whitespace, integer, some, token)
import qualified Data.Set     as Set
import Data.Char (isDigit)
import Utils.Lists (frequencies)
import Data.List (sortBy)
import Data.Ord (comparing, Down (Down))

type Card = Int

data HandType = HighCard | OnePair | TwoPair | ThreeOAK | FullHouse | FourOAK | FiveOAK deriving (Eq, Ord, Show)

data Hand = Hand { cards :: [Card], score :: Int, typ :: HandType } deriving (Eq, Show)

instance Ord Hand where
    compare :: Hand -> Hand -> Ordering
    compare h1 h2 | typ h1 /= typ h2 = compare (typ h1) (typ h2)
                  | otherwise = head [uncurry compare cs | cs <- zip (cards h1) (cards h2), uncurry (/=) cs]

parse :: String -> [Hand]
parse = doParse parser where
    parser = do some parseHand

parseHand :: Parser Hand
parseHand = do {cs <- chars 5; whitespace; bet <- integer; token '\n'; return $ Hand (map letterToInt cs) bet (findHandType (map letterToInt cs))}

-- Functions

findHandType :: [Card] -> HandType
findHandType cs = findHandType' $ sortBy (comparing (Data.Ord.Down . snd)) $ frequencies cs where
    findHandType' :: [(Card, Int)] -> HandType
    -- High card: all cards are distinct.
    findHandType' fs | snd (head fs) == 1 = HighCard
    -- Five of a kind: all cards are equal.
    findHandType' fs | snd (head fs) == 5 = FiveOAK
    -- Four of a kind: four cards are equal.
    findHandType' fs | snd (head fs) == 4 = FourOAK
    -- Full house: three cards are equal + other two cards are equal.
    findHandType' fs | snd (head fs) == 3 && snd (fs !! 1) == 2 = FullHouse
    -- Three of a kind: three cards are equal.
    findHandType' fs | snd (head fs) == 3 = ThreeOAK
    -- Two pairs: two cards + two other cards are equal.
    findHandType' fs | snd (head fs) == 2 && snd (fs !! 1) == 2 = TwoPair
    -- One pair: two cards are equal.
    findHandType' fs | snd (head fs) == 2 = OnePair
    findHandType' fs = error (show cs)

letterToInt :: Char -> Int
letterToInt 'A' = 14
letterToInt 'K' = 13
letterToInt 'Q' = 12
letterToInt 'J' = 11
letterToInt 'T' = 10
letterToInt d | isDigit d = read [d] :: Int
letterToInt d = error [d]
