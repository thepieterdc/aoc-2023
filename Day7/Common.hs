module Day7.Common where

import Data.List (sortOn)
import qualified Data.Ord
import Utils.Lists (frequencies)
import Utils.Parser (Parser, chars, doParse, integer, some, token, whitespace)

type Card = Int

data HandType = HighCard | OnePair | TwoPair | ThreeOAK | FullHouse | FourOAK | FiveOAK deriving (Eq, Ord, Show)

data Hand = Hand {cards :: [Card], score :: Int, typ :: HandType} deriving (Eq, Show)

instance Ord Hand where
  compare :: Hand -> Hand -> Ordering
  compare h1 h2
    | typ h1 /= typ h2 = compare (typ h1) (typ h2)
    | otherwise = head [uncurry compare cs | cs <- zip (cards h1) (cards h2), uncurry (/=) cs]

parse :: (Char -> Int) -> String -> [Hand]
parse mappingFn = doParse parser
  where
    parser = do some (parseHand mappingFn)

parseHand :: (Char -> Int) -> Parser Hand
parseHand mappingFn = do cs <- chars 5; whitespace; bet <- integer; token '\n'; return $ Hand (map mappingFn cs) bet (findHandType (map mappingFn cs))

-- Functions

findHandType :: [Card] -> HandType
findHandType cs = findHandType' $ sortOn (Data.Ord.Down . snd) $ frequencies cs
  where
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
