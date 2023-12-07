module Day7.Advanced where

import Data.Char (isDigit)
import Data.List (sort, sortOn)
import Data.Ord (Down (Down), comparing)
import qualified Data.Set as Set
import Utils.IO (loadInput)
import Utils.Lists (frequencies)
import Utils.Parser
  ( Parser,
    chars,
    doParse,
    integer,
    some,
    token,
    whitespace,
  )

type Card = Int

data HandType = HighCard | OnePair | TwoPair | ThreeOAK | FullHouse | FourOAK | FiveOAK deriving (Eq, Ord, Show)

data Hand = Hand {cards :: [Card], score :: Int, typ :: HandType} deriving (Eq, Show)

instance Ord Hand where
  compare :: Hand -> Hand -> Ordering
  compare h1 h2
    | typ h1 /= typ h2 = compare (typ h1) (typ h2)
    | otherwise = head [uncurry compare cs | cs <- zip (cards h1) (cards h2), uncurry (/=) cs]

parse :: String -> [Hand]
parse = doParse parser
  where
    parser = do some parseHand

parseHand :: Parser Hand
parseHand = do cs <- chars 5; whitespace; bet <- integer; token '\n'; return $ Hand (map letterToInt cs) bet (findHandType (map letterToInt cs))

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

letterToInt :: Char -> Int
letterToInt 'A' = 14
letterToInt 'K' = 13
letterToInt 'Q' = 12
letterToInt 'T' = 10
letterToInt 'J' = 1
letterToInt d | isDigit d = read [d] :: Int
letterToInt d = error [d]

upgradeType :: Hand -> HandType
upgradeType h
  -- Five of a kind can never be upgraded as it's already the highest, also this handles
  -- an edge case where there are no Js in the hand.
  | typ h == FiveOAK = FiveOAK
  -- Replace the J's by the highest occurring (non-J) number in the hand.
  | otherwise = findHandType $ map (\c -> if c == 1 then replacement else c) (cards h)
  where
    nonJs = sortOn (Data.Ord.Down . snd) $ frequencies $ filter (/= 1) (cards h)
    replacement = fst $ head nonJs

main :: IO ()
main = loadInput >>= print . sum . zipWith (curry (\p -> fst p * score (snd p))) [1 ..] . sort . map (\h -> Hand (cards h) (score h) (upgradeType h)) . parse
