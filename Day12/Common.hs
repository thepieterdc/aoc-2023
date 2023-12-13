module Day12.Common where

import Data.Map (Map)
import qualified Data.Map as Map
import Utils.Lists (maybeHead)
import Utils.Parser (Parser, doParse, eol, integer, some, token, (<|>))

type Cache = Map String Int

data Condition = Ok | Broken | Unknown deriving (Eq, Show)

data Record = Record {conditions :: [Condition], brokens :: [Int]} deriving (Eq, Show)

parse :: String -> [Record]
parse = doParse $ some parseRecord

parseBrokens :: Parser [Int]
parseBrokens = parseMore <|> parseLast
  where
    parseMore = do num <- integer; token ','; rest <- parseBrokens; return $ num : rest
    parseLast = do num <- integer; eol; return [num]

parseCondition :: Parser Condition
parseCondition = parseBroken <|> parseOk <|> parseUnknown
  where
    parseBroken = token '#' >> return Broken
    parseOk = token '.' >> return Ok
    parseUnknown = token '?' >> return Unknown

parseRecord :: Parser Record
parseRecord = do
  conditions <- some parseCondition
  token ' '
  Record conditions <$> parseBrokens

-- solutions :: Record -> Int
solutions record = fst $ solutions' Map.empty (conditions record) (brokens record)
  where
    -- Define a hash function to represent the current.
    hash :: [Condition] -> [Int] -> String
    hash conditions brokens = show (length conditions) ++ show brokens

    solutions' :: Cache -> [Condition] -> [Int] -> (Int, Cache)
    -- Attempt to reuse a value from the cache.
    solutions' cache cs bs | Map.member (hash cs bs) cache = (cache Map.! hash cs bs, cache)
    -- Fully solved.
    solutions' cache [] [] = (1, Map.insert (hash [] []) 1 cache)
    -- No more broken springs, solution is valid if there are no more broken conditions either.
    solutions' cache cs [] = (value, Map.insert (hash cs []) value cache)
      where
        value = if Broken `notElem` cs then 1 else 0
    -- No more conditions, but still broken springs: invalid solution.
    solutions' cache [] bs = (0, Map.insert (hash [] bs) 0 cache)
    -- Advance through an Ok condition.
    solutions' cache allCs@(c : cs) bs | c == Ok = (value, Map.insert (hash allCs bs) value cache')
      where
        (value, cache') = solutions' cache cs bs
    -- Explore a Broken/Unknown condition.
    solutions' cache allCs@(c : cs) allBs@(b : bs) | c /= Ok = (startHere + startLater, Map.insert (hash allCs allBs) (startHere + startLater) cache'')
      where
        (startHere, cache') = if validRecurse then solutions' cache next bs else (0, cache)
          where
            (testGroup, rest) = splitAt b allCs
            (endMarker, next) = splitAt 1 rest
            validRecurse = length testGroup == b && notElem Ok testGroup && maybeHead endMarker /= Just Broken
        (startLater, cache'') = if c == Unknown then solutions' cache' cs allBs else (0, cache')
    solutions' _ cs bs = error (show cs ++ show bs)
