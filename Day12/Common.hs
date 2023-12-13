module Day12.Common where

import Utils.Lists (maybeHead)
import Utils.Parser (Parser, doParse, eol, integer, some, token, (<|>))

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

solutions :: Record -> Int
solutions record = solutions' (conditions record) (brokens record)
  where
    -- solutions' :: [Condition] -> [Int] -> Int
    -- Fully solved.
    solutions' [] [] = 1
    -- No more broken springs, valid solution if there are no more broken conditions either.
    solutions' cs [] = if Broken `notElem` cs then 1 else 0
    -- No more conditions, but still broken springs: invalid solution.
    solutions' [] bs = 0
    -- Advance through an Ok condition.
    solutions' (c : cs) bs | c == Ok = solutions' cs bs
    -- Explore a Broken/Unknown condition.
    solutions' allCs@(c : cs) allBs@(b : bs) | c /= Ok = startHere + startLater
      where
        startHere = if length testGroup == b && notElem Ok testGroup && maybeHead endMarker /= Just Broken then solutions' next bs else 0
          where
            (testGroup, rest) = splitAt b allCs
            (endMarker, next) = splitAt 1 rest
        startLater = if c == Unknown then solutions' cs allBs else 0
    solutions' cs bs = error (show cs ++ show bs)
