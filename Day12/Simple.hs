module Day12.Simple where

import Utils.IO (loadInput)
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

-- solutions :: Record -> Int
solutions record = solutions' (conditions record) (brokens record)
  where
    solutions' :: [Condition] -> [Int] -> Int
    -- Fully solved.
    solutions' [] [] = 1
    -- Still broken conditions, but no more broken springs: invalid solution.
    solutions' cs [] | Broken `elem` cs = 0
    -- Advance through an Ok condition.
    solutions' (c : cs) bs | c == Ok = solutions' cs bs
    -- Explore an Unknown condition.
    solutions' allCs@(c : cs) allBs@(b : bs) = startHere + startLater
      where
        startHere = if notElem Ok testGroup && length testGroup == b then solutions' rest bs else 0
          where
            (testGroup, rest) = splitAt 3 allCs
        startLater = if c == Unknown then solutions' cs allBs else 0

main :: IO ()
main = loadInput >>= print . solutions . head . parse
