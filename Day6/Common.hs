module Day6.Common where

import Utils.Parser
  ( Parser,
    digits,
    doParse,
    eol,
    integer,
    some,
    string,
    token,
    whitespace,
    (<|>),
  )

data Race = Race {time :: Int, distance :: Int} deriving (Eq, Show)

parse :: String -> [Race]
parse = doParse parser
  where
    parser = do
      string "Time:"
      times <- parseNumbers
      string "Distance:"
      distances <- parseNumbers
      return $ [Race t d | (t, d) <- zip times distances]

parseNumbers :: Parser [Int]
parseNumbers = do parseMore <|> parseLast
  where
    parseMore = do whitespace; num <- integer; rest <- parseNumbers; return (num : rest)
    parseLast = do whitespace; num <- integer; eol; return [num]

solve :: ([Race] -> [Race]) -> String -> Int
solve raceTf input = product $ map waysToWin $ raceTf $ parse input

waysToWin :: Race -> Int
waysToWin race = result
  where
    -- Max distance d in an allowed time t with a given speed v: d = (v * (t - v)),
    -- where d and t are known and v is not -> rewrite into -v^2 + vt -d = 0,
    -- solve for v.
    d = fromIntegral $ time race ^ 2 - (4 * (-1) * (-distance race))
    root = sqrt d
    min = ((-fromIntegral (time race)) - root) / 2
    max = ((-fromIntegral (time race)) + root) / 2
    result = ceiling max - floor min - 1
