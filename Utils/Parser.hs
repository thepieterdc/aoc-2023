-- |
-- Module      : Utils.Parser
-- Description : Contains methods to parse strings into usable structures.
-- Copyright   : (c) Pieter De Clercq, 2022
-- License     : MIT
--
-- Contains methods to parse strings into usable structures.
module Utils.Parser (Parser, char, chars, digit, digits, doParse, end, eol, guard, integer, many, optional, some, spot, string, token, until, whitespace, (<|>)) where

import Control.Applicative (Alternative, empty, many, some, (<|>))
import Control.Monad
  ( MonadPlus,
    ap,
    guard,
    liftM,
    mplus,
    mzero,
    void,
  )
import Data.Char (isDigit)
import Prelude hiding (until)

-- | Definiton of a Parser from Strings to custom types.
newtype Parser a = Parser (String -> [(a, String)])

-- | Executes the given parser.
doParse ::
  (Show a) =>
  -- | The parser to run
  Parser a ->
  -- | The input string to parse
  String ->
  -- | The resulting structure
  a
doParse m s = one [x | (x, t) <- apply m s, t == ""]
  where
    one [x] = x
    one [] = error ("Parse not completed:\n" ++ show s)
    one xs | length xs > 1 = error ("Multiple parses found:\n" ++ show xs)
    one _ = error "Unknown error"

apply :: Parser a -> String -> [(a, String)]
apply (Parser f) = f

instance Alternative Parser where
  empty :: Parser a
  empty = mzero

  many :: Parser a -> Parser [a]
  many p = some p `mplus` return []

  some :: Parser a -> Parser [a]
  some p = do x <- p; xs <- many p; return (x : xs)

  -- \|Runs the second parser if the first did not succeed.
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) p q =
    Parser
      ( \s ->
          case apply p s of
            [] -> apply q s
            res -> res
      )

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (\s -> [(x, s)])

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) = ap

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap = liftM

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  m >>= k = Parser (\s -> [(y, u) | (x, t) <- apply m s, (y, u) <- apply (k x) t])

instance MonadPlus Parser where
  mplus :: Parser a -> Parser a -> Parser a
  mplus m n = Parser (ap ((++) . apply m) (apply n))

  mzero :: Parser a
  mzero = Parser (const [])

-- | Parses a single character.
char :: Parser Char
char = Parser f
  where
    f [] = []
    f (c : s) = [(c, s)]

-- | Parses the given amount of characters into a String.
chars :: Int -> Parser String
chars 0 = return ""
chars amt = do x <- char; y <- chars (amt - 1); return (x : y)

-- | Parses a single digit into a Char.
digit :: Parser Char
digit = spot isDigit

-- | Parses multiple digits into a String.
digits :: Parser String
digits = some digit

-- | Matches the end of a line.
eol :: Parser ()
eol = void $ token '\n'

-- | Matches the end of the file.
end :: Parser ()
end = Parser f
  where
    f [] = [((), [])]
    f cs = [((), cs)]

-- | Parses multiple digits into an Integer.
integer :: Parser Int
integer = positive <|> negative
  where
    positive = do read <$> digits
    negative = do token '-'; d <- digits; return (-read d)

-- | Attempts to parse a String that might be absent.
optional :: String -> Parser String
optional s = string s <|> return []

-- | Matches the given character.
spot :: (Char -> Bool) -> Parser Char
spot p = do c <- char; guard (p c); return c

-- | Matches the given string.
string :: String -> Parser String
string s = do cs <- chars (length s); guard (cs == s); return cs

-- | Matches the given character, returning the result.
token :: Char -> Parser Char
token c = spot (== c)

-- | Parses the string until the given character is found.
until :: Char -> Parser String
until c = do cs <- some $ spot (/= c); c' <- char; return $ cs ++ [c]

-- | Parses whitespace, discarding the result.
whitespace :: Parser ()
whitespace = void (some $ spot (== ' '))
