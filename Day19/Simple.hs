module Day19.Simple where

import Data.Char (isAsciiLower)
import Utils.IO (loadInput)
import Utils.Parser (Parser, char, doParse, eol, integer, optional, some, spot, string, token, (<|>))

data Rating = Rating {x :: Int, m :: Int, a :: Int, s :: Int} deriving (Eq, Show)

data Condition = LessThan Int | GreaterThan Int deriving (Eq, Show)

data WorkflowStep = Conditional Char Condition WorkflowStep | Goto String | Accept | Refuse deriving (Eq, Show)

data Workflow = Workflow {name :: String, steps :: [WorkflowStep]} deriving (Eq, Show)

parse :: String -> ([Workflow], [Rating])
parse = doParse parser
  where
    parser = do
      workflows <- some parseWorkflow
      eol
      ratings <- some parseRating
      return (workflows, ratings)

parseCondition :: Parser Condition
parseCondition = parseGreaterThan <|> parseLessThan
  where
    parseGreaterThan = do token '>'; GreaterThan <$> integer
    parseLessThan = do token '<'; LessThan <$> integer

parsePart :: Parser Char
parsePart = do token 'x' <|> token 'm' <|> token 'a' <|> token 's'

parseRating :: Parser Rating
parseRating = do
  string "{x="
  x <- integer
  string ",m="
  m <- integer
  string ",a="
  a <- integer
  string ",s="
  s <- integer
  string "}"
  eol
  return $ Rating x m a s

parseWorkflow :: Parser Workflow
parseWorkflow = do
  name <- some (spot isAsciiLower)
  token '{'
  steps <- parseSteps
  token '}'
  eol
  return $ Workflow name steps
  where
    parseSteps = parseMore <|> parseLast
      where
        parseMore = do step <- parseWorkflowStep; token ','; rest <- parseSteps; return $ step : rest
        parseLast = do step <- parseWorkflowStep; return [step]

parseWorkflowStep :: Parser WorkflowStep
parseWorkflowStep = do parseConditional <|> parseAccept <|> parseRefuse <|> parseGoto
  where
    parseAccept = do token 'A'; return Accept
    parseConditional = do p <- parsePart; c <- parseCondition; token ':'; Conditional p c <$> parseWorkflowStep
    parseGoto = do target <- some (spot isAsciiLower); return $ Goto target
    parseRefuse = do token 'R'; return Refuse

main :: IO ()
main = loadInput >>= print . parse
