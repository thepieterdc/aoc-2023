module Day19.Common where

import Data.Char (isAsciiLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Utils.Parser (Parser, char, doParse, eol, integer, optional, some, spot, string, token, (<|>))

data Condition = LessThan Int | GreaterThan Int deriving (Eq, Show)

data WorkflowStep = Conditional Char Condition WorkflowStep | Goto String | Accept | Refuse deriving (Eq, Show)

data Workflow = Workflow {name :: String, steps :: [WorkflowStep]} deriving (Eq, Show)

type Rating = Map Char Int

parse :: String -> (Map String Workflow, [Rating])
parse = doParse parser
  where
    parser = do
      workflows <- some parseWorkflow
      eol
      ratings <- some parseRating
      return (Map.fromList (map (\wf -> (name wf, wf)) workflows), ratings)

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
  return $ Map.fromList [('x', x), ('m', m), ('a', a), ('s', s)]

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

evaluateCondition :: Condition -> Int -> Bool
evaluateCondition (LessThan x) y = y < x
evaluateCondition (GreaterThan x) y = y > x
