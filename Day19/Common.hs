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
      return (prune $ Map.fromList (map (\wf -> (name wf, wf)) workflows), ratings)

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

isAccepted :: Map String Workflow -> Rating -> Bool
isAccepted workflows rating = result == Accept
  where
    result = solve workflows rating $ steps (workflows Map.! "in")

pruneSteps :: Map String Workflow -> [WorkflowStep] -> [WorkflowStep]
pruneSteps wfs wfSteps | isAlwaysAccept wfSteps = [Accept]
  where
    isAlwaysAccept :: [WorkflowStep] -> Bool
    isAlwaysAccept (Accept : _) = True
    isAlwaysAccept (Conditional _ _ Accept : rest) = isAlwaysAccept rest
    isAlwaysAccept [] = True
    isAlwaysAccept _ = False
pruneSteps wfs wfSteps | isAlwaysRefuse wfSteps = [Refuse]
  where
    isAlwaysRefuse :: [WorkflowStep] -> Bool
    isAlwaysRefuse (Refuse : _) = True
    isAlwaysRefuse (Conditional _ _ Refuse : rest) = isAlwaysRefuse rest
    isAlwaysRefuse [] = True
    isAlwaysRefuse _ = False
pruneSteps wfs (x : rest) = x : pruneSteps wfs rest
pruneSteps _ [] = []

prune :: Map String Workflow -> Map String Workflow
prune workflows = Map.map pruneWorkflow workflows
  where
    pruneWorkflow :: Workflow -> Workflow
    pruneWorkflow (Workflow name steps) = Workflow name $ pruneSteps workflows steps

score :: Rating -> Int
score rating = sum $ Map.elems rating

solve :: Map String Workflow -> Rating -> [WorkflowStep] -> WorkflowStep
solve _ _ (Accept : _) = Accept
solve _ _ (Refuse : _) = Refuse
solve wfs rating ((Goto wf : _)) = solve wfs rating $ steps (wfs Map.! wf)
solve wfs rating ((Conditional c cond target) : next) = solve wfs rating evaluated
  where
    value = rating Map.! c
    evaluated = if evaluateCondition cond value then [target] else next
