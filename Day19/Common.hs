module Day19.Common where

import Data.Char (isAsciiLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Utils.Parser (Parser, char, doParse, eol, integer, optional, some, spot, string, token, (<|>))

data Condition = LessThan Int | GreaterThan Int deriving (Eq, Show)

data WorkflowStep = Conditional Char Condition WorkflowStep | Goto String | Accept | Refuse deriving (Eq, Show)

type Rating = Map Char Int

parse :: String -> (Map String [WorkflowStep], [Rating])
parse = doParse parser
  where
    parser = do
      workflows <- some parseWorkflow
      eol
      ratings <- some parseRating
      return (prune $ prune $ prune $ Map.fromList workflows, ratings)

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

parseWorkflow :: Parser (String, [WorkflowStep])
parseWorkflow = do
  name <- some (spot isAsciiLower)
  token '{'
  steps <- parseSteps
  token '}'
  eol
  return (name, steps)
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

findUnreachableWorkflows :: [String] -> Map String [WorkflowStep] -> Set String
findUnreachableWorkflows keys wfs = Set.difference (Set.fromList keys) reachable
  where
    reachable = Set.insert "in" $ Set.fromList $ mapMaybe getTarget (concat $ Map.elems wfs)

getTarget :: WorkflowStep -> Maybe String
getTarget (Conditional _ _ (Goto x)) = Just x
getTarget (Goto x) = Just x
getTarget _ = Nothing

isAccepted :: Map String [WorkflowStep] -> Rating -> Bool
isAccepted workflows rating = result == Accept
  where
    result = solve workflows rating $ workflows Map.! "in"

pruneSteps :: Map String [WorkflowStep] -> [WorkflowStep] -> [WorkflowStep]
pruneSteps wfs wfSteps | isAlwaysAccept wfSteps = [Accept]
  where
    isAlwaysAccept :: [WorkflowStep] -> Bool
    isAlwaysAccept (Accept : _) = True
    isAlwaysAccept (Conditional _ _ Accept : rest) = isAlwaysAccept rest
    isAlwaysAccept (Conditional _ _ (Goto x) : rest) = isAlwaysAccept (wfs Map.! x) && isAlwaysAccept rest
    isAlwaysAccept (Goto target : rest) = isAlwaysAccept (wfs Map.! target)
    isAlwaysAccept [] = True
    isAlwaysAccept _ = False
pruneSteps wfs wfSteps | isAlwaysRefuse wfSteps = [Refuse]
  where
    isAlwaysRefuse :: [WorkflowStep] -> Bool
    isAlwaysRefuse (Refuse : _) = True
    isAlwaysRefuse (Conditional _ _ Refuse : rest) = isAlwaysRefuse rest
    isAlwaysRefuse (Conditional _ _ (Goto x) : rest) = isAlwaysRefuse (wfs Map.! x) && isAlwaysRefuse rest
    isAlwaysRefuse (Goto target : rest) = isAlwaysRefuse (wfs Map.! target)
    isAlwaysRefuse [] = True
    isAlwaysRefuse _ = False
pruneSteps wfs (x : rest) = x : pruneSteps wfs rest
pruneSteps _ [] = []

prune :: Map String [WorkflowStep] -> Map String [WorkflowStep]
prune workflows = Map.filterWithKey (\x _ -> not $ Set.member x unreachable) prunedWorkflows
  where
    prunedWorkflows = Map.map (pruneSteps workflows) workflows
    unreachable = findUnreachableWorkflows (Map.keys workflows) prunedWorkflows

score :: Rating -> Int
score rating = sum $ Map.elems rating

solve :: Map String [WorkflowStep] -> Rating -> [WorkflowStep] -> WorkflowStep
solve _ _ (Accept : _) = Accept
solve _ _ (Refuse : _) = Refuse
solve wfs rating ((Goto wf : _)) = solve wfs rating (wfs Map.! wf)
solve wfs rating ((Conditional c cond target) : next) = solve wfs rating evaluated
  where
    value = rating Map.! c
    evaluated = if evaluateCondition cond value then [target] else next
