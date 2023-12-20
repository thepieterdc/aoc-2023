module Day19.Simple where

import Data.Map (Map)
import qualified Data.Map as Map
import Day19.Common (Condition, Rating, Workflow (..), WorkflowStep (..), evaluateCondition, parse)
import Utils.IO (loadInput)

findAccepted :: (Map String Workflow, [Rating]) -> [Rating]
findAccepted (workflows, ratings) = filter (isAccepted workflows) ratings

isAccepted :: Map String Workflow -> Rating -> Bool
isAccepted workflows rating = result == Accept
  where
    result = solve workflows rating $ steps (workflows Map.! "in")

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

main :: IO ()
main = loadInput >>= print . sum . map score . findAccepted . parse
