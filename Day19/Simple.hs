module Day19.Simple where

import Data.Map (Map)
import Day19.Common (Rating, Workflow, isAccepted, parse, score)
import Utils.IO (loadInput)

findAccepted :: (Map String Workflow, [Rating]) -> [Rating]
findAccepted (workflows, ratings) = filter (isAccepted workflows) ratings

main :: IO ()
main = loadInput >>= print . sum . map score . findAccepted . parse
