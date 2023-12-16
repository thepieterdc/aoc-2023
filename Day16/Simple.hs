module Day16.Simple where

import qualified Data.Set as Set
import Day16.Common (count, parse, walk)
import Utils.Grid (Direction (..))
import Utils.IO (loadInput)

main :: IO ()
main = loadInput >>= print . count . walk (Set.singleton ((0, 0), East)) . parse
