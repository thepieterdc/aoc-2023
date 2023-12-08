module Day8.Simple where

import Day8.Common (parse, walk)
import Utils.IO (loadInput)

main :: IO ()
main = loadInput >>= print . walk "AAA" (== "ZZZ") . parse
