module Day12.Advanced where

import Data.List (intercalate)
import Day12.Common (Condition (..), Record (..), parse, solutions)
import GHC.IO.SubSystem (conditional)
import Utils.IO (loadInput)

expand :: Record -> Record
expand record = Record cs bs
  where
    cs = intercalate [Unknown] $ replicate 5 $ conditions record
    bs = concat $ replicate 5 $ brokens record

main :: IO ()
main = loadInput >>= print . sum . map (solutions . expand) . parse
