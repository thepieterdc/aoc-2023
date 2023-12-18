module Day18.Advanced where

import Data.Text.Internal.Read (hexDigitToInt)
import Day18.Common (Instruction (..), findEdges, parse, solve)
import Utils.Grid (Direction (..))
import Utils.IO (loadInput)
import Utils.Number (hexToDec)
import Utils.Parser (Parser, chars, eol, integer, string, token, until, (<|>))
import Prelude hiding (until)

charToDirection :: Char -> Direction
charToDirection '0' = East
charToDirection '1' = South
charToDirection '2' = West
charToDirection '3' = North

parseInstruction :: Parser Instruction
parseInstruction = do
  until '('
  string " (#"
  colour <- chars 6
  token ')'
  eol
  return $ Instruction (charToDirection $ last colour) (hexToDec $ take 5 colour)

main :: IO ()
main = loadInput >>= print . solve . findEdges (0, 0) (0, []) . parse parseInstruction
