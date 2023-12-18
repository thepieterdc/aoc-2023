module Day18.Simple where

import Day18.Common (Instruction (..), findEdges, parse, solve)
import Utils.Grid (Direction (..))
import Utils.IO (loadInput)
import Utils.Parser (Parser, chars, eol, integer, string, token, until, whitespace, (<|>))
import Prelude hiding (until)

parseDirection :: Parser Direction
parseDirection = east <|> north <|> south <|> west
  where
    east = token 'R' >> return East
    north = token 'U' >> return North
    south = token 'D' >> return South
    west = token 'L' >> return West

parseInstruction :: Parser Instruction
parseInstruction = do
  dir <- parseDirection
  token ' '
  amount <- integer
  whitespace
  until '\n'
  return $ Instruction dir amount

main :: IO ()
main = loadInput >>= print . solve . findEdges (0, 0) (0, []) . parse parseInstruction
