-- |
-- Module      : Utils.IO
-- Description : Contains methods to handle input/output parsing.
-- Copyright   : (c) Pieter De Clercq, 2022
-- License     : MIT
--
-- Contains methods to handle input/output parsing.
module Utils.IO (module Utils.IO) where

import System.Environment (getArgs)

-- | Interprets the first passed argument to the program as a file and returns
--  its contents.
loadInput :: IO String
loadInput = do file : _ <- getArgs; readFile file
