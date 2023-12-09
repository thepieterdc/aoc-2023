module Day5.Common where

import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Utils.Lists (maybeHead)
import Utils.Parser
  ( Parser,
    digits,
    doParse,
    eol,
    integer,
    some,
    string,
    token,
    whitespace,
    (<|>),
  )

type SeedRange = (Int, Int)

data Transform = Transform {destStart :: Int, destEnd :: Int, sourceStart :: Int, sourceEnd :: Int} deriving (Eq, Show)

data Almanac = Almanac
  { seeds :: [Int],
    seedsToSoils :: [Transform],
    soilsToFertilizers :: [Transform],
    fertilizersToWaters :: [Transform],
    watersToLights :: [Transform],
    lightsToTemperatures :: [Transform],
    temperaturesToHumidities :: [Transform],
    humiditiesToLocations :: [Transform]
  }
  deriving (Eq, Show)

parse :: String -> Almanac
parse = doParse parser
  where
    parser = do
      string "seeds:"
      seeds <- parseSeeds
      string "\n\nseed-to-soil map:\n"
      seedsToSoils <- parseTransforms
      string "\nsoil-to-fertilizer map:\n"
      soilsToFertilizers <- parseTransforms
      string "\nfertilizer-to-water map:\n"
      fertilizersToWaters <- parseTransforms
      string "\nwater-to-light map:\n"
      watersToLights <- parseTransforms
      string "\nlight-to-temperature map:\n"
      lightsToTemperatures <- parseTransforms
      string "\ntemperature-to-humidity map:\n"
      temperaturesToHumidities <- parseTransforms
      string "\nhumidity-to-location map:\n"
      Almanac seeds seedsToSoils soilsToFertilizers fertilizersToWaters watersToLights lightsToTemperatures temperaturesToHumidities <$> parseTransforms

parseSeeds :: Parser [Int]
parseSeeds = do parseMore <|> parseLast
  where
    parseMore = do whitespace; num <- integer; rest <- parseSeeds; return (num : rest)
    parseLast = do whitespace; num <- integer; return [num]

parseTransforms :: Parser [Transform]
parseTransforms = do parseMore <|> parseLast
  where
    parseMore = do ds <- integer; whitespace; ss <- integer; whitespace; l <- integer; eol; rest <- parseTransforms; return $ Transform ds (ds + l) ss (ss + l - 1) : rest
    parseLast = do ds <- integer; whitespace; ss <- integer; whitespace; l <- integer; eol; return [Transform ds (ds + l) ss (ss + l - 1)]

progress :: [Transform] -> [SeedRange] -> [SeedRange]
progress tfs ((start, end) : rest) = progress' applicableTfs (start, end) ++ progress tfs rest
  where
    applicableTfs = sortOn sourceStart [tf | tf <- tfs, sourceStart tf <= end && sourceEnd tf >= start]
progress _ [] = []

progress' :: [Transform] -> SeedRange -> [SeedRange]
progress' [] range = [range]
progress' tfs (start, end) = tfd : (if maxTf == end then [] else progress' newTfs (maxTf + 1, end))
  where
    tf = head tfs
    maxTf = min end (sourceEnd tf)
    minTf = max start (sourceStart tf)
    tfd = if start == minTf then transform tf (start, maxTf) else (start, minTf)
    newTfs = if start == minTf then drop 1 tfs else tfs

transform :: Transform -> (Int, Int) -> (Int, Int)
transform tf (start, end) = (transformedStart, transformedEnd)
  where
    offset = start - sourceStart tf
    transformedStart = destStart tf + offset
    transformedEnd = transformedStart + (end - start)

solve :: (Almanac -> [SeedRange]) -> Almanac -> Int
solve seedsFn a =
  minimum $
    map fst $
      foldr
        (progress . (\f -> f a))
        (seedsFn a)
        [humiditiesToLocations, temperaturesToHumidities, lightsToTemperatures, watersToLights, fertilizersToWaters, soilsToFertilizers, seedsToSoils]
