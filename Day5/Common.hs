module Day5.Common where

import           Data.Maybe   (fromMaybe)
import qualified Data.Set     as Set
import           Utils.Lists  (maybeHead)
import           Utils.Parser (Parser, digits, doParse, integer, some, string,
                               token, whitespace, (<|>))
import Data.List (sortBy)
import Data.Function (on)

type SeedRange = (Int, Int)

data Transform = Transform { destStart :: Int, sourceStart :: Int, rangeLength :: Int} deriving (Eq, Show)

data Almanac = Almanac {
    seeds :: [Int],
    seedsToSoils :: [Transform],
    soilsToFertilizers :: [Transform],
    fertilizersToWaters :: [Transform],
    watersToLights :: [Transform],
    lightsToTemperatures :: [Transform],
    temperaturesToHumidities :: [Transform],
    humiditiesToLocations :: [Transform]
} deriving (Eq, Show)

parse :: String -> Almanac
parse = doParse parser where
    parser = do
        string "seeds:";
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
parseSeeds = do {parseMore <|> parseLast} where
    parseMore = do {whitespace; num <- integer; rest <- parseSeeds; return (num : rest)}
    parseLast = do {whitespace; num <- integer; return [num]}

parseTransforms :: Parser [Transform]
parseTransforms = do {parseMore <|> parseLast} where
    parseMore = do {ds <- integer; whitespace; ss <- integer; whitespace; len <- integer; token '\n'; rest <- parseTransforms; return $ Transform ds ss len : rest}
    parseLast = do {ds <- integer; whitespace; ss <- integer; whitespace; len <- integer; token '\n'; return [Transform ds ss len]}

-- progress :: [Transform] -> SeedRange -> [SeedRange]
progress transforms input = result where
    -- Find all transforms that can process either the start or the end of the range.
    applicableTransforms = sortBy (compare `on` sourceStart) [tf | tf <- transforms, sourceStart tf <= start || (end < sourceStart tf + rangeLength tf)]
    result = progress' input applicableTransforms where
        progress' ()
        progress' _ [] = []

    -- If start can be mapped, map as far as possible and call recursive again
    -- If start cannot be mapped, find first point where it can be and call recursive again

solve :: (Almanac -> [SeedRange]) -> Almanac -> Maybe Transform
    -- Merge tuples after every call to progress
solve seedsFn a = progress (seedsToSoils a) (head (seedsFn a))
