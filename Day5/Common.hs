module Day5.Common where

import           Data.Maybe   (fromMaybe)
import           Utils.Lists  (maybeHead)
import           Utils.Parser (Parser, digits, doParse, integer, some, string,
                               token, whitespace, (<|>))

data Range = Range { destStart :: Int, sourceStart :: Int, rangeLength :: Int} deriving (Eq, Show)

data Almanac = Almanac { seeds :: [Int], seedsToSoils :: [Range], soilsToFertilizers :: [Range], fertilizersToWaters :: [Range], watersToLights :: [Range], lightsToTemperatures :: [Range], temperaturesToHumidities :: [Range], humiditiesToLocations :: [Range]} deriving (Eq, Show)

parse :: String -> Almanac
parse = doParse parser where
    parser = do
        string "seeds:";
        seeds <- parseSeeds
        string "\n\nseed-to-soil map:\n"
        seedsToSoils <- parseRanges
        string "\nsoil-to-fertilizer map:\n"
        soilsToFertilizers <- parseRanges
        string "\nfertilizer-to-water map:\n"
        fertilizersToWaters <- parseRanges
        string "\nwater-to-light map:\n"
        watersToLights <- parseRanges
        string "\nlight-to-temperature map:\n"
        lightsToTemperatures <- parseRanges
        string "\ntemperature-to-humidity map:\n"
        temperaturesToHumidities <- parseRanges
        string "\nhumidity-to-location map:\n"
        Almanac seeds seedsToSoils soilsToFertilizers fertilizersToWaters watersToLights lightsToTemperatures temperaturesToHumidities <$> parseRanges

parseSeeds :: Parser [Int]
parseSeeds = do {parseMore <|> parseLast} where
    parseMore = do {whitespace; num <- integer; rest <- parseSeeds; return (num : rest)}
    parseLast = do {whitespace; num <- integer; return [num]}

parseRanges :: Parser [Range]
parseRanges = do {parseMore <|> parseLast} where
    parseMore = do {ds <- integer; whitespace; ss <- integer; whitespace; len <- integer; token '\n'; rest <- parseRanges; return $ Range ds ss len : rest}
    parseLast = do {ds <- integer; whitespace; ss <- integer; whitespace; len <- integer; token '\n'; return [Range ds ss len]}

mapping :: [Range] -> Int -> Int
mapping range seed = maybe seed (\r -> destStart r + (seed - sourceStart r)) match where
    match = maybeHead [r | r <- range, sourceStart r <= seed && (seed < sourceStart r + rangeLength r)]

solve :: (Almanac -> [Int]) -> Almanac -> Int
solve seedsFn a = minimum $ map (mapping (humiditiesToLocations a) . mapping (temperaturesToHumidities a) . mapping (lightsToTemperatures a) . mapping (watersToLights a) . mapping (fertilizersToWaters a) . mapping (soilsToFertilizers a) . mapping (seedsToSoils a)) (seedsFn a)
