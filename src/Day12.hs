{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import Control.Arrow (Arrow((&&&)))
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.String (fromString)

type Input = String

parseContent :: String -> Value
parseContent = fromMaybe Null . decode . fromString

solve1 :: Value -> Int
solve1 Null = 0
solve1 (Bool _) = 0
solve1 (Number x) = round x
solve1 (String _) = 0
solve1 (Array xs) = foldl (\acc v -> acc + solve1 v) 0 xs
solve1 (Object xs) = foldl (\acc v -> acc + solve1 v) 0 xs

solve2 :: Value -> Int
solve2 Null = 0
solve2 (Bool _) = 0
solve2 (Number x) = round x
solve2 (String _) = 0
solve2 (Array xs) = foldl (\acc v -> acc + solve2 v) 0 xs
solve2 (Object xs)
    | containsRed xs = 0
    | otherwise = foldl (\acc v -> acc + solve2 v) 0 xs

containsRed :: Object -> Bool
containsRed = foldl (\acc v -> acc || isRed v) False
    where isRed (String "red") = True
          isRed _ = False

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
