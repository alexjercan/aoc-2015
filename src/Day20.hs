module Day20 where

import Control.Arrow (Arrow((&&&)))
import Data.List (find)
import Util.Factor (allFactors)

type Input = Int

parseContent :: String -> Input
parseContent = read

score :: Int -> Int
score n = 10 * sum (allFactors n)

score' :: Int -> Int
score' n = 11 * sum (filter ((<= 50) . div n) (allFactors n))

solution :: Int -> (Int -> Int) -> Maybe Int
solution x s = find ((x <=) . s) [2 ..]

solve1 :: Int -> Maybe Int
solve1 x = solution x score

solve2 :: Int -> Maybe Int
solve2 x = solution x score'

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
