module Day10 where

import Data.Char (digitToInt)
import Control.Arrow (Arrow((&&&)))
import Util.Extra (rstrip)
import Data.List (group)

type Input = [Int]

parseContent :: String -> Input
parseContent = map digitToInt . rstrip

step :: [Int] -> [Int]
step = concatMap (\xs -> [length xs, head xs]) . group

solution :: Int -> [Int] -> Int
solution n = length . (!!n) . iterate step

solve1 :: [Int] -> Int
solve1 = solution 40

solve2 :: [Int] -> Int
solve2 = solution 50

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
