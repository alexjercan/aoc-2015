module Day17 where

import Control.Arrow (Arrow((&&&)))
import Data.List (subsequences, minimumBy)
import Data.Function (on)

type Input = [Int]

parseContent :: String -> Input
parseContent = map read . lines

solution :: [Int] -> [[Int]]
solution = filter ((==150) . sum) . subsequences

solve1 :: [Int] -> Int
solve1 = length . solution

solve2 :: [Int] -> Int
solve2 xs = length $ filter ((==length mx) . length) xs'
    where xs' = solution xs
          mx = minimumBy (compare `on` length) xs'

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
