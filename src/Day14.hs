module Day14 where

import Control.Arrow ((&&&))

solve1 :: String -> Int
solve1 = const 0

solve2 :: String -> Int
solve2 = const 0

solve :: String -> IO ()
solve = print . (solve1 &&& solve2)
