module Day18 where

import Control.Arrow (Arrow((&&&)))
import qualified Data.Set as S

countNeighbors :: S.Set (Int, Int) -> (Int, Int) -> Int
countNeighbors s (x, y) =
    length $
    filter (`S.member` s) $
    [(i, j) | i <- [x - 1 .. x + 1], j <- [y - 1 .. y + 1], i /= x || j /= y]

type Input = [[Char]]

parseContent = lines

rule :: S.Set (Int, Int) -> (Int, Int) -> Bool
rule s p
    | p `S.member` s = n == 2 || n == 3
    | otherwise = n == 3
  where
    n = countNeighbors s p

step :: Int -> Int -> S.Set (Int, Int) -> S.Set (Int, Int)
step n m s = foldl (\acc p -> if rule s p then S.insert p acc else acc) S.empty indices
  where
    indices = [(i, j) | i <- [0 .. n - 1], j <- [0 .. m - 1]]

onCorners :: Int -> Int -> S.Set (Int, Int) -> S.Set (Int, Int)
onCorners n m s = foldr S.insert s cs
    where cs = [(0, 0), (0, m-1), (n-1, 0), (n-1, m-1)]

solve1 ms = length $ (!!100) $ iterate (step n m) s
  where
    n = length ms
    m = length $ head ms
    s =
        S.fromList
            [(i, j) | i <- [0 .. n - 1], j <- [0 .. m - 1], ms !! i !! j == '#']

solve2 ms = length $ (!!100) $ iterate (onCorners n m . step n m . onCorners n m) s
  where
    n = length ms
    m = length $ head ms
    s =
        S.fromList
            [(i, j) | i <- [0 .. n - 1], j <- [0 .. m - 1], ms !! i !! j == '#']

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
