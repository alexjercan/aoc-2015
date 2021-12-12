module Day01 where

import Control.Arrow ((&&&))

import Util ( rstrip )

parseContent :: String -> [Int]
parseContent = map parseParen . rstrip

parseParen :: Char -> Int
parseParen '(' = 1
parseParen ')' = -1
parseParen _ = undefined

solve1 :: String -> Int
solve1 = sum . parseContent

solve2 :: String -> Int
solve2 xs = fst $ head $ dropWhile ((>=0) . snd) $ zip [1..] (scanl1 (+) $ parseContent xs)

solve :: String -> IO ()
solve = print . (solve1 &&& solve2)
