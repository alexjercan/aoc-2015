module Day01 where

import Util.Extra ( rstrip )
import Control.Arrow ((&&&))

type Input = [Int]

parseContent :: String -> Input
parseContent = map parseParen . rstrip

parseParen :: Char -> Int
parseParen '(' = 1
parseParen ')' = -1
parseParen _ = undefined

solve1 :: Input -> Int
solve1 = sum

solve2 :: Input -> Int
solve2 = fst . head . dropWhile ((>=0) . snd) . zip [1..] . scanl1 (+)

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
