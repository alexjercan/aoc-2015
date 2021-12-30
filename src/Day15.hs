module Day15 where

import Control.Arrow (Arrow((&&&)))
import Data.List (transpose)
import Text.Parsec (string)
import Util.Parser (Parser, intP, parseList, word)

ingredientP :: Parser [Int]
ingredientP = do
    _ <- word >> string ": capacity "
    c <- intP
    _ <- string ", durability "
    d <- intP
    _ <- string ", flavor "
    f <- intP
    _ <- string ", texture "
    t <- intP
    _ <- string ", calories "
    c' <- intP
    return [c, d, f, t, c']

type Input = [[Int]]

parseContent :: String -> Input
parseContent = parseList ingredientP . lines

solution :: (Ord b, Num b, Enum b) => [[b]] -> [(b, b)]
solution xs =
    [ (product . take 4 &&& last) $
    map (max 0 . sum . zipWith (*) [i, j, k, l]) (transpose xs)
    | i <- [0 .. 100]
    , j <- [0 .. 100 - i]
    , k <- [0 .. 100 - i - j]
    , l <- [100 - i - j - k]
    ]

solve1 :: [[Int]] -> Int
solve1 = maximum . map fst . solution

solve2 :: [[Int]] -> Int
solve2 = maximum . map fst . filter ((== 500) . snd) . solution

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
