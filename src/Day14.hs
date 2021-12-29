module Day14 where

import Control.Arrow (Arrow((&&&)))
import Text.Parsec (string)
import Util.Parser (Parser, intP, parseList, word)

raindeerP :: Parser (Int, Int, Int)
raindeerP =
    (,,) <$> (word *> string " can fly " *> intP) <*>
    (string " km/s for " *> intP) <*>
    (string " seconds, but then must rest for " *> intP <* string " seconds.")

type Input = [(Int, Int, Int)]

parseContent :: String -> Input
parseContent = parseList raindeerP . lines

stepN :: Int -> (Int, Int, Int) -> Int
stepN = go 0 0
  where
    go m d n x@(v, t, r)
        | n <= 0 = d
        | m < t = go (m + 1) (d + v) (n - 1) x
        | otherwise = go 0 d (n - r) x

solve1 :: [(Int, Int, Int)] -> Int
solve1 = maximum . map (stepN 2503)

stepOnceR :: (Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int)
stepOnceR (v, t, r, m, d, s)
    | m < t = (v, t, r, m + 1, d + v, s)
    | m == t + r - 1 = (v, t, r, 0, d, s)
    | otherwise = (v, t, r, m + 1, d, s)

stepOnce :: [(Int, Int, Int, Int, Int, Int)] -> [(Int, Int, Int, Int, Int, Int)]
stepOnce xs =
    map (\x@(v, t, r, m, d, s) ->
             if d == mx
                 then (v, t, r, m, d, s + 1)
                 else x)
        xs'
  where
    mx = maximum $ map (\(v, t, r, m, d, s) -> d) xs'
    xs' = map stepOnceR xs

solve2 :: [(Int, Int, Int)] -> Int
solve2 =
    maximum .
    map (\(_, _, _, _, _, s) -> s) .
    (!! 2503) . iterate stepOnce . map (\(v, t, r) -> (v, t, r, 0, 0, 0))

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
