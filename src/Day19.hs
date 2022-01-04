module Day19 where

import Control.Arrow (Arrow((&&&)))
import Data.Bifunctor (Bifunctor(bimap, second))
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Text.Parsec (string)
import Util.Input (first2)
import Util.Parser (Parser, parseList, word)
import Data.List(group, nub, intersperse)
import Data.Char (isUpper)

replacementP :: Parser (String, String)
replacementP = (,) <$> word <*> (string " => " >> word)

type Input = (M.Map String [String], String)

parseContent :: String -> Input
parseContent =
    bimap
        (M.fromListWith (++) . map (second (: [])) . parseList replacementP)
        head .
    first2 . splitOn [""] . lines

insertAt :: String -> String -> [String] -> Int -> String
insertAt p s xs n = concat $ intersperse p (take n xs) ++ [s] ++ intersperse p (drop n xs)

replacements :: String -> String -> String -> [String]
replacements p s text = map (insertAt p s xs) [1 .. n - 1]
    where xs = splitOn p text
          n  = length xs

solve1 (ms, s) = length $ nub $ M.foldlWithKey (\acc k as -> acc ++ concatMap (\a -> replacements k a s) as) [] ms

solve2 :: Input -> Int
solve2 (_, s) = n - rn - ar - 2 * y - 1
          where rn = length (splitOn "Rn" s) - 1
                ar = length (splitOn "Ar" s) - 1
                y = length (splitOn "Y" s) - 1
                n = length $ filter isUpper s

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
