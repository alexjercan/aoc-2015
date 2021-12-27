module Day11 where

import Control.Arrow (Arrow((&&&)))
import qualified Data.Map.Strict as M
import Util.Extra (rstrip)

encode :: Integral a => a -> a -> [a]
encode x base = encode' x []
  where
    encode' x' z
        | x' == 0 = z
        | otherwise = encode' (div x' base) (mod x' base : z)

decode :: (Foldable t, Num a) => t a -> a -> a
decode num base =
    fst $ foldr (\a (b, i) -> (b + a * base ^ i, i + 1)) (0, 0) num

type Input = String

parseContent :: String -> Input
parseContent = rstrip

oneIncreasingStraight :: [Char] -> Bool
oneIncreasingStraight (x:ys@(y:z:xs))
    | fromEnum x + 1 == fromEnum y && fromEnum y + 1 == fromEnum z = True
    | otherwise = oneIncreasingStraight ys
oneIncreasingStraight _ = False

notContainsIOL :: Foldable t => t Char -> Bool
notContainsIOL xs = 'i' `notElem` xs && 'o' `notElem` xs && 'l' `notElem` xs

twoNonOverlappingPairs :: [Char] -> Bool
twoNonOverlappingPairs = go True
  where
    go True (x:y:xs)
        | x == y = go False xs
        | otherwise = go True (y : xs)
    go False (x:y:xs)
        | x == y = True
        | otherwise = go False (y : xs)
    go _ _ = False

check :: [Char] -> Bool
check xs =
    oneIncreasingStraight xs && notContainsIOL xs && twoNonOverlappingPairs xs

increment :: String -> String
increment xs =
    map (\x -> toEnum (x + 97) :: Char) $
    encode (decode (map (\x -> fromEnum x - 97) xs) 26 + 1) 26

solution :: String -> [Char]
solution = head . dropWhile (not . check) . iterate increment

solve1 :: String -> String
solve1 = solution

solve2 :: String -> [Char]
solve2 = solution . increment . solution

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
