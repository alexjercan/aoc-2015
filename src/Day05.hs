module Day05 where
import Control.Arrow (Arrow((&&&)))
import Data.List (isInfixOf, group)

type Input = [String]

parseContent :: String -> Input
parseContent = lines

naughtySubstrings :: [String]
naughtySubstrings = ["ab", "cd", "pq", "xy"]

vowels :: String
vowels = "aeiou"

containsNaughtySubstring :: String -> Bool
containsNaughtySubstring x = any (`isInfixOf` x) naughtySubstrings

containsThreeVowels :: String -> Bool
containsThreeVowels = (>=3) . length . filter (`elem` vowels)

containsCharTwiceInARow :: String -> Bool
containsCharTwiceInARow = any ((>1) . length) . group

isNice :: String -> Bool
isNice x = not (containsNaughtySubstring x) && containsThreeVowels x && containsCharTwiceInARow x

solve1 :: [String] -> Int
solve1 = length . filter isNice

hasThreePalindrome :: String -> Bool
hasThreePalindrome (a:rs'@(b:c:rs)) = a == c || hasThreePalindrome rs'
hasThreePalindrome _ = False

containsOrdered :: Char -> Char -> String -> Bool
containsOrdered x y (a:rs'@(b:rs)) = (x == a && y == b) || containsOrdered x y rs'
containsOrdered _ _ _ = False

hasNonOverlapPairs :: String -> Bool
hasNonOverlapPairs (x:rs'@(y:rs)) = containsOrdered x y rs || hasNonOverlapPairs rs'
hasNonOverlapPairs _ = False

isNice' :: String -> Bool
isNice' x = hasThreePalindrome x && hasNonOverlapPairs x

solve2 = length . filter isNice'

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
