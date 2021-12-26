module Day08 where

import Control.Arrow ( Arrow((&&&)) )
import Data.List (isInfixOf)

type Input = [String]

cringeRegex :: String -> Int
cringeRegex [] = -2
cringeRegex ('\\':'\\':xs) = 1 + cringeRegex xs
cringeRegex ('\\':'"':xs) = 1 + cringeRegex xs
cringeRegex ('\\':'x':_:_:xs) = 1 + cringeRegex xs
cringeRegex (x:xs) = 1 + cringeRegex xs

parseContent = lines

solve1 xs = g xs - f xs
    where f = sum . map cringeRegex
          g = sum . map length

solve2 xs = f xs - g xs
    where f = sum . map (length . show)
          g = sum . map length

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
