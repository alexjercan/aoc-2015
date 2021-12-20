module Day02 where

import Util.Parser (Parser, integerP, charP, parseList)
import Control.Arrow ((&&&))

data Rectangle = Rectangle Int Int Int deriving Show
type Input = [Rectangle]

rectangleParser :: Parser Rectangle
rectangleParser = do
    a <- integerP
    charP 'x'
    b <- integerP
    charP 'x'
    Rectangle a b <$> integerP

parseContent :: String -> Input
parseContent = parseList rectangleParser . lines

area :: Rectangle -> Int
area (Rectangle a b c) = (*2) $ sum [a*b, a*c, b*c]

slack :: Rectangle -> Int
slack (Rectangle a b c) = minimum [a*b, a*c, b*c]

volume :: Rectangle -> Int
volume (Rectangle a b c) = product [a, b, c]

ribbon :: Rectangle -> Int
ribbon (Rectangle a b c) = (*2) $ minimum [a+b, a+c, b+c]

solve1 :: Input -> Int
solve1 = sum . map ((+) . area <*> slack)

solve2 :: Input -> Int
solve2 = sum . map ((+) . ribbon <*> volume)

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
