module Day16 where

import Control.Arrow (Arrow((&&&)))
import qualified Data.Map.Strict as M
import Text.Parsec (sepBy, string)
import Util.Parser (Parser, intP, parseList, word)

clueP :: Parser (String, Int)
clueP = (,) <$> word <*> (string ": " >> intP)

auntP :: Parser (M.Map String Int)
auntP = do
    _ <- string "Sue " >> intP >> string ": "
    M.fromList <$> clueP `sepBy` string ", "

knownClues :: M.Map String Int
knownClues =
    M.fromList
        [ ("children", 3)
        , ("cats", 7)
        , ("samoyeds", 2)
        , ("pomeranians", 3)
        , ("akitas", 0)
        , ("vizslas", 0)
        , ("goldfish", 5)
        , ("trees", 3)
        , ("cars", 2)
        , ("perfumes", 1)
        ]

knownClues' :: M.Map String [Int]
knownClues' =
    M.fromList
        [ ("children", [3])
        , ("cats", [8 .. 100])
        , ("samoyeds", [2])
        , ("pomeranians", [0 .. 2])
        , ("akitas", [0])
        , ("vizslas", [0])
        , ("goldfish", [0 .. 4])
        , ("trees", [4 .. 100])
        , ("cars", [2])
        , ("perfumes", [1])
        ]

type Input = [M.Map String Int]

parseContent :: String -> Input
parseContent = parseList auntP . lines

checkOne :: M.Map String Int -> Bool
checkOne = M.foldlWithKey (\acc k v -> acc && (knownClues M.! k == v)) True

solve1 :: [M.Map String Int] -> Integer
solve1 = head . map fst . filter (checkOne . snd) . zip [1 ..]

checkOne' :: M.Map String Int -> Bool
checkOne' =
    M.foldlWithKey (\acc k v -> acc && (v `elem` knownClues' M.! k)) True

solve2 :: [M.Map String Int] -> Integer
solve2 = head . map fst . filter (checkOne' . snd) . zip [1 ..]

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
