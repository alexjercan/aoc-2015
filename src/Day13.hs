module Day13 where

import Control.Applicative (Alternative((<|>)))
import Control.Arrow (Arrow((&&&)))
import Data.List (nub, permutations)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Text.Parsec (char, space, string)
import Util.Extra (mkPairs')
import Util.Parser (Parser, intP, parseList, word)

happyP :: Parser Int
happyP = (string "gain " >> intP) <|> (string "lose " >> ((-1) *) <$> intP)

tableP :: Parser ((String, String), Int)
tableP = do
    s1 <- word
    v <- string " would " >> happyP
    s2 <- string " happiness units by sitting next to " >> word
    _ <- char '.'
    return ((s1, s2), v)

type Input = M.Map (String, String) Int

nodes :: Eq a => M.Map (a, a) b -> [a]
nodes = nub . map fst . M.keys

solution :: (Num a1, Ord a2) => M.Map (a2, a2) a1 -> [a1]
solution m =
    map (foldl (\acc k -> acc + at m k 0 + at m (swap k) 0) 0 . mkPairs') $
    permutations keys
  where
    keys = nodes m
    at m k v = fromMaybe v (m M.!? k)

parseContent :: String -> Input
parseContent = M.fromList . parseList tableP . lines

solve1 = maximum . solution

solve2 = maximum . solution . insertMe
  where
    insertMe = M.insert ("me", "me") 0

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
