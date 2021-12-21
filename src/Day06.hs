module Day06 where

import Control.Applicative (Alternative((<|>)))
import Control.Arrow (Arrow((&&&)))
import Text.Parsec (char, choice, space, string)
import Util.Parser (Parser, naturalP, parseList)
import qualified Data.Set as S
import qualified Data.Map as M

toggleS :: Ord a => a -> S.Set a -> S.Set a
toggleS a s
    | a `S.member` s = S.delete a s
    | otherwise      = S.insert a s

data Light
    = On
    | Off
    | Toggle
    deriving (Show)

data Grid a =
    Grid a a a a
    deriving (Show)

data Line = Line Light (Grid Int) deriving Show

lightP :: Parser Light
lightP =
    char 't' >>
    ((Toggle <$ string "oggle") <|>
     (string "urn o" >> ((On <$ string "n") <|> Off <$ string "ff")))

gridP :: Parser (Grid Int)
gridP = do
    xs <- naturalP
    _ <- char ','
    ys <- naturalP
    _ <- space >> string "through" >> space
    xe <- naturalP
    _ <- char ','
    Grid xs xe ys <$> naturalP

lineP :: Parser Line
lineP = do
    light <- lightP
    _ <- space
    Line light <$> gridP

type Input = [Line]

parseContent :: String -> Input
parseContent = parseList lineP . lines

indices :: Grid Int -> [(Int, Int)]
indices (Grid xs xe ys ye) = [(x, y) | x <- [xs..xe], y <- [ys..ye]]

step :: S.Set (Int, Int) -> Line -> S.Set (Int, Int)
step s (Line On g) = foldr S.insert s $ indices g
step s (Line Off g) = foldr S.delete s $ indices g
step s (Line Toggle g) = foldr toggleS s $ indices g

solve1 :: [Line] -> Int
solve1 = length . foldl step S.empty

decrementOne :: Int -> Int -> Int
decrementOne _ a = max 0 (a - 1)

step' :: M.Map (Int, Int) Int -> Line -> M.Map (Int, Int) Int
step' m (Line On g) = foldr (flip (M.insertWith (+)) 1) m $ indices g
step' m (Line Off g) = foldr (flip (M.insertWith decrementOne) 0) m $ indices g
step' m (Line Toggle g) = foldr (flip (M.insertWith (+)) 2) m $ indices g

solve2 :: [Line] -> Int
solve2 = sum . M.elems . foldl step' M.empty

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
