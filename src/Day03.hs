{-# LANGUAGE DeriveFunctor #-}
module Day03 where

import Util (charP, Parser, manyP, parse, counter)
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import qualified Data.Map as M

data Position a = Position a a deriving (Show, Ord, Eq, Functor)

instance Applicative Position where
    pure a = Position a a
    (<*>) (Position fa fb) (Position a b) = Position (fa a) (fb b)

instance Semigroup a => Semigroup (Position a) where
    (<>) (Position a b) (Position c d) = Position (a <> c) (b <> d)

instance Monoid a => Monoid (Position a) where
    mempty = Position mempty mempty

instance Semigroup Int where
    (<>) = (+)

instance Monoid Int where
  mempty = 0

data Direction = Up | LeftD | RightD | Down deriving Show

toPosition :: Direction -> Position Int
toPosition Up = Position 0 1
toPosition LeftD = Position (-1) 0
toPosition RightD = Position 1 0
toPosition Down = Position 0 (-1)

directionP :: Parser Direction
directionP = Up <$ charP '^' <|> LeftD <$ charP '<' <|> RightD <$ charP '>' <|> Down <$ charP 'v'

everyOther :: [a] -> ([a], [a])
everyOther xs = go xs [] []
    where go [] xs ys = (reverse xs, reverse ys)
          go (x:y:zs) xs ys = go zs (x:xs) (y:ys)
          go _ _ _ = undefined

evenInd :: [a] -> [a]
evenInd (x:_:xs) = x : evenInd xs
evenInd _ = []

oddInd :: [a] -> [a]
oddInd (_:y:ys) = y : oddInd ys
oddInd _ = []

type Input = [Direction]

parseContent :: String -> Input
parseContent = either (error . show) id . parse (manyP directionP)

solution :: [Direction] -> [Position Int]
solution = scanl1 (<>) . (Position 0 0:) . map toPosition

solve1 :: [Direction] -> Int
solve1 = length . counter . solution

solve2 :: [Direction] -> Int
solve2 = length . counter . ((++) <$> solution . evenInd <*> solution . oddInd)

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
