module Day07 where

import Control.Applicative (Alternative((<|>)))
import Control.Arrow (Arrow((&&&)))
import Data.Bifunctor (Bifunctor(second))
import Data.Bits (Bits((.&.), (.|.), complement, shift))
import Data.Functor ((<&>))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Tuple (swap)
import Text.Parsec (anyToken, space, string, try)
import Util.Parser (Parser, intP, parseList, word)

data Wire
    = Label String
    | Const Int
    deriving (Show)

wireP :: Parser Wire
wireP = Label <$> word <|> Const <$> intP

data Gate
    = Wire Wire
    | And Wire Wire
    | Or Wire Wire
    | LShift Wire Int
    | RShift Wire Int
    | Not Wire
    deriving (Show)

andP :: Parser Gate
andP = And <$> (wireP <* (space >> string "AND" >> space)) <*> wireP

orP :: Parser Gate
orP = Or <$> (wireP <* (space >> string "OR" >> space)) <*> wireP

lShiftP :: Parser Gate
lShiftP = LShift <$> (wireP <* (space >> string "LSHIFT" >> space)) <*> intP

rShiftP :: Parser Gate
rShiftP = RShift <$> (wireP <* (space >> string "RSHIFT" >> space)) <*> intP

notP :: Parser Gate
notP = Not <$> ((string "NOT" >> space) *> wireP)

gateP :: Parser (String, Gate)
gateP = swap <$> ((,) <$> a <*> b)
  where
    a =
        try andP <|> try orP <|> try lShiftP <|> try rShiftP <|> try notP <|>
        try (Wire <$> wireP)
    b = (space >> string "->" >> space) *> word

type Input = [(String, Gate)]

parseContent :: String -> Input
parseContent = parseList gateP . lines

solveWire :: M.Map String Int -> Wire -> Maybe Int
solveWire m ((Const a)) = Just a
solveWire m ((Label a)) = m M.!? a

solveGate :: M.Map String Int -> Gate -> Maybe Int
solveGate m (Wire w) = solveWire m w
solveGate m (And w1 w2) = (.&.) <$> solveWire m w1 <*> solveWire m w2
solveGate m (Or w1 w2) = (.|.) <$> solveWire m w1 <*> solveWire m w2
solveGate m (LShift w n) = solveWire m w <&> (`shift` n)
solveGate m (RShift w n) = solveWire m w <&> (`shift` (-n))
solveGate m (Not w) = complement <$> solveWire m w

step :: M.Map String Int -> Input -> M.Map String Int
step m =
    M.union m .
    M.fromList .
    map (second fromJust) .
    filter (\(l, v) -> l `M.notMember` m && isJust v) .
    map (second (solveGate m))

solution :: M.Map String Int -> Input -> Int
solution m =
    (M.! "a") . head . dropWhile (not . M.member "a") . scanl step m . repeat

solve1 :: Input -> Int
solve1 = solution M.empty

solve2 xs = solution (M.singleton "b" a) xs
  where
    a = solve1 xs

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
