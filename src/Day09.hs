module Day09 where

import qualified Data.Map.Strict as M
import Text.Parsec ( string )
import Util.Parser ( Parser, parseList, intP, word )
import Data.List ( nub, permutations )
import Data.Tuple ( swap )
import Control.Arrow
import Data.Maybe
import Util.Extra

edgeP :: Parser ((String, String), Int)
edgeP = (,) <$> ((,) <$> (word <* string " to ") <*> word) <*> (string " = " *> intP)

type Input = M.Map (String, String) Int

mkUndirected :: M.Map (String, String) Int -> M.Map (String, String) Int
mkUndirected = M.foldlWithKey (\acc k a -> M.insert (swap k) a (M.insert k a acc)) M.empty

parseContent :: String -> Input
parseContent = mkUndirected . M.fromList . parseList edgeP . lines

solution :: (Num a1, Ord a2) => M.Map (a2, a2) a1 -> [a1]
solution m = map (foldl (\acc k -> acc + m M.! k) 0 . mkPairs) $ permutations $ nub $ map fst $ M.keys m

solve1 :: M.Map (String, String) Int -> Int
solve1 = minimum . solution

solve2 :: M.Map (String, String) Int -> Int
solve2 = maximum . solution

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
