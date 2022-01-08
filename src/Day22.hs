module Day22 where

import Control.Arrow (Arrow((&&&)))
import Text.Parsec (space, string)
import Util.Parser (Parser, intP, parse)

data Effect =
    Effect Int (Stats -> Stats)

data Stats = Stats Int Int Int Int deriving Show

data Player =
    Player Stats [Effect]

playerP :: Parser Stats
playerP = do
    hp <- string "Hit Points: " >> (intP <* space)
    dmg <- string "Damage: " >> (intP <* space)
    return $ Stats hp dmg 0 0

type Input = Stats

parseContent :: String -> Input
parseContent = either (error . show) id . parse playerP

solve1 = id

solve2 = const ()

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
