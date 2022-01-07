module Day21 where

import Control.Arrow (Arrow((&&&)))
import Data.Bifunctor (Bifunctor(first))
import Data.List (subsequences)
import Text.Parsec (space, string)
import Util.Parser (Parser, intP, parse)

data Player =
    Player Int Int Int
    deriving (Show)

playerP :: Parser Player
playerP = do
    hp <- string "Hit Points: " >> (intP <* space)
    dmg <- string "Damage: " >> (intP <* space)
    arm <- string "Armor: " >> (intP <* space)
    return $ Player hp dmg arm

type Input = Player

parseContent :: String -> Input
parseContent = either (error . show) id . parse playerP

data Item =
    Item Int Int Int
    deriving (Show)

weapons :: [Item]
weapons = [Item 8 4 0, Item 1 5 0, Item 25 6 0, Item 40 7 0, Item 74 8 0]

armors :: [Item]
armors =
    [ Item 0 0 0
    , Item 13 0 1
    , Item 31 0 2
    , Item 53 0 3
    , Item 75 0 4
    , Item 102 0 5
    ]

rings :: [Item]
rings =
    [ Item 0 0 0
    , Item 0 0 0
    , Item 25 1 0
    , Item 50 2 0
    , Item 100 3 0
    , Item 20 0 1
    , Item 40 0 2
    , Item 80 0 3
    ]

loadouts :: [(Item, Item, [Item])]
loadouts = [(w, a, rs) | w <- weapons, a <- armors, rs <- rings']
  where
    rings' = filter ((<= 2) . length) $ subsequences rings

turn :: (Player, Player, Bool) -> (Player, Player, Bool)
turn (p1@(Player _ dmg1 _), Player hp2 dmg2 arm2, isPlayer) =
    (Player (hp2 - dmg) dmg2 arm2, p1, not isPlayer)
  where
    dmg = max 1 (dmg1 - arm2)

equip :: (Item, Item, [Item]) -> (Player, Int)
equip (w, a, rs) =
    foldl
        (\(Player hp dmg arm, cost) (Item c d a) ->
             (Player hp (dmg + d) (arm + a), cost + c))
        (Player 100 0 0, 0)
        (w : a : rs)

simulate :: Player -> Player -> Bool
simulate p1 p2 = go (p1, p2, True)
  where
    isDead (Player hp _ _, _, _) = hp <= 0
    go state@(_, _, isPlayer) =
        if isDead state
            then not isPlayer
            else go (turn state)

solution :: Player -> [(Bool, Int)]
solution boss = map (first (`simulate` boss) . equip) loadouts

solve1 :: Player -> Int
solve1 = minimum . map snd . filter fst . solution

solve2 :: Player -> Int
solve2 = maximum . map snd . filter (not . fst) . solution

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
