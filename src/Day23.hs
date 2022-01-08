{-# LANGUAGE TupleSections #-}
module Day23 where

import Control.Applicative (Alternative((<|>)))
import Text.Parsec.Char (char, letter, string)
import Util.Parser (Parser, intP, parseList)
import Control.Arrow (Arrow((&&&)))
import Control.Monad.State (State, modify, get, evalState)
import qualified Data.Map.Strict as M

data Instr
    = Hlf Char
    | Tpl Char
    | Inc Char
    | Jmp Int
    | Jie Char Int
    | Jio Char Int
    deriving (Show)

instrP :: Parser Instr
instrP =
    Hlf <$> (string "hlf " >> letter) <|> Tpl <$> (string "tpl " >> letter) <|>
    Inc <$> (string "inc " >> letter) <|>
    (char 'j' >>
     (Jmp <$> (string "mp " >> intP) <|>
      (char 'i' >>
       (Jie <$> (string "e " >> letter) <*> (string ", " >> intP) <|>
        (Jio <$> (string "o " >> letter) <*> (string ", " >> intP))))))

type Input = [Instr]

parseContent :: String -> Input
parseContent = parseList instrP . lines

type Comp = (M.Map Char Int, Int, [Instr])
type Eval = State Comp

eval :: M.Map Char Int -> Int -> Instr -> (M.Map Char Int, Int)
eval reg ip (Hlf c) = (M.insertWith (flip div) c 2 reg, ip + 1)
eval reg ip (Tpl c) = (M.insertWith (*) c 3 reg, ip + 1)
eval reg ip (Inc c) = (M.insertWith (+) c 1 reg, ip + 1)
eval reg ip (Jmp i) = (reg, ip + i)
eval reg ip (Jie c i) = (reg, ip + if even (reg M.! c) then i else 1)
eval reg ip (Jio c i) = (reg, ip + if 1 == (reg M.! c) then i else 1)

step :: Comp -> Comp
step (reg, ip, is) = (reg', ip', is)
    where instr = is !! ip
          (reg', ip') = eval reg ip instr

solveM :: Char -> Eval Int
solveM c = do
    (reg, ip, is) <- get
    if ip < 0 || ip >= length is
       then return $ reg M.! c
       else modify step >> solveM c

solve1 = evalState (solveM 'b') . (M.fromList [('a', 0), ('b', 0)], 0,)

solve2 = evalState (solveM 'b') . (M.fromList [('a', 1), ('b', 0)], 0,)

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
