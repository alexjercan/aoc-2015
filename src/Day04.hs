module Day04 where

import Data.ByteString.Conversion ( toByteString )
import Control.Arrow ((&&&))
import Util.Extra (rstrip)
import Data.Digest.Pure.MD5 ( md5 )

type Input = String

parseContent :: String -> Input
parseContent = rstrip

firstNZero :: Int -> [Char] -> Bool
firstNZero n = all ('0' ==) . take n

checkNumber :: Int -> String -> Int -> Bool
checkNumber n s = firstNZero n . show . md5 . toByteString . (s ++) . show

solve1 :: String -> Int
solve1 s = head $ filter (checkNumber 5 s) [1..]

solve2 :: String -> Int
solve2 s = head $ filter (checkNumber 6 s) [1..]

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
