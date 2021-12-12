{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day16Test where

import Day16
import Test.Framework

sampleInput :: String
sampleInput = ""

test_solve1 :: IO ()
test_solve1 = do
    assertEqual 0 $ solve1 sampleInput

test_solve2 :: IO ()
test_solve2 = do
    assertEqual 0 $ solve2 sampleInput
