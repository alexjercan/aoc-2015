module Main where

import Text.Printf (printf)

import qualified Day01
import System.Environment (getArgs)
import System.Exit (exitSuccess, ExitCode (ExitFailure), exitWith)

runDay :: Int -> String -> IO ()
runDay 1 = Day01.solve
runDay _ = undefined

readDay :: Int -> IO String
readDay day = do
    readFile $ "input/day" ++ printf "%02d" day ++ ".input"

mainOffline :: Int ->  IO ()
mainOffline day = do
    content <- readDay day
    runDay day content

main :: IO ()
main = do
    args <- getArgs
    (method, day) <- parse args
    case method of
        0 -> mainOffline day
        _ -> undefined

parse :: [[Char]] -> IO (Int, Int)
parse ["-h"]            = usage   >> exitSuccess
parse ["-v"]            = version >> exitSuccess
parse []                = read <$> getContents
parse [day]             = return (0, read day)
parse _                 = usage >> exitWith (ExitFailure 1)

usage :: IO ()
usage   = putStrLn "Usage: aoc2015 [-vh] day"

version :: IO ()
version = putStrLn "Aoc2015"

