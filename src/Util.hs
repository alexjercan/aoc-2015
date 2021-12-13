module Util where

import Data.Char (isSpace)
import Text.Parsec (Parsec, many1, digit, letter, char)
import Text.Parsec.Error (ParseError)
import qualified Text.Parsec as Parsec

rstrip :: [Char] -> [Char]
rstrip = reverse . dropWhile isSpace . reverse

type Parser = Parsec String ()

parse :: Parser a -> String -> Either ParseError a
parse p = Parsec.parse p ""

parseList :: Parser a -> [String] -> [a]
parseList p = either (error . show) id . mapM (parse p)

integerP :: Parser Int
integerP = read <$> many1 digit

charP :: Char -> Parser Char
charP = char
