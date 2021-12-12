module Util where

import Data.Char (isSpace)

rstrip :: [Char] -> [Char]
rstrip = reverse . dropWhile isSpace . reverse
