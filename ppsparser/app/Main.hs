module Main where

import PPSParser
import PPSTypes
import qualified Data.Text.IO as T 
import Text.Parsec

main :: IO ()
main = do
  text1 <- T.readFile "test.txt"
  --text2 <- readFile "test2.txt"
  --print $ runParser parseHeaderAny emptyOutput "(unknown)" text2
  print $ runParser potatoParse emptyOutput "(unknown)" text1
