{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module PPSParser(
  testParse,
  parseHeaderAny,
  potatoParse
) where

import PPSTypes
import qualified PPSToken as PT
import Control.Monad
import qualified Data.Text as T
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.Text

import Lens.Micro.Platform

potatoEndOfLine :: PotatoParser ()
potatoEndOfLine = void $ many $ do
    newline
    many (try (many (oneOf " \t") >> newline))
    return ()

parseLine :: PotatoParser String
parseLine = do
  r <- many (noneOf "\r\n")
  potatoEndOfLine
  return r

parseHeaderAny :: PotatoParser Header
parseHeaderAny = do
  many1 (char '=') >> endOfLine
  headerString <- (Prelude.foldl1 (<|>) $ map (try . string) headerStrings) <?> "valid header"
  endOfLine
  many1 (char '=') >> endOfLine
  return $ read headerString

parseHeader :: Header -> PotatoParser Header
parseHeader h = do
    let
      hs = show h
    many1 (char '=') >> endOfLine
    headerString <- try (string hs) <?> hs
    endOfLine
    many1 (char '=') >> endOfLine
    -- add it to the headers we saw
    modifyState (over headers (h:))
    return h


manyTillHeaderOrEoF :: PotatoParser a -> PotatoParser [a]
manyTillHeaderOrEoF x = manyTill x ((void . lookAhead . try $ parseHeaderAny) <|> void eof)

parseTitles :: PotatoParser (Map.Map String String)
parseTitles = do
  titles <- manyTillHeaderOrEoF parseLine
  -- first word is key, remaining is value
  return $ foldl (\m (a,b) -> Map.insert a b m) Map.empty (map makekv titles)
  where
    makekv s = (takeWhile (/=' ') s, dropWhile (/=' ') s)

-- TODO do manyTillHeaderOrEoF parseTitles version using state
--parseTitle :: PotatoParser ()
--parseTitle = do
  --s <- parseLine

parseObjects :: PotatoParser ()
parseObjects = do
  parseHeader OBJECTS
  PT.whiteSpace
  many $ do
    ident <- PT.identifier
    color <- PT.identifier
    modifyState (over objectList (Map.insert ident color))
  return ()

parseLegend :: PotatoParser ()
parseLegend = do
  objects <- getState
  parseHeader LEGEND
  PT.whiteSpace
  manyTillHeaderOrEoF $ do
    (key:[]) <- PT.identifier <|> PT.operator <?> "unreserved character"
    PT.reservedOp "="
    value <- PT.objExprAndOnly
    modifyState (over legend (Map.insert key value))
  return ()

-- DELETE ME when done
parseRest :: PotatoParser ()
parseRest = do
  h <- parseHeaderAny
  modifyState (over headers (h:))
  commands <- manyTillHeaderOrEoF parseLine
  void eof <|> parseSections

parseSections :: PotatoParser ()
parseSections = do
  void . many $
    try parseObjects <|>
    try parseLegend <|> parseRest


testParse :: PotatoParser ()
testParse = do
  endBy (T.pack <$> many (noneOf "\r\n")) endOfLine
  eof

-- split file into lines first
potatoParse :: PotatoParser Output
potatoParse = do
  titles <- parseTitles
  putState $ Output {
      _title = titles Map.! "title",
      _author = titles Map.! "author",
      _homepage = titles Map.! "homepage",
      _headers = [],
      _objectList = Map.empty,
      _legend = Map.empty
    }
  parseSections
  getState
