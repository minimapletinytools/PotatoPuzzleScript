{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module PPSParser(
  testParse,
  parseHeaderAny,
  potatoParse,

  -- TODO make internal, exposed for testing
  parseObjects,
  parseLegend,
  parseCollisionLayers

) where

import PPSTypes
import qualified PPSToken as PT
import Control.Monad
import qualified Data.Text as T
import qualified Data.Map as Map
import Text.Parsec

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
    try (string hs) <?> hs
    endOfLine
    many1 (char '=') >> endOfLine
    -- add it to the headers we saw
    modifyState (over headers (h:))
    PT.whiteSpace
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
  manyTillHeaderOrEoF $ do
    ident <- PT.identifier
    color <- PT.identifier
    modifyState (over objectList (Map.insert ident color))
  return ()

parseLegend :: PotatoParser ()
parseLegend = do
  objects <- getState >>= return . _objectList
  manyTillHeaderOrEoF $ do
    (key:[]) <- PT.identifier <|> PT.operator <?> "unreserved character"
    -- TODO fail on duplicate keys
    PT.reservedOp "="
    value <- PT.objExprFromObjectMap objects <?> "recognized object"
    modifyState (over legend (Map.insert key value))
  return ()

-- TODO
parseSounds :: PotatoParser ()
parseSounds = void $ manyTillHeaderOrEoF parseLine

parseCollisionLayers :: PotatoParser ()
parseCollisionLayers = do
  objects <- getState >>= return . _objectList
  manyTillHeaderOrEoF $ do
    r <- PT.commaSep (PT.objConstFromObjectMap objects)
    modifyState (over collisionLayers (r:))
    PT.whiteSpace
  return ()


--parseObjectTerm :: PotatoParser

parseRules :: PotatoParser ()
parseRules = do
  objects <- getState >>= return . _objectList
  manyTillHeaderOrEoF parseLine
    -- rule
      -- pattern match
      -- command
      -- condition -> rule ?
      -- rule + rule (creates one execution group)
    -- pattern match
      -- (optional modifier) [term] -> [term]
    -- condition
      -- (optional modifier) [term] // does this conflict with pattern match style rules above?
      -- varCondition
    -- command
      -- cancel
      -- varModify
      -- win

    --modifyState (over legend (Map.insert key value))
  return ()

parseWinConditions :: PotatoParser ()
parseWinConditions = do
  objects <- getState >>= return . _objectList
  PT.whiteSpace
  manyTillHeaderOrEoF $ parseLine
    -- no/all/some [object] (optional [positional term] [object])
    -- positional term
      -- on (means +1 Z-axis)
      -- other ones??
    --modifyState (over legend (Map.insert key value))
  return ()

parseLevels :: PotatoParser ()
parseLevels = do
  state <- getState
  let
    objects = _objectList state
    legend = _legend state
  PT.whiteSpace
  manyTillHeaderOrEoF $ parseLine
  return ()

-- | parseSections_ takes a list of parser tuples
-- tries to execute first tuple of first element
-- if fail, go onto next tuple in list and repeat
-- if success execute and return second element
-- could probably make this more efficient using ParseT and unparser directly
parseSection_ :: [(PotatoParser a, PotatoParser ())] -> PotatoParser ()
parseSection_ [] = return ()
parseSection_ ((a,b):[]) = try a >> b
parseSection_ ((a,b):xs) = do
  -- use Maybe so that we can use <|> with different return types
  v <- (try a >>= return . Just) <|> (parseSection_ xs >> return Nothing)
  case v of
    Nothing -> return ()
    Just _ -> b


parseSections :: PotatoParser ()
parseSections = do
  -- because of many here, we can't get errors out
  -- TODO move repeat behavior into parseSection_ so you can get errors
  void . many $ parseSection_ [
    (parseHeader OBJECTS, parseObjects)
    , (parseHeader LEGEND, parseLegend)
    , (parseHeader SOUNDS, parseSounds)
    , (parseHeader COLLISIONLAYERS, parseCollisionLayers)
    , (parseHeader RULES, parseRules)
    , (parseHeader WINCONDITIONS, parseWinConditions)
    , (parseHeader LEVELS, parseLevels)
    ]
  PT.whiteSpace
  void eof


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
      _legend = Map.empty,
      _collisionLayers = []
    }
  parseSections
  getState
