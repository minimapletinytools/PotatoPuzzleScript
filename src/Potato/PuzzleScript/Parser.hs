{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Potato.PuzzleScript.Parser(
  testParse,
  parseHeaderAny,
  potatoParse,

  -- TODO make internal, exposed for testing
  parseObjects,
  parseLegend,
  parseCollisionLayers

) where

import Potato.PuzzleScript.Types
import Potato.PuzzleScript.ParserOutput
import Potato.PuzzleScript.ExpressionParsers
import qualified Potato.PuzzleScript.Token as PT
import Control.Monad
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as U
import Text.Parsec

import Lens.Micro.Platform

potatoEndOfLine :: PotatoParser ()
potatoEndOfLine = void $ many $ do
    endOfLine
    many (try (many (oneOf " \t") >> endOfLine))
    return ()

parseLine :: PotatoParser String
parseLine = do
  r <- many (noneOf "\r\n")
  potatoEndOfLine
  return r

parseHeaderAny :: PotatoParser Header
parseHeaderAny = do
  many1 (char '=') >> endOfLine
  headerString <- (Prelude.foldl1 (<|>) $ map (try . string) (map show allHeaders)) <?> "valid header"
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
  manyTillHeaderOrEoF $ do
    LegendExpr key value <- parse_LegendExpr
    modifyState (over legend (Map.insert key value))
  return ()

-- TODO
parseSounds :: PotatoParser ()
parseSounds = void $ manyTillHeaderOrEoF parseLine

parseCollisionLayers :: PotatoParser ()
parseCollisionLayers = do
  manyTillHeaderOrEoF $ do
    -- parse each layer as comma separated objects
    -- TODO switch to SingleObject
    r <- PT.commaSep parse_Object
    modifyState (over collisionLayers (r:))
    PT.whiteSpace
  return ()


--parseObjectTerm :: PotatoParser

parseRules :: PotatoParser ()
parseRules = do
  r <- manyTillHeaderOrEoF $ parse_Rule
  --manyTillHeaderOrEoF parseLine
  modifyState (set rules r)
  return ()

parseWinCondExpritions :: PotatoParser ()
parseWinCondExpritions = do
  PT.whiteSpace
  manyTillHeaderOrEoF $ do
    -- parse each layer as comma separated objects
    r <- parse_WinCondExpr
    modifyState (over winConditions (r:))
    PT.whiteSpace
  return ()



tryFirstThen :: Stream s m t => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m b
tryFirstThen a b = try (a >> b) <|> b

parseLevelSlice :: PotatoParser ((Int,Int), LevelSlice)
parseLevelSlice = do
  state <- getState
  let
    legend = _legend state
    parseLevelLine = do
      r <- many1 (oneOf (Map.keys legend))
      many (oneOf " \t") >> endOfLine
      return r
  slices <- manyTill parseLevelLine (try (void endOfLine) <|> lookAhead eof) >>= return . reverse
  PT.whiteSpace
  if length slices > 0 then return () else fail "level must only use symbols defined in legend"
  let x = length (head slices)
  if all (\x' -> length x' == x) slices then return () else fail "all lines in level must have same length"
  return ((x, length slices), U.fromList (concat slices))

setDepth :: Int -> (Int, Int) -> Size
setDepth z (x,y) = (x,y,z)

parseLevel :: PotatoParser Level
parseLevel = do
  name <- tryParseLevelMessage
  slice <- parseLevelSlice
  return $ Level  (setDepth 1 (fst slice)) [snd slice] name

-- we don't use token here because we want don't want it to skip eol in case there is nothing after MESSAGE
tryParseLevelMessage :: PotatoParser String
tryParseLevelMessage = (string "MESSAGE" >> parseLine) <|> return ""

parseMultiLevel :: PotatoParser Level
parseMultiLevel = do
  name <- tryParseLevelMessage
  PT.symbol "DEPTH"
  x <- PT.natural >>= return . fromIntegral
  if x > 0 then return () else fail "expected >0 DEPTH"
  slices <- forM [1..x] (const parseLevelSlice)
  -- TODO check that all sizes are the same, why is this so annoying
  return $ Level (setDepth x (fst (slices !! 0))) (map snd slices) name



parseLevels :: PotatoParser ()
parseLevels = do
  PT.whiteSpace
  manyTillHeaderOrEoF $ do
    level <- parseMultiLevel <|> parseLevel <?> "valid level definition"
    modifyState (over levels (level:))
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
  -- TODO add error if parsing unrecognized header
  void . many $ parseSection_ [
    (parseHeader OBJECTS, parseObjects)
    , (parseHeader LEGEND, parseLegend)
    , (parseHeader SOUNDS, parseSounds)
    , (parseHeader COLLISIONLAYERS, parseCollisionLayers)
    , (parseHeader RULES, parseRules)
    , (parseHeader WINCONDITIONS, parseWinCondExpritions)
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
  putState $ emptyOutput {
      _title = titles Map.! "title",
      _author = titles Map.! "author",
      _homepage = titles Map.! "homepage"
    }
  parseSections
  getState
