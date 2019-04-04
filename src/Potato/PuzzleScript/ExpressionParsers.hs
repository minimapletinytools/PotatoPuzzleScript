{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Potato.PuzzleScript.ExpressionParsers (
  parse_Object,
  parse_SingleObject,
  parse_ObjectExpr,
  parse_LegendExpr,
  parse_WinCondExpr,
  parse_Rule

) where

import Potato.PuzzleScript.Types
import qualified Potato.PuzzleScript.Token as PT
import Potato.PuzzleScript.ParserOutput

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.Expr

guardError :: Bool -> String -> PotatoParser ()
guardError b msg = if b then return () else fail msg

maybeParens :: PotatoParser a -> PotatoParser a
maybeParens p = PT.parens p <|> p

opTable_Boolean :: [[Operator T.Text Output Identity Boolean]]
opTable_Boolean =
  [[Prefix (PT.reservedOp "not" >> return (Boolean_Not))],
  [Infix (PT.reservedOp "and" >> return (Boolean_Bin And)) AssocLeft],
  [Infix (PT.reservedOp "or" >> return (Boolean_Bin Or)) AssocLeft]]

parse_Boolean_Input :: PotatoParser Boolean
parse_Boolean_Input = do
  enum <- choice $ map (\x -> PT.reserved (show x) >> return (show x)) allKeyboardInputs
  return $ Boolean_Input (read enum)

parse_Boolean_Term :: PotatoParser Boolean
parse_Boolean_Term =
  PT.parens parse_Boolean <|>
  parse_Boolean_Input <|>
  (PT.reserved "True" >> return Boolean_True) <|>
  (PT.reserved "False" >> return Boolean_False) <?>
  "valid Boolean expression"

parse_Boolean :: PotatoParser Boolean
parse_Boolean = buildExpressionParser opTable_Boolean parse_Boolean_Term <?> "Boolean"

parse_Command :: PotatoParser Command
parse_Command = do
  name <- string "TODOREPLACEMEWITHAREALCOMMAND"
  -- TODO
  --guardError (Map.member name om) ("unknown object " ++ name)
  return name

parse_Object :: PotatoParser Object
parse_Object = do
  om <- getState >>= return . _objectList
  name <- PT.identifier
  guardError (Map.member name om) ("unknown object " ++ name)
  return name

parse_SpaceModifier :: PotatoParser SpaceModifier
parse_SpaceModifier = try (PT.symbol "Abs" >> return Abs) <|> try (PT.symbol "Rel" >> return Rel) <|> return Default

parse_Orientation :: PotatoParser Orientation
parse_Orientation = choice (map (\x -> do { PT.reserved x; return x}) (Map.keys knownOrientations))

parse_ROrientation :: PotatoParser ROrientation
parse_ROrientation = do
  absorrel <- parse_SpaceModifier
  name <- maybeParens parse_Orientation
  return $ SpaceModifiedString absorrel name

parse_Velocity :: PotatoParser Velocity
parse_Velocity = do
  let vm = knownVelocities
  name <- PT.identifier <|> PT.operator
  guardError (Map.member name vm) ("unknown velocity " ++ name)
  return name

parse_RVelocity :: PotatoParser RVelocity
parse_RVelocity = do
  let vm = knownVelocities
  absorrel <- parse_SpaceModifier
  let parseVel = PT.identifier <|> PT.operator
  name <- maybeParens parseVel
  guardError (Map.member name vm) ("unknown velocity " ++ name)
  return $ SpaceModifiedString absorrel name


parse_SingleObject_Orientation :: PotatoParser SingleObject
parse_SingleObject_Orientation = do
  orient <- parse_ROrientation
  obj <- parse_Object
  return $ SingleObject_Orientation orient obj

parse_SingleObject :: PotatoParser SingleObject
parse_SingleObject = (try (parse_Object >>= return . SingleObject) <|> parse_SingleObject_Orientation)


opTable_ObjectExpr :: [[Operator T.Text Output Identity ObjectExpr]]
opTable_ObjectExpr =
  [[Infix (PT.reservedOp "and" >> return (ObjectExpr_Bin And_Obj)) AssocLeft],
  [Infix (PT.reservedOp "or" >> return (ObjectExpr_Bin Or_Obj)) AssocLeft]]

parse_ObjectExpr_Term :: PotatoParser ObjectExpr
parse_ObjectExpr_Term = PT.parens parse_ObjectExpr <|> (parse_SingleObject >>= return . ObjectExpr_Single)

parse_ObjectExpr :: PotatoParser ObjectExpr
parse_ObjectExpr = buildExpressionParser opTable_ObjectExpr parse_ObjectExpr_Term <?> "ObjectExpr"



parse_LegendExpr :: PotatoParser LegendExpr
parse_LegendExpr = do
  (key:[]) <- try PT.identifier <|> try PT.operator <?> "unreserved char"
  PT.reservedOp "="
  value <- parse_ObjectExpr
  return $ LegendExpr key value

parse_WinUnOp :: PotatoParser WinUnOp
parse_WinUnOp =
  try (PT.reservedOp "All" >> return Win_All) <|>
  try (PT.reservedOp "Some" >> return Win_Some) <|>
  try (PT.reservedOp "No" >> return Win_No) <?>
  "valid win condition unary operator"

parse_WinBinOp :: PotatoParser WinBinOp
parse_WinBinOp = PT.reservedOp "on" >> return Win_On

parse_BasicWinCondExpr :: PotatoParser BasicWinCondExpr
parse_BasicWinCondExpr = do
  op <- parse_WinUnOp
  obj <- parse_SingleObject
  return $ BasicWinCondExpr op obj

parse_WinCondExprBinOp :: PotatoParser WinCondExpr
parse_WinCondExprBinOp = do
  exp1 <- parse_BasicWinCondExpr
  op <- parse_WinBinOp
  exp2 <- parse_SingleObject
  return $ WinCondExpr_Bin op exp1 exp2

parse_WinCondExpr :: PotatoParser WinCondExpr
parse_WinCondExpr = try parse_WinCondExprBinOp <|> (parse_BasicWinCondExpr >>= return . WinCondExpr_Basic)

parse_PatBinOp :: PotatoParser PatBinOp
parse_PatBinOp = do PT.reservedOp "|" >> return Pipe

parse_PatternObject_Velocity :: PotatoParser PatternObj
parse_PatternObject_Velocity = do
  v <- parse_RVelocity
  obj <- parse_SingleObject
  return $ PatternObject_Velocity v obj

parse_PatternObj :: PotatoParser PatternObj
parse_PatternObj =
  try parse_PatternObject_Velocity <|>
  try (parse_ObjectExpr >>= return . PatternObject) <?>
  "PatternObj"

parse_Pattern :: PotatoParser Pattern
parse_Pattern = PT.brackets $ do
  first <- parse_PatternObj
  rest <- many $ do
    op <- parse_PatBinOp
    p <- parse_PatternObj
    return (op, p)
  return $ foldr (\(op, p) acc -> (\p' -> Pattern_Bin op p' (acc p))) Pattern_PatternObj rest $ first

parse_Patterns :: PotatoParser Patterns
parse_Patterns = sepBy parse_Pattern (many $ oneOf " \t") >>= return . Patterns

parse_RuleArrow :: PotatoParser ()
parse_RuleArrow = PT.reservedOp "->"

-- TODO validate same num args
-- TODO validate elipses are in the same position
validate_PatternPair :: Pattern -> Pattern -> Maybe String
validate_PatternPair lhs rhs = Nothing

-- | validate_UnscopedRule_Patterns checks if an UnscopedRule is valid
-- returns Nothing if rule is valid
validate_UnscopedRule_Patterns :: UnscopedRule -> Maybe String
validate_UnscopedRule_Patterns (UnscopedRule_Patterns (Patterns []) (Patterns[])) = Nothing
validate_UnscopedRule_Patterns (UnscopedRule_Patterns _ (Patterns[])) = Just "Pattern count mismatch"
validate_UnscopedRule_Patterns (UnscopedRule_Patterns (Patterns[]) _) = Just "Pattern count mismatch"
validate_UnscopedRule_Patterns (UnscopedRule_Patterns (Patterns (x:xs)) (Patterns (y:ys))) = case validate_PatternPair x y of
  Nothing -> validate_UnscopedRule_Patterns (UnscopedRule_Patterns (Patterns xs) (Patterns ys))
  just -> just
validate_UnscopedRule_Patterns _ = Just "Not a pattern match rule"

parse_UnscopedRule_Patterns :: PotatoParser UnscopedRule
parse_UnscopedRule_Patterns = do
  p1 <- parse_Patterns
  parse_RuleArrow
  p2 <- parse_Patterns
  return $ UnscopedRule_Patterns p1 p2

parse_UnscopedRule_Rule :: PotatoParser UnscopedRule
parse_UnscopedRule_Rule = do
  p <- parse_Patterns
  parse_RuleArrow
  r <- parse_Rule
  return $ UnscopedRule_Rule p r


parse_UnscopedRule_Boolean :: PotatoParser UnscopedRule
parse_UnscopedRule_Boolean = do
  p <- parse_Boolean
  parse_RuleArrow
  r <- parse_Rule
  return $ UnscopedRule_Boolean p r


parse_UnscopedRule :: PotatoParser UnscopedRule
parse_UnscopedRule =
  try parse_UnscopedRule_Boolean <|>
  try parse_UnscopedRule_Rule <|>
  try parse_UnscopedRule_Patterns <?>
  "UnscopedRule"

parse_Rule_Scoped :: PotatoParser Rule
parse_Rule_Scoped = do
  v <- parse_Velocity
  r <- parse_UnscopedRule
  return $ Rule_Scoped v r

-- do we support multi nested loops?
--parse_Rule_Looped :: LookupMaps -> PotatoParser Rule
--parse_Rule_Looped lm = do
--  rules <- between (PT.reserved "startLoop") (PT.reserved "endLoop") parse_Rule

parse_Rule :: PotatoParser Rule
parse_Rule =
  try parse_Rule_Scoped <|>
  try (parse_UnscopedRule >>= return . Rule) <|>
  try (parse_Command >>= return . Rule_Command) <?>
  "Rule"
