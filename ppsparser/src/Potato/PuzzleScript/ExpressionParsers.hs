{-# LANGUAGE TemplateHaskell #-}

module Potato.PuzzleScript.ExpressionParsers (
  parse_Object,
  parse_SingleObject,
  parse_ObjectExpr,
  parse_LegendExpr,
  parse_WinCond,
  parse_Rule


) where

import Potato.Math.Integral.TR
import Potato.PuzzleScript.Types
import qualified Potato.PuzzleScript.Token as PT
import Potato.PuzzleScript.ParserOutput

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.Expr

type LookupMaps = (ObjectMap, VelocityMap)

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

-- TODO consider allowing patterns here?
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
  -- TODO don't fail
  fail "no command support yet"
  -- TODO reserved
  name <- PT.identifier
  -- TODO check command is valid
  --guardError (Map.member name om) ("unknown object " ++ name)
  return name

parse_Object :: ObjectMap -> PotatoParser Object
parse_Object om = do
  name <- PT.identifier
  guardError (Map.member name om) ("unknown object " ++ name)
  return name

parse_AbsOrRel :: PotatoParser (a -> AbsOrRel a)
parse_AbsOrRel = try (PT.symbol "Abs" >> return Abs) <|> try (PT.symbol "Rel" >> return Rel) <|> return Abs

parse_Orientation :: PotatoParser Orientation
parse_Orientation = choice (map (\x -> do { PT.reserved x; return x}) (Map.keys orientations))

parse_ROrientation :: PotatoParser ROrientation
parse_ROrientation = do
  absorrel <- parse_AbsOrRel
  let parseOrientation = choice (map (\x -> do { PT.reserved x; return x}) (Map.keys orientations))
  name <- maybeParens parseOrientation
  return $ absorrel name

parse_Velocity :: VelocityMap -> PotatoParser Velocity
parse_Velocity vm = do
  name <- PT.identifier <|> PT.operator
  guardError (Map.member name vm) ("unknown velocity " ++ name)
  return name

parse_RVelocity :: VelocityMap -> PotatoParser RVelocity
parse_RVelocity vm = do
  absorrel <- parse_AbsOrRel
  let parseVel = PT.identifier <|> PT.operator
  name <- maybeParens parseVel
  guardError (Map.member name vm) ("unknown velocity " ++ name)
  return $ absorrel name


parse_SingleObject_Orientation :: ObjectMap -> PotatoParser SingleObject
parse_SingleObject_Orientation om = do
  orient <- parse_ROrientation
  obj <- parse_Object om
  return $ SingleObject_Orientation orient obj

parse_SingleObject :: ObjectMap -> PotatoParser SingleObject
parse_SingleObject om = try (parse_Object om >>= return . SingleObject) <|> parse_SingleObject_Orientation om


opTable_ObjectExpr :: [[Operator T.Text Output Identity ObjectExpr]]
opTable_ObjectExpr =
  [[Infix (PT.reservedOp "and" >> return (ObjectExpr_Bin And_Obj)) AssocLeft],
  [Infix (PT.reservedOp "or" >> return (ObjectExpr_Bin Or_Obj)) AssocLeft]]

parse_ObjectExpr_Term :: ObjectMap -> PotatoParser ObjectExpr
parse_ObjectExpr_Term om = PT.parens (parse_ObjectExpr om) <|> (parse_SingleObject om >>= return . ObjectExpr_Single)

parse_ObjectExpr :: ObjectMap -> PotatoParser ObjectExpr
parse_ObjectExpr om = buildExpressionParser opTable_ObjectExpr (parse_ObjectExpr_Term om) <?> "ObjectExpr"



parse_LegendExpr :: ObjectMap -> PotatoParser LegendExpr
parse_LegendExpr om = do
  (key:[]) <- try PT.identifier <|> try PT.operator <?> "unreserved char"
  PT.reservedOp "="
  value <- parse_ObjectExpr om
  return $ LegendExpr key value

parse_WinUnOp :: PotatoParser WinUnOp
parse_WinUnOp =
  try (PT.reservedOp "All" >> return Win_All) <|>
  try (PT.reservedOp "Some" >> return Win_Some) <|>
  try (PT.reservedOp "No" >> return Win_No) <?>
  "valid win condition unary operator"

parse_WinBinOp :: PotatoParser WinBinOp
parse_WinBinOp = PT.reservedOp "on" >> return Win_On

parse_BasicWinCond :: ObjectMap -> PotatoParser BasicWinCond
parse_BasicWinCond om = do
  op <- parse_WinUnOp
  obj <- parse_SingleObject om
  return $ BasicWinCond op obj

parse_WinCondBinOp :: ObjectMap -> PotatoParser WinCond
parse_WinCondBinOp om = do
  exp1 <- parse_BasicWinCond om
  op <- parse_WinBinOp
  exp2 <- parse_SingleObject om
  return $ WinCond_Bin op exp1 exp2

parse_WinCond :: ObjectMap -> PotatoParser WinCond
parse_WinCond om = try (parse_WinCondBinOp om) <|> (parse_BasicWinCond om >>= return . WinCond_Basic)

parse_PatBinOp :: PotatoParser PatBinOp
parse_PatBinOp = do PT.reservedOp "|" >> return Pipe

parse_PatternObject_Velocity :: LookupMaps -> PotatoParser PatternObj
parse_PatternObject_Velocity (om,vm) = do
  v <- parse_RVelocity vm
  obj <- parse_SingleObject om
  return $ PatternObject_Velocity v obj

parse_PatternObj :: LookupMaps -> PotatoParser PatternObj
parse_PatternObj lm@(om,_) =
  try (parse_PatternObject_Velocity lm) <|>
  try (parse_ObjectExpr om >>= return . PatternObject) <?>
  "PatternObj"


parse_Pattern :: LookupMaps -> PotatoParser Pattern
parse_Pattern lm = PT.brackets $ do
  first <- (parse_PatternObj lm)
  rest <- many $ do
    op <- parse_PatBinOp
    p <- (parse_PatternObj lm)
    return (op, p)
  return $ foldr (\(op, p) acc -> (\p' -> Pattern_Bin op p' (acc p))) Pattern_PatternObj rest $ first

parse_Patterns :: LookupMaps -> PotatoParser Patterns
parse_Patterns lm = many (parse_Pattern lm) >>= return . Patterns

parse_RuleBinOp :: PotatoParser RuleBinOp
parse_RuleBinOp = do PT.reservedOp "->" >> return Arrow

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

parse_UnscopedRule_Patterns :: LookupMaps -> PotatoParser UnscopedRule
parse_UnscopedRule_Patterns lm = do
  p1 <- parse_Patterns lm
  parse_RuleBinOp
  p2 <- parse_Patterns lm
  return $ UnscopedRule_Patterns p1 p2

parse_UnscopedRule_Rule :: LookupMaps -> PotatoParser UnscopedRule
parse_UnscopedRule_Rule lm = do
  p <- parse_Patterns lm
  parse_RuleBinOp
  r <- parse_Rule lm
  return $ UnscopedRule_Rule p r


parse_UnscopedRule_Boolean :: LookupMaps -> PotatoParser UnscopedRule
parse_UnscopedRule_Boolean lm = do
  p <- parse_Boolean
  parse_RuleBinOp
  r <- parse_Rule lm
  return $ UnscopedRule_Boolean p r


parse_UnscopedRule :: LookupMaps -> PotatoParser UnscopedRule
parse_UnscopedRule lm =
  try (parse_UnscopedRule_Boolean lm) <|>
  try (parse_UnscopedRule_Rule lm) <|>
  try (parse_UnscopedRule_Patterns lm) <?>
  "UnscopedRule"

parse_Rule_Scoped :: LookupMaps -> PotatoParser Rule
parse_Rule_Scoped lm@(_,vm) = do
  v <- parse_Velocity vm
  r <- parse_UnscopedRule lm
  return $ Rule_Scoped v r

-- do we support multi nested loops?
--parse_Rule_Looped :: LookupMaps -> PotatoParser Rule
--parse_Rule_Looped lm = do
--  rules <- between (PT.reserved "startLoop") (PT.reserved "endLoop") parse_Rule

parse_Rule :: LookupMaps -> PotatoParser Rule
parse_Rule lm =
  try (parse_Rule_Scoped lm) <|>
  try (parse_UnscopedRule lm >>= return . Rule) <|>
  try (parse_Command >>= return . Rule_Command) <?>
  "Rule"
