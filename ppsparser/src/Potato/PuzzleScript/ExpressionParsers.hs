{-# LANGUAGE TemplateHaskell #-}

module Potato.PuzzleScript.ExpressionParsers (
  parse_Object,
  parse_SingleObject,


  parse_ObjectExpr,

  parse_LegendExpr,

  parse_WinCond


) where

import Potato.Math.Integral.TR
import Potato.PuzzleScript.Types
import qualified Potato.PuzzleScript.Token as PT
import Potato.PuzzleScript.ParserOutput

import qualified Data.Map as Map
import Text.Parsec

type LookupMaps = (ObjectMap, VelocityMap)

guardError :: Bool -> String -> PotatoParser ()
guardError b msg = if b then return () else fail msg

parse_Object :: ObjectMap -> PotatoParser Object
parse_Object om = do
  name <- PT.identifier
  guardError (Map.member name om) ("unknown object " ++ name)
  return name

parse_ObjBinOp :: PotatoParser ObjBinOp
parse_ObjBinOp = (PT.reservedOp "and" >> return And_Obj) <|> (PT.reservedOp "or" >> return Or_Obj)

parse_Orientation :: PotatoParser Orientation
parse_Orientation = choice (map (\x -> do { PT.reserved x; return x}) (Map.keys orientations))

parse_Velocity :: VelocityMap -> PotatoParser Velocity
parse_Velocity vm = do
  name <- PT.identifier
  guardError (Map.member name vm) ("unknown velocity " ++ name)
  return name

parse_SingleObject_Orientation :: ObjectMap -> PotatoParser SingleObject
parse_SingleObject_Orientation om = do
  orient <- parse_Orientation
  obj <- parse_Object om
  return $ SingleObject_Orientation orient obj

parse_SingleObject :: ObjectMap -> PotatoParser SingleObject
parse_SingleObject om = try (parse_Object om >>= return . SingleObject) <|> parse_SingleObject_Orientation om


parse_ObjectExpr_Bin :: ObjectMap -> PotatoParser ObjectExpr
parse_ObjectExpr_Bin om = do
  exp1 <- PT.parens (parse_ObjectExpr om)
  op <- parse_ObjBinOp
  exp2 <- PT.parens (parse_ObjectExpr om)
  return $ ObjectExpr_Bin op exp1 exp2


parse_ObjectExpr :: ObjectMap -> PotatoParser ObjectExpr
parse_ObjectExpr om = try (parse_SingleObject om >>= return . ObjectExpr_Single) <|> parse_ObjectExpr_Bin om

parse_LegendExpr :: ObjectMap -> PotatoParser LegendExpr
parse_LegendExpr om = do
  (key:[]) <- try PT.identifier <|> try PT.operator <?> "unreserved char"
  PT.reservedOp "="
  value <- parse_ObjectExpr om
  return $ LegendExpr key value

parse_WinUnOp :: PotatoParser WinUnOp
parse_WinUnOp =
  (PT.reservedOp "All" >> return Win_All)
  <|> (PT.reservedOp "Some" >> return Win_Some)
  <|> (PT.reservedOp "No" >> return Win_No)

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
