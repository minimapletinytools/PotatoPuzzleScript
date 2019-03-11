{-# LANGUAGE TemplateHaskell #-}

module Potato.PuzzleScript.Types (
  Header(..),
  headerStrings,

  Size,
  LevelSlice,
  Level(..),

  Object,
  Orientation,
  Velocity,
  Command,
  Color,
  LegendKey,
  ObjectMap, VelocityMap, OrientationMap, LegendMap,

  orientations,
  knownVelocities,

  BooleanBinOp(..),
  Boolean(..),

  ObjBinOp(..),
  SingleObject(..),
  ObjectExpr(..),
  LegendExpr(..),

  WinUnOp(..),
  WinBinOp(..),
  BasicWinCond(..),
  WinCond(..),

  PatBinOp(..),
  PatternObj(..),
  Pattern,

  RuleBinOp(..),
  Command,
  UnscopedRule(..),
  Rule(..),


  --isObjBinOn,
  --isSingleObject,
  --isPatternObject,
  --isWinCondition
) where

import Potato.Math.Integral.TR

import qualified Data.Map as Map
import Text.Parsec
import qualified Data.Vector.Unboxed as U

import Lens.Micro.Platform

data Header = OBJECTS | LEGEND | SOUNDS | COLLISIONLAYERS | RULES | WINCONDITIONS | LEVELS deriving (Read, Show)

headerStrings :: [String]
headerStrings = ["OBJECTS", "LEGEND", "SOUNDS", "COLLISIONLAYERS", "RULES", "WINCONDITIONS", "LEVELS"]

type Object = String
-- TODO make Object var for "..."
type Orientation = String
type Velocity = String
type Command = String
type Color = String

type VelocityMap = Map.Map Velocity TR
type OrientationMap = Map.Map Orientation Rotation

-- TODO finish
orientations :: OrientationMap
orientations = Map.fromList [("R_UP", undefined)]
type ObjectMap = Map.Map Object Color
type LegendKey = Char
type LegendMap = Map.Map LegendKey ObjectExpr


type Size = (Int, Int)
type LevelSlice = U.Vector Char
data Level = Level Size [LevelSlice] String deriving(Show)


knownVelocities :: VelocityMap
knownVelocities = Map.fromList []




data BooleanBinOp = And | Or deriving(Show)
data Boolean = Boolean_True | Boolean_False | Boolean_Not Boolean | Boolean_Bin BooleanBinOp Boolean deriving(Show)

data ObjBinOp = And_Obj | Or_Obj deriving(Show)
data SingleObject = SingleObject Object | SingleObject_Orientation Orientation Object deriving(Show)
data ObjectExpr = ObjectExpr_Single SingleObject| ObjectExpr_Bin ObjBinOp ObjectExpr ObjectExpr deriving(Show)

data LegendExpr = LegendExpr Char ObjectExpr

data WinUnOp = Win_All | Win_Some | Win_No deriving(Show)
data WinBinOp = Win_On deriving(Show)
data BasicWinCond = BasicWinCond WinUnOp SingleObject deriving(Show)
-- TODO rename to WinCondExpr
data WinCond = WinCond_Basic BasicWinCond | WinCond_Bin WinBinOp BasicWinCond SingleObject deriving(Show)

data PatBinOp = Pipe deriving(Show)
data PatternObj = PatternObject ObjectExpr | PatternObject_Velocity Velocity SingleObject deriving(Show)
type Pattern = [PatternObj]

data RuleBinOp = Arrow deriving(Show)
data UnscopedRule = UnscopedRule_Pattern Pattern Pattern | UnscopedRule_Rule Pattern Rule | UnscopedRuleBoolean Boolean Rule deriving(Show)
data Rule = Rule_Command Command | Rule UnscopedRule | Rule_Scoped Velocity UnscopedRule deriving(Show)




{-

data UnOp = Not
  | All | No | Some  -- win cond operators
  deriving (Show, Eq)
data BinOp =
  And | Or -- boolean operators
  | Arrow -- rule operators
  | On -- win cond operators
  | Pipe -- pattern operators (assoc right)
  deriving (Show, Eq)

data Expr =
  -- whatevers
  TrueExpr | FalseExpr
  | IntExpr Int
  -- object
  | ObjectExpr Object | OrientObjExpr Orientation Expr | MovingObjExpr Velocity Expr
  -- command
  | CommandExpr Command
  -- rules
  | ScopedRuleExpr Velocity Expr


  | UnExpr UnOp Expr
  | BinExpr BinOp Expr Expr deriving (Show)



-- | isObjBinOp returns true if the binary operator is a valid object operator
isObjBinOn :: BinOp -> Bool
isObjBinOn And = True
isObjBinOn Or = True
isObjBinOn _ = False

-- | isBooleanBinOn returns true if the binary operator is a valid Boolean operator
isBooleanBinOp :: BinOp -> Bool
isBooleanBinOp = isObjBinOn -- happens to be the same

-- |
isPatternBinOp :: BinOp -> Bool
isPatternBinOp x = x == Pipe

-- | isSingleObjet returns true if the expression is a valid SingleObject
-- SingleObject = Object | Orientation Object
isSingleObject :: Expr -> Bool
isSingleObject (ObjectExpr _) = True
isSingleObject (OrientObjExpr _ (ObjectExpr _)) = True
isSingleObject _ = False

-- | isBasicObject returns true if the expression is a valid basic object (no modifiers)
--isBasicObject :: Expr -> Bool
--isBasicObject (BinExpr x a b) = isObjBinOn x && isBasicObject a && isBasicObject b
--isBasicObject x = isSingleObject x

-- | isPatternObject returns true if the expression is a valid object in a PatternObject
-- PatternObject = SingleObject | Velocity SingleObject
isPatternObject :: Expr -> Bool
isPatternObject (MovingObjExpr _ x) = isSingleObject x
isPatternObject x = isSingleObject x

-- |
-- Pattern = PatternObject | PatternObject pipe Pattern
isPattern :: Expr -> Bool
isPattern (BinExpr x a b) = isPatternBinOp x && isPattern a && isPattern b -- assoc right not required this way
isPattern x = isPatternObject x

-- | isBoolean returns true if the expression is a valid Boolean
-- Boolean = TrueExpr | FalseExpr | Boolean
-- TODO algebraic operators
isBoolean :: Expr -> Bool
isBoolean TrueExpr = True
isBoolean FalseExpr = True
isBoolean (BinExpr x a b) = isBooleanBinOp x && isBoolean a && isBoolean b
isBoolean _ = False

-- | isRule returns true if the expression is a valid rule expression
-- Rule = Command | Velocity Pattern -> Pattern | Velocity Pattern -> Rule | Boolean -> rule
isRule :: Expr -> Bool
isRule (CommandExpr _) = True
isRule (ScopedRuleExpr _ (BinExpr Arrow a b)) = isPattern a && (isRule b || isPattern b)
isRule (BinExpr Arrow a b) = isBoolean a && isRule b


-- | isBasicWinCondition returns true if the expression is a valid basic win condition expression
-- BasicWinCondition = All/No/Some SingleObject
isBasicWinCondition :: Expr -> Bool
isBasicWinCondition (UnExpr All _) = True
isBasicWinCondition (UnExpr No _) = True
isBasicWinCondition (UnExpr Some _) = True
isBasicWinCondition _ = False

-- | isWinCondition returns true if the expression is a valid win condition expression
-- WinCondition = BasicWinCondition | BasicWinCondition On SingleObject
-- a valid win condition expressions are limited see https://www.puzzlescript.net/Documentation/winconditions.html
-- for complex win conditions, use rules instead
isWinCondition :: Expr -> Bool
isWinCondition (BinExpr On x (ObjectExpr _)) = isBasicWinCondition x
isWinCondition x = isBasicWinCondition x

-}
