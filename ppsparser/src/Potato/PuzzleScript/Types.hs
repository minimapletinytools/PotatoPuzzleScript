{-# LANGUAGE TemplateHaskell #-}

module Potato.PuzzleScript.Types (
  Header(..),
  allHeaders,

  Size,
  LevelSlice,
  Level(..),

  KeyboardInput(..),
  allKeyboardInputs,
  AbsOrRel(..),
  Object,
  Orientation,
  Velocity,
  ROrientation,
  RVelocity,
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

data Header = OBJECTS | LEGEND | SOUNDS | COLLISIONLAYERS | RULES | WINCONDITIONS | LEVELS deriving (Read, Show, Enum)
allHeaders :: [Header]
allHeaders = enumFrom OBJECTS

data AbsOrRel a = Abs a | Rel a deriving (Functor, Show)

type Object = String
-- TODO make Object var for "..."
type Orientation = String
type Velocity = String

type ROrientation = AbsOrRel Orientation
type RVelocity = AbsOrRel Velocity

type Command = String
type Color = String


-- TODO these two need to store AbsOrRel type info
-- one idea is to do Map.Map Orientation (AbsOrRel Rotation)
-- and ROrientation Abs/Rel overrides Rotation Abs/Rel when used
type OrientationMap = Map.Map Orientation Rotation
type VelocityMap = Map.Map Velocity TR
knownVelocities :: VelocityMap
knownVelocities = Map.fromList [("v", identity),("^", identity),(">", identity),("<", identity)]

-- TODO finish
orientations :: OrientationMap
orientations = Map.fromList [("R_UP", undefined)]
type ObjectMap = Map.Map Object Color
type LegendKey = Char
type LegendMap = Map.Map LegendKey ObjectExpr


type Size = (Int, Int, Int)
type LevelSlice = U.Vector Char
-- level is from x y z order min to max
data Level = Level Size [LevelSlice] String deriving(Show)

data KeyboardInput = K_NONE | K_LEFT | K_RIGHT | K_DOWN | K_UP | K_Z | K_X deriving(Show, Read, Enum)
allKeyboardInputs :: [KeyboardInput]
allKeyboardInputs = enumFrom K_LEFT

data BooleanBinOp = And | Or deriving(Show)
data Boolean = Boolean_Var String | Boolean_Input KeyboardInput | Boolean_True | Boolean_False | Boolean_Not Boolean | Boolean_Bin BooleanBinOp Boolean Boolean
instance Show Boolean where
  show (Boolean_Var s) = s
  show (Boolean_Input input) = show input
  show Boolean_True = "True"
  show Boolean_False = "False"
  show (Boolean_Not b) = "Not " ++ show b
  show (Boolean_Bin op b1 b2) = show b1 ++ " " ++ show op ++ " " ++ show b2

data ObjBinOp = And_Obj | Or_Obj deriving(Show)
data SingleObject = SingleObject Object | SingleObject_Orientation ROrientation Object
instance Show SingleObject where
  show (SingleObject obj) = obj
  show (SingleObject_Orientation orient obj) = show orient ++ " " ++ obj

data ObjectExpr = ObjectExpr_Single SingleObject| ObjectExpr_Bin ObjBinOp ObjectExpr ObjectExpr
instance Show ObjectExpr where
  show (ObjectExpr_Single obj) = show obj
  show (ObjectExpr_Bin op exp1 exp2) = show exp1 ++ " " ++ show op ++ " " ++ show exp2

data LegendExpr = LegendExpr Char ObjectExpr
instance Show LegendExpr where
  show (LegendExpr k v) = show k ++ " = " ++ show v

data WinUnOp = Win_All | Win_Some | Win_No
instance Show WinUnOp where
  show Win_All = "All"
  show Win_Some = "Some"
  show Win_No = "No"
data WinBinOp = Win_On

instance Show WinBinOp where
  show Win_On = "on"

data BasicWinCond = BasicWinCond WinUnOp SingleObject
instance Show BasicWinCond where
  show (BasicWinCond op obj) = show op ++ " " ++ show obj

-- TODO rename to WinCondExpr
data WinCond = WinCond_Basic BasicWinCond | WinCond_Bin WinBinOp BasicWinCond SingleObject
instance Show WinCond where
  show (WinCond_Basic bwc) = show bwc
  show (WinCond_Bin op bwc obj) = show bwc ++ " " ++ show op ++ " " ++ show obj


data PatBinOp = Pipe
instance Show PatBinOp where
  show Pipe = "|"
-- velocity restricted to single objects for now
data PatternObj = PatternObject ObjectExpr | PatternObject_Velocity RVelocity SingleObject
instance Show PatternObj where
  show (PatternObject expr) = show expr
  show (PatternObject_Velocity vel obj) = show vel ++ " " ++ show obj

-- TODO may want to add more separators, not just the | that inherits scope
type Pattern = [PatternObj]

-- TODO patterns
--type Patterns = [Pattern]

indentOnce :: String -> String
indentOnce = concat . map ("    " ++) . lines

data RuleBinOp = Arrow
instance Show RuleBinOp where
  show Arrow = "->"

data UnscopedRule = UnscopedRule_Pattern Pattern Pattern | UnscopedRule_Rule Pattern Rule | UnscopedRule_Boolean Boolean Rule
instance Show UnscopedRule where
  show (UnscopedRule_Pattern p1 p2) = show p1 ++ " -> " ++ show p2
  show (UnscopedRule_Rule p r) = show p ++ " ->\n" ++ indentOnce (show r)
  show (UnscopedRule_Boolean b r) = show b ++ " -> " ++ indentOnce (show r)

data Rule = Rule_Command Command | Rule UnscopedRule | Rule_Scoped Velocity UnscopedRule
instance Show Rule where
  show (Rule_Command c) = show c
  show (Rule r) = show r
  show (Rule_Scoped vel r) = show vel ++ " " ++ show r

-- TODO RuleGroup [Rule]




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
