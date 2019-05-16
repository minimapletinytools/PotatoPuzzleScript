{-# LANGUAGE TemplateHaskell #-}

module Potato.PuzzleScript.Types (
  module Potato.PuzzleScript.SpaceTypes,
  Header(..),
  allHeaders,

  Size,
  LevelSlice,
  Level(..),

  KeyboardInput(..),
  allKeyboardInputs,

  Object,

  Color,
  LegendKey,
  ObjectMap,
  LegendMap,
  BooleanBinOp(..),
  Boolean(..),

  ObjBinOp(..),
  SingleObject(..),
  ObjectExpr(..),
  LegendExpr(..),

  WinUnOp(..),
  WinBinOp(..),
  BasicWinCondExpr(..),
  WinCondExpr(..),

  PatBinOp(..),
  PatternObj(..),
  Pattern(..),
  Patterns(..),

  Command(..),

  UnscopedRule(..),
  Rule(..),
  RuleGroup(..)


  --isObjBinOn,
  --isSingleObject,
  --isPatternObject,
  --isWinCondExprition
) where

import Potato.PuzzleScript.SpaceTypes



import Data.List (intersperse)
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as U

data Header = OBJECTS | LEGEND | SOUNDS | COLLISIONLAYERS | RULES | LATE | WINCONDITIONS | LEVELS deriving (Read, Show, Enum)
allHeaders :: [Header]
allHeaders = enumFrom OBJECTS


type Object = String
-- TODO make Object var for "..."

type Color = String

type ObjectMap = Map.Map Object Color
type LegendKey = Char
type LegendMap = Map.Map LegendKey ObjectExpr

type Size = (Int, Int, Int)
type LevelSlice = U.Vector Char
-- level is from x y z order min to max
data Level = Level Size [LevelSlice] String deriving(Show)

data KeyboardInput = K_NONE | K_LEFT | K_RIGHT | K_DOWN | K_UP | K_ACTION | K_Z | K_X deriving(Show, Read, Eq, Enum)
allKeyboardInputs :: [KeyboardInput]
allKeyboardInputs = enumFrom K_LEFT

data BooleanBinOp = And | Or deriving(Show, Eq)
data Boolean = Boolean_Var String | Boolean_Input KeyboardInput | Boolean_True | Boolean_False | Boolean_Not Boolean | Boolean_Bin BooleanBinOp Boolean Boolean deriving(Eq)
instance Show Boolean where
  show (Boolean_Var s) = s
  show (Boolean_Input input) = show input
  show Boolean_True = "True"
  show Boolean_False = "False"
  show (Boolean_Not b) = "Not " ++ show b
  show (Boolean_Bin op b1 b2) = show b1 ++ " " ++ show op ++ " " ++ show b2

data ObjBinOp = And_Obj | Or_Obj deriving(Eq)
instance Show ObjBinOp where
  show And_Obj = "and"
  show Or_Obj = "or"

data SingleObject = SingleObject Object | SingleObject_Orientation Orientation Object deriving(Eq)
instance Show SingleObject where
  show (SingleObject obj) = obj
  show (SingleObject_Orientation orient obj) = orient ++ " " ++ obj
  --show (SingleObject_Orientation orient obj) = "(" ++ orient ++ " " ++ obj ++ ")"

data ObjectExpr = ObjectExpr_Single SingleObject| ObjectExpr_Bin ObjBinOp ObjectExpr ObjectExpr deriving(Eq)
instance Show ObjectExpr where
  show (ObjectExpr_Single obj) = show obj
  show (ObjectExpr_Bin op exp1 exp2) = "(" ++ show exp1 ++ " " ++ show op ++ " " ++ show exp2 ++ ")"

data LegendExpr = LegendExpr Char ObjectExpr deriving(Eq)
instance Show LegendExpr where
  show (LegendExpr k v) = show k ++ " = " ++ show v

data WinUnOp = Win_All | Win_Some | Win_No deriving(Eq)
instance Show WinUnOp where
  show Win_All = "All"
  show Win_Some = "Some"
  show Win_No = "No"

data WinBinOp = Win_On deriving(Eq)
instance Show WinBinOp where
  show Win_On = "on"

data BasicWinCondExpr = BasicWinCondExpr WinUnOp SingleObject deriving(Eq)
instance Show BasicWinCondExpr where
  show (BasicWinCondExpr op obj) = show op ++ " " ++ show obj

data WinCondExpr = WinCondExpr_Basic BasicWinCondExpr | WinCondExpr_Bin WinBinOp BasicWinCondExpr SingleObject deriving(Eq)
instance Show WinCondExpr where
  show (WinCondExpr_Basic bwc) = show bwc
  show (WinCondExpr_Bin op bwc obj) = show bwc ++ " " ++ show op ++ " " ++ show obj

data PatBinOp = Pipe deriving(Eq)
instance Show PatBinOp where
  show Pipe = "|"
-- velocity restricted to single objects for now
data PatternObj = PatternObject ObjectExpr | PatternObject_Velocity Velocity SingleObject deriving(Eq)
instance Show PatternObj where
  show (PatternObject expr) = show expr
  show (PatternObject_Velocity vel obj) = vel ++ " " ++ show obj

-- TODO may want to add more separators, not just the | that inherits scope
data Pattern = Pattern_PatternObj PatternObj | Pattern_Bin PatBinOp PatternObj Pattern deriving(Eq)

showPattern_ :: Pattern -> String
showPattern_ (Pattern_PatternObj p) = show p
showPattern_ (Pattern_Bin op p1 p2) = show p1 ++ " " ++ show op ++ " " ++ showPattern_ p2
instance Show Pattern where
  show p = "[" ++ showPattern_ p ++ "]"

newtype Patterns = Patterns { unPatterns::[Pattern] } deriving(Eq)

instance Show Patterns where
  show (Patterns []) = ""
  show (Patterns (x:xs)) = show x ++ " " ++ show (Patterns xs)

indentOnce :: String -> String
indentOnce = concat . map ("    " ++) . lines

data UnscopedRule = UnscopedRule_Patterns Patterns Patterns | UnscopedRule_Rule Patterns Rule | UnscopedRule_Boolean Boolean Rule deriving(Eq)
instance Show UnscopedRule where
  show (UnscopedRule_Patterns p1 p2) = show p1 ++ " -> " ++ show p2
  show (UnscopedRule_Rule p r) = show p ++ " ->\n" ++ indentOnce (show r)
  show (UnscopedRule_Boolean b r) = show b ++ " ->\n" ++ indentOnce (show r)

data Command = Cancel | Win | Message String deriving (Eq)
instance Show Command where
  show Cancel = "CANCEL"
  show Win = "WIN"
  show (Message x) = "MESSAGE " ++ x

data Rule = Rule_Command Command | Rule UnscopedRule | Rule_Scoped Velocity UnscopedRule deriving(Eq)
instance Show Rule where
  show (Rule_Command c) = show c
  show (Rule r) = show r
  show (Rule_Scoped vel r) = vel ++ " " ++ show r

newtype RuleGroup = RuleGroup { unRuleGroup :: [Rule] } deriving(Eq)
instance Show RuleGroup where
  show = concat . intersperse " +\n" . map show . unRuleGroup
