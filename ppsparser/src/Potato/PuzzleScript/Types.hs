{-# LANGUAGE TemplateHaskell #-}

module Potato.PuzzleScript.Types (
  Header(..),
  headerStrings,

  Size,
  LevelSlice,
  Level(..),

  Object,
  Color,
  white,

  UnOp(..),
  BinOp(..),
  Expr(..),


  ObjectMap, LegendMap,

  Output(..),
  title, author, homepage, headers, objectList, legend, collisionLayers, winConditions, levels,
  emptyOutput,

  PotatoParser,

  -- internal stuff
  -- TODO eventually do the internal module trick
  isObjBinOn,
  isSingleObject,
  isPatternObject,
  isWinCondition
) where

import Potato.Math.Integral.TR
import qualified Data.Text as T
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
white :: Color
white = "white"

type Size = (Int, Int)
type LevelSlice = U.Vector Char
data Level = Level Size [LevelSlice] String deriving(Show)

-- | TODO Matrix var support
--type Matrix = String

-- | ObjMod represents an object modifier token
-- there are two distinct types of ObjMod VelMod and RotMod
-- RotMod represents orientation of an object
-- VelMod represents velocity of a moving object
-- then there are two distinct categories of ObjMod Static and Contextual
-- contextual mods match several patterns and create a context object that is used later on in the pattern matching
-- (note a static mod is just a contextual mod that uses itself as the context)
--type ObjMod = String
--type VelMod = String
--type RotMod = String

-- context is hierarchical
  -- [up obj1 |> down obj2 |> right obj3] -> [any obj1 |> any obj2 |> any obj3]
    -- RHS anys gets replaced with up down right in that order
  -- [up obj1 |> (any obj2 |^ relforward obj3) |> relforward obj4 ] -> [any obj1 |> any obj2 |> any obj3 |> any obj3]
    -- relforward obj3 is matched with <rotation of obj2> obj3
    -- relforward obj4 is matched with up obj4 and is next to obj2 <-- this is a little weird but makes it easier to express some patterns than if we chose obj3 here
--RotMods
--ignore, matched with all rots on LHS, can only be replaced with ignore or static mods on RHS
--any, matched with all rots on LHS, sets current rot as context

type ObjectMap = Map.Map Object Color
type LegendMap = Map.Map Char Expr

data UnOp = Not
  | All | No | Some  -- win cond operators
  deriving (Show, Eq)
data BinOp =
  And | Or -- boolean operators
  | Arrow -- rule operators
  | On -- win cond operators
  | Pipe -- pattern operators (assoc right)
  deriving (Show, Eq)

--data ObjExpr = ObjectExpr Object | OrientObjExpr ObjExpr | BinOp ObjExpr ObjExpr
--data PatternExpr_ = PatternExpr_ deriving (Show)
--data PatternExpr = PatternExpr deriving (Show)
--data RuleExpr = RuleExpr deriving (Show)




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


data Output = Output {
    _title :: String,
    _author :: String,
    _homepage :: String,
    _headers :: [Header],
    _objectList :: ObjectMap,
    _legend :: LegendMap,
    _collisionLayers :: [[Object]],
    _winConditions :: [Expr],
    _levels :: [Level]
} deriving (Show)

makeLenses ''Output

emptyOutput :: Output
emptyOutput = Output {
    _title = "",
    _author = "",
    _homepage = "",
    _headers = [],
    _objectList = Map.empty,
    _legend = Map.empty,
    _collisionLayers = [],
    _winConditions = [],
    _levels = []
  }

type PotatoParser = Parsec T.Text Output
