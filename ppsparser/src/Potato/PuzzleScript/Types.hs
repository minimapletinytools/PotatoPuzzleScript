{-# LANGUAGE TemplateHaskell #-}

module Potato.PuzzleScript.Types (
  Header(..),
  headerStrings,

  Object,
  Color,
  white,

  UnOp(..),
  BinOp(..),
  Expr(..),
  isWinConditionExpr,

  ObjectMap, LegendMap,

  Output(..),
  title, author, homepage, headers, objectList, legend, collisionLayers, winConditions,
  emptyOutput,

  PotatoParser
) where

import Potato.Math.CubeTR
import qualified Data.Text as T
import qualified Data.Map as Map
import Text.Parsec

import Lens.Micro.Platform

data Header = OBJECTS | LEGEND | SOUNDS | COLLISIONLAYERS | RULES | WINCONDITIONS | LEVELS deriving (Read, Show)

headerStrings :: [String]
headerStrings = ["OBJECTS", "LEGEND", "SOUNDS", "COLLISIONLAYERS", "RULES", "WINCONDITIONS", "LEVELS"]

type Object = String

type Color = String
white :: Color
white = "white"

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
  deriving (Show)
data BinOp = And | Or | Arrow
  | On -- win cond operators
  deriving (Show)

data Expr = ObjExpr Object
  -- | ModObjExpr ObjMod ObjMod Object
  | UnExpr UnOp Expr
  | BinExpr BinOp Expr Expr deriving (Show)


-- | isModObjExpr returns true if the expression is a valid modified object expression
--isModObjExpr :: Expr -> Bool

-- | isWinConditionExpr returns true if the expression is a valid win condition expression
-- a valid win condition expressions are limited see https://www.puzzlescript.net/Documentation/winconditions.html
-- for complex win conditions, use rules instead
isWinConditionExpr :: Expr -> Bool
isWinConditionExpr (BinExpr On x (ObjExpr _)) = isWinConditionExpr_ x
isWinConditionExpr x = isWinConditionExpr_ x

isWinConditionExpr_ :: Expr -> Bool
isWinConditionExpr_ (UnExpr All _) = True
isWinConditionExpr_ (UnExpr No _) = True
isWinConditionExpr_ (UnExpr Some _) = True
isWinConditionExpr_ _ = False

data Output = Output {
    _title :: String,
    _author :: String,
    _homepage :: String,
    _headers :: [Header],
    _objectList :: ObjectMap,
    _legend :: LegendMap,
    _collisionLayers :: [[Object]],
    _winConditions :: [Expr]
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
    _winConditions = []
  }

type PotatoParser = Parsec T.Text Output
