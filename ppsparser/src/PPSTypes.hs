{-# LANGUAGE TemplateHaskell #-}

module PPSTypes(
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

type ObjectMap = Map.Map Object Color
type LegendMap = Map.Map Char Expr

data UnOp = Not
  | All | No | Some  -- win cond operators
  deriving (Show)
data BinOp = And | Or | Arrow
  | On -- win cond operators
  deriving (Show)
data Expr = ConstExpr Object | UnExpr UnOp Expr | BinExpr BinOp Expr Expr deriving (Show)


-- | isWinConditionExpr returns true if the expression is a valid win condition expression
-- a valid win condition expressions are limited see https://www.puzzlescript.net/Documentation/winconditions.html
-- for complex win conditions, use rules instead
isWinConditionExpr :: Expr -> Bool
isWinConditionExpr (BinExpr On x (ConstExpr _)) = isWinConditionExpr_ x
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
