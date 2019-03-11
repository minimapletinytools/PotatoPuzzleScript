{-# LANGUAGE TemplateHaskell #-}

module Potato.PuzzleScript.ParserOutput (
  Output(..),
  title, author, homepage, headers, objectList, legend, collisionLayers, winConditions, levels,
  emptyOutput,
  PotatoParser
) where

import Potato.PuzzleScript.Types
import qualified Data.Text as T
import qualified Data.Map as Map

import Text.Parsec

import Lens.Micro.Platform

data Output = Output {
    _title :: String,
    _author :: String,
    _homepage :: String,
    _headers :: [Header],
    _velocityList :: VelocityMap,
    _objectList :: ObjectMap,
    _legend :: LegendMap,
    _collisionLayers :: [[Object]],
    _winConditions :: [WinCond],
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
    _velocityList = knownVelocities,
    _legend = Map.empty,
    _collisionLayers = [],
    _winConditions = [],
    _levels = []
  }

type PotatoParser = Parsec T.Text Output
