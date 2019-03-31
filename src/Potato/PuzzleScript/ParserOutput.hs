{-# LANGUAGE TemplateHaskell #-}

module Potato.PuzzleScript.ParserOutput (
  Output(..),
  title, author, homepage, headers, objectList, legend, collisionLayers, rules, winConditions, levels, velocityList,
  emptyOutput,
  findBackgroundKey,
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
    -- TODO rename to objectMap
    _objectList :: ObjectMap,
    _legend :: LegendMap,
    _collisionLayers :: [[Object]],
    --list of loops of groups of rules
    _rules :: [Rule],
    _winConditions :: [WinCondExpr],
    _levels :: [Level]
}

instance Show Output where
  show o =
    "title: " ++ (show $ _title o) ++ "\n" ++
    "author: " ++ (show $ _author o) ++ "\n" ++
    "homepage: " ++ (show $ _homepage o) ++ "\n" ++
    "headers: " ++ (show $ _headers o) ++ "\n" ++
    "velocityList: " ++ (show $ _velocityList o) ++ "\n" ++
    "objectList: " ++ (show $ _objectList o) ++ "\n" ++
    "legend: " ++ (show $ _legend o) ++ "\n" ++
    "collisionLayers: " ++ (show $ _collisionLayers o) ++ "\n" ++
    "rules: " ++ (show $ _rules o) ++ "\n" ++
    "winConditions: " ++ (show $ _winConditions o) ++ "\n" ++
    "levels: " ++ (show $ _levels o)

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
    _rules = [],
    _winConditions = [],
    _levels = []
  }

findBackgroundKey :: LegendMap -> Char
findBackgroundKey lm = r where
  mapfn v = case v of
    ObjectExpr_Single (SingleObject "Background") -> Just ()
    _ -> Nothing
  r = case Map.keys (Map.mapMaybe mapfn lm) of
    [] -> '.'
    x:_ -> x

type PotatoParser = Parsec T.Text Output
