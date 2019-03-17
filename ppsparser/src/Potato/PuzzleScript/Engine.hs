module Potato.PuzzleScript.Engine (
  Point,
  Entry,
  LevelState
) where

import Potato.Math.Integral.TR
import Potato.PuzzleScript.Types
import Potato.PuzzleScript.ParserOutput
import qualified Data.Vector as V
import qualified Data.Map as Map

import Lens.Micro.Platform

type Point = (Int, Int, Int)
type Entry = [(Orientation, Object)]
type Vars = Map.Map String Int
type LevelState = Map.Map Point Entry
data GameState = GameState {
  _size :: Point,
  _history :: [LevelState]
}

type Rules = [Rule]

initGameState :: Output -> GameState
initGameState output = undefined


data ExecutionCtx = ExecutionCtx {
  _scope :: TR,
  _remainingRules :: Rules
}


emptyExecutionCtx :: ExecutionCtx
emptyExecutionCtx = ExecutionCtx {
    _scope = emptyTR,
    _remainingRules = []
}

execLevel :: Rules -> LevelState -> LevelState
execLevel r l = undefined
