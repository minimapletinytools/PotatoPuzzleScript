module Potato.PuzzleScript.Engine(

) where

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

data Rules = Rules

initGameState :: Output -> GameState
initGameState output = undefined


execLevel :: Rules -> LevelState -> LevelState
execLevel r l = undefined
