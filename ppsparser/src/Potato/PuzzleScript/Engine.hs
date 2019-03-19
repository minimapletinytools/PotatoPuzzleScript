module Potato.PuzzleScript.Engine (
  Point,
  Entry,
  LevelState,
  initLevelState
) where

import Potato.Math.Integral.TR
import Potato.PuzzleScript.Types
import Potato.PuzzleScript.ParserOutput
import qualified Data.List.Index as L
import qualified Data.Vector.Unboxed as U
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


objectExprToEntry :: ObjectExpr -> Entry
objectExprToEntry (ObjectExpr_Single (SingleObject obj)) = [("", obj)]
objectExprToEntry (ObjectExpr_Single (SingleObject_Orientation (Abs orient) obj)) = [(orient, obj)]
objectExprToEntry (ObjectExpr_Bin And_Obj exp1 exp2) = objectExprToEntry exp1 ++ objectExprToEntry exp2
objectExprToEntry _ = error "or not allowed"

initLevelState :: LegendMap -> Level -> LevelState
initLevelState lm (Level (x,_,_) entries _) = L.ifoldl outfoldfn Map.empty entries where
  bg = findBackgroundKey lm
  infoldfn :: Int -> LevelState -> Int -> Char -> LevelState
  infoldfn z m i e = if e == bg then m else Map.insert (i `mod` x, i `div` x, z) (objectExprToEntry (lm Map.! e)) m
  outfoldfn :: LevelState -> Int -> LevelSlice -> LevelState
  outfoldfn m i e = U.ifoldl (infoldfn i) m e

initGameState :: Output -> GameState
initGameState output = undefined


data ExecutionCtx = ExecutionCtx {
  _scope :: TR,
  _remainingRules :: Rules
}


emptyExecutionCtx :: ExecutionCtx
emptyExecutionCtx = ExecutionCtx {
    _scope = identity,
    _remainingRules = []
}

execLevel :: Rules -> LevelState -> LevelState
execLevel r l = undefined
