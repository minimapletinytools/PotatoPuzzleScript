{-# LANGUAGE TemplateHaskell #-}

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
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import Data.Maybe

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


data ExecutionCtx = ExecutionCtx {
  _scope :: TR,
  _state :: LevelState
}

makeLenses ''ExecutionCtx

emptyExecutionCtx :: ExecutionCtx
emptyExecutionCtx = ExecutionCtx {
    _scope = identity,
    _state = Map.empty
}


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


-- orientation of found pattern is always relative to FORWARD so only a position is provided
findPatternObj :: ExecutionCtx -> PatternObj -> Maybe Translation
findPatternObj ctx pattern = undefined

findPattern :: ExecutionCtx -> Pattern -> Maybe [Translation]
findPattern ctx pattern = forM pattern (findPatternObj ctx)

applyRule_PatternOnce :: UnscopedRule -> ExecutionCtx -> (ExecutionCtx, Bool)
applyRule_PatternOnce (UnscopedRule_Pattern lhs rhs) ctx = undefined

applyUnscopedRule :: UnscopedRule -> ExecutionCtx -> ExecutionCtx
applyUnscopedRule (UnscopedRule_Pattern lhs rhs) ctx = undefined
applyUnscopedRule (UnscopedRule_Rule lhs rule) ctx = if isJust $ findPattern ctx lhs
  then applyRule rule ctx
  else ctx
applyUnscopedRule (UnscopedRule_Boolean lhs rule) ctx = undefined

velocityToScope :: Velocity -> TR
velocityToScope v = undefined

-- TODO make sure ctx scope is reset before calling this
-- maybe better just to note have scope as part of ctx...
applyRule :: Rule -> ExecutionCtx -> ExecutionCtx
applyRule (Rule_Command cmd) ctx = undefined
applyRule (Rule rule) ctx = applyUnscopedRule rule ctx
applyRule (Rule_Scoped vel rule) ctx = applyUnscopedRule rule ctx' where
  ctx' = set scope (velocityToScope vel) ctx


--applyRule :: Rule -> ExecutionCtx -> LevelState -> LevelState
--applyRule r ctx ls = iterate (\ls_ -> applyRuleOnce r ctx ) (ls, True)

initGameState :: Output -> GameState
initGameState output = undefined



execLevel :: Set.Set KeyboardInput -> Rules -> LevelState -> LevelState
execLevel r l = undefined
