module Potato.PuzzleScript.SpaceTypes (
  SpaceModifier(..),
  SpaceModifiedString(..),
  Orientation,
  Velocity,
  ROrientation,
  RVelocity,
  RRotation(..),
  RTR(..),
  flattenRRotation,
  flattenRTR,
  isDirection,
  listToMatcher,
  RRotationMatcher,
  RTRMatcher,
  OrientationMap,
  knownOrientations,
  makeOrientation,
  VelocityMap,
  knownVelocities,
  makeVelocity
) where

import Potato.Math.Integral.TR
import qualified Linear.Matrix as M
import qualified Data.Map as Map

data SpaceModifier = Abs | Rel | Default deriving(Eq, Show)
combineSpaceModifier :: SpaceModifier -> SpaceModifier -> SpaceModifier
combineSpaceModifier Abs _ = Abs
combineSpaceModifier Rel _ = Rel
combineSpaceModifier Default x = x

data SpaceModifiedString = SpaceModifiedString SpaceModifier String deriving(Eq)

instance Show (SpaceModifiedString) where
  show (SpaceModifiedString Abs x) = "Abs " ++ x
  show (SpaceModifiedString Rel x) = "Rel " ++ x
  show (SpaceModifiedString Default x) = x



type Orientation = String
type Velocity = String

-- TODO rename to SMOrientation/Velocity
-- Orientations are relative by default
-- Velocities are absolute by default
type ROrientation = SpaceModifiedString
type RVelocity = SpaceModifiedString

-- TODO rename to SMRotation/TR
data RRotation = RRotation SpaceModifier Rotation deriving(Show)
data RTR = RTR SpaceModifier TR deriving(Show)

-- | flattenRRotation returns RRotations relative to parent (if relative) in global scope
-- Default means Rel
flattenRRotation :: TR -> RRotation -> Rotation
flattenRRotation parent (RRotation sm r) = case sm of
  Abs -> r
  _ -> (_rotation parent) M.!*! r

-- | flattenRTR returns RTR relative to parent (if relative) in global scope
-- Default means Abs
flattenRTR :: TR -> RTR -> TR
flattenRTR parent (RTR sm tr) = case sm of
  Rel -> parent !*! tr
  _ -> tr

-- |
isDirection :: TR -> Bool
isDirection tr = _translation tr /= zeroTranslation && _rotation tr == zeroRotation

data Matcher_ a = Matcher_ {
  matcher :: a -> Bool,
  enumerate :: [a]
}

listToMatcher :: (Eq a) => [a] -> Matcher_ a
listToMatcher xs = Matcher_ {
  matcher = \x -> elem x xs,
  enumerate = xs
}

type RRotationMatcher = Matcher_ RRotation
type RTRMatcher = Matcher_ RTR


type OrientationMap = Map.Map Orientation RRotation

knownOrientations :: OrientationMap
knownOrientations = Map.fromList [("R_FORWARD", RRotation Rel zeroRotation)]

makeOrientation :: OrientationMap -> ROrientation -> Maybe (RRotation)
makeOrientation om (SpaceModifiedString osm k) = do
  RRotation sm r <- Map.lookup k om
  return $ RRotation (combineSpaceModifier sm osm) r

type VelocityMap = Map.Map Velocity (RTR)

knownVelocities :: VelocityMap
knownVelocities = Map.fromList [("v", RTR Abs identity),("^", RTR Abs identity),(">", RTR Abs identity),("<", RTR Abs identity)]

makeVelocity :: VelocityMap -> RVelocity -> Maybe (RTR)
makeVelocity om (SpaceModifiedString osm k) = do
  RTR sm r <- Map.lookup k om
  return $ RTR (combineSpaceModifier sm osm) r


{-type OrientationMap = Map.Map Orientation RRotationMatcher

knownOrientations :: OrientationMap
knownOrientations = Map.fromList
  [("R_FORWARD", listToMatcher [RRotation Rel zeroRotation])]


type VelocityMap = Map.Map Velocity RTRMatcher

knownVelocities :: VelocityMap
knownVelocities = Map.fromList
  [ ("v", listToMatcher [RTR Abs identity])
  , ("^", listToMatcher [RTR Abs identity])
  , (">", listToMatcher [RTR Abs identity])
  , ("<", listToMatcher [RTR Abs identity])]
-}


--knownDirections :: VelocityMap