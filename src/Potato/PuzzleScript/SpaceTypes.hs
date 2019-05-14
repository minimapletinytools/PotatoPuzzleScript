module Potato.PuzzleScript.SpaceTypes (
  SpaceModifier(..),
  Orientation,
  Velocity,
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
  knownVelocities
) where

import Potato.Math.Integral.TR
import qualified Linear.Matrix as M
import qualified Data.Map as Map
import Control.Monad

data SpaceModifier = Abs | Rel | Default deriving(Eq, Show)
combineSpaceModifier :: SpaceModifier -> SpaceModifier -> SpaceModifier
combineSpaceModifier Abs _ = Abs
combineSpaceModifier Rel _ = Rel
combineSpaceModifier Default x = x

-- these are named identifiers of Rotations and TRs
-- TODO rename these variables
type Orientation = String
type Velocity = String

-- TODO rename to SMRotation/TR
data RRotation = RRotation SpaceModifier Rotation deriving(Show, Eq)
data RTR = RTR SpaceModifier TR deriving(Show, Eq)

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

instance (Show a) => Show (Matcher_ a) where
  show = show . enumerate

listToMatcher :: (Eq a) => [a] -> Matcher_ a
listToMatcher xs = Matcher_ {
  matcher = \x -> elem x xs,
  enumerate = xs
}

type RRotationMatcher = Matcher_ RRotation
type RTRMatcher = Matcher_ RTR

isRTRMatcherDirection :: RTRMatcher -> Bool
isRTRMatcherDirection m = count == 1 && r where
  xs = enumerate m
  count = length xs
  x = head xs
  r = case x of
    RTR Abs _ -> True
    _ -> False

isRRotationMatcherOrientation :: RRotationMatcher -> Bool
isRRotationMatcherOrientation m = count == 1 && r where
  xs = enumerate m
  count = length xs
  x = head xs
  r = case x of
    RRotation Abs _ -> True
    _ -> False

-- |
-- use for parsing object orientation in legend
makeOrientation :: OrientationMap -> Orientation -> Maybe (RRotation)
makeOrientation om e = do
  _xs <- Map.lookup e om
  let xs = enumerate _xs
  guard $ length xs == 1
  return $ head xs

type OrientationMap = Map.Map Orientation RRotationMatcher
type VelocityMap = Map.Map Velocity RTRMatcher


knownOrientations :: OrientationMap
knownOrientations = Map.fromList
  [ ("FFORWARD", listToMatcher [RRotation Rel zeroRotation])
  , ("FUP", listToMatcher [RRotation Rel zeroRotation])
  , ("FBACKWARD", listToMatcher [RRotation Rel zeroRotation])
  , ("FDOWN", listToMatcher [RRotation Rel zeroRotation])
  , ("FLEFT", listToMatcher [RRotation Rel zeroRotation])
  , ("FRIGHT", listToMatcher [RRotation Rel zeroRotation])
  , ("FPLUSZ", listToMatcher [RRotation Rel zeroRotation])
  , ("FMINUSZ", listToMatcher [RRotation Rel zeroRotation])
  ]

knownVelocities :: VelocityMap
knownVelocities = Map.fromList
  [ ("^", listToMatcher [RTR Rel identity])
  , ("v", listToMatcher [RTR Rel identity])
  , ("<", listToMatcher [RTR Rel identity])
  , (">", listToMatcher [RTR Rel identity])
  , ("^^", listToMatcher [RTR Rel identity])
  , ("VV", listToMatcher [RTR Rel identity])

  --, ("↶", listToMatcher [RTR Rel identity])
  --, ("↷", listToMatcher [RTR Rel identity])

  , ("UP", listToMatcher [RTR Abs identity])
  , ("DOWN", listToMatcher [RTR Abs identity])
  , ("LEFT", listToMatcher [RTR Abs identity])
  , ("RIGHT", listToMatcher [RTR Abs identity])
  , ("PLUSZ", listToMatcher [RTR Abs identity])
  , ("MINUSZ", listToMatcher [RTR Abs identity])

  , ("TURN_LEFT", listToMatcher [RTR Rel identity])
  , ("TURN_RIGHT", listToMatcher [RTR Rel identity])

  , ("VERTICAL", listToMatcher [RTR Abs identity, RTR Abs identity])
  , ("HORIZONTAL", listToMatcher [RTR Abs identity, RTR Abs identity])

  , ("PARALLEL", listToMatcher [RTR Rel identity, RTR Rel identity])
  , ("PERPENDICULAR", listToMatcher [RTR Rel identity, RTR Rel identity])
  ]


knownDirections :: VelocityMap
knownDirections = Map.fromList
  [ ("UP", listToMatcher [RTR Abs identity])
  , ("DOWN", listToMatcher [RTR Abs identity])
  , ("LEFT", listToMatcher [RTR Abs identity])
  , ("RIGHT", listToMatcher [RTR Abs identity])
  , ("PLUSZ", listToMatcher [RTR Abs identity])
  , ("MINUSZ", listToMatcher [RTR Abs identity])
  ]
