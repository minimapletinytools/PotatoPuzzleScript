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
  -- TODO
  , ("FBACKWARD", listToMatcher [RRotation Rel zeroRotation])
  , ("FDOWN", listToMatcher [RRotation Rel zeroRotation])
  , ("FLEFT", listToMatcher [RRotation Rel zeroRotation])
  , ("FRIGHT", listToMatcher [RRotation Rel zeroRotation])
  , ("FPLUSZ", listToMatcher [RRotation Rel zeroRotation])
  , ("FMINUSZ", listToMatcher [RRotation Rel zeroRotation])
  ]

trup :: TR
trup = TR (V3 0 1 0) zeroRotation
trdown :: TR
trdown = TR (V3 0 (-1) 0) zeroRotation
trleft :: TR
trleft = TR (V3 1 0 0) zeroRotation
trright :: TR
trright = TR (V3 (-1) 0 0) zeroRotation
trplusz :: TR
trplusz = TR (V3 0 0 1) zeroRotation
trminusz :: TR
trminusz = TR (V3 0 0 (-1)) zeroRotation

tr90z :: TR
tr90z = TR zeroTranslation $
  V3 (V3 0 1 0)
     (V3 (-1) 0 0)
     (V3 0 0 1)

tr270z :: TR
tr270z = TR zeroTranslation $
  V3 (V3 0 (-1) 0)
     (V3 1 0 0)
     (V3 0 0 1)

knownVelocities :: VelocityMap
knownVelocities = Map.fromList
  [ ("^", listToMatcher [RTR Rel trup])
  , ("v", listToMatcher [RTR Rel trdown])
  , ("<", listToMatcher [RTR Rel trleft])
  , (">", listToMatcher [RTR Rel trright])
  , ("^^", listToMatcher [RTR Rel trplusz])
  , ("VV", listToMatcher [RTR Rel trminusz])

  -- TODO
  , ("↶", listToMatcher [RTR Rel tr90z])
  , ("↷", listToMatcher [RTR Rel tr270z])

  , ("PARALLEL", listToMatcher [RTR Rel trup, RTR Rel trdown])
  , ("PERPENDICULAR", listToMatcher [RTR Rel trleft, RTR Rel trright])

  , ("UP", listToMatcher [RTR Abs trup])
  , ("DOWN", listToMatcher [RTR Abs trdown])
  , ("LEFT", listToMatcher [RTR Abs trleft])
  , ("RIGHT", listToMatcher [RTR Abs trright])
  , ("PLUSZ", listToMatcher [RTR Abs trplusz])
  , ("MINUSZ", listToMatcher [RTR Abs trminusz])

  -- TODO
  , ("TURN_LEFT", listToMatcher [RTR Abs tr90z])
  , ("TURN_RIGHT", listToMatcher [RTR Abs tr270z])

  , ("VERTICAL", listToMatcher [RTR Abs trup, RTR Abs trdown])
  , ("HORIZONTAL", listToMatcher [RTR Abs trleft, RTR Abs trright])
  ]


knownDirections :: VelocityMap
knownDirections = Map.fromList
  [ ("UP", listToMatcher [RTR Abs trup])
  , ("DOWN", listToMatcher [RTR Abs trdown])
  , ("LEFT", listToMatcher [RTR Abs trleft])
  , ("RIGHT", listToMatcher [RTR Abs trright])
  , ("PLUSZ", listToMatcher [RTR Abs trplusz])
  , ("MINUSZ", listToMatcher [RTR Abs trminusz])
  ]
