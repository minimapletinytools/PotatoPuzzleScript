{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common (
  splitSizeAndReduce,
  splitSize,
  limitSize,
  hasNoDups,
  defaultObjects,
  defaultObjectMap,
  anObject,
  defaultOutput,
  legendKeys,
  KnownROrientation(..),
  KnownRVelocity(..),
  KnownObject(..)
) where

import Potato.PuzzleScript

import qualified Data.Set as Set
import qualified Data.Map as Map

import Test.QuickCheck


splitSizeAndReduce_ :: Int -> Gen (Int, Int)
splitSizeAndReduce_ n = do
  x <- choose (0,n-1)
  return (x, n-x-1)

splitSizeAndReduce :: Gen (Int, Int)
splitSizeAndReduce = getSize >>= splitSizeAndReduce_

splitSize :: Gen (Int, Int)
splitSize = do
  n <- getSize
  x <- choose (0,n)
  return (x, n-x)

limitSize :: Int -> Gen a -> Gen a
limitSize x gen = do
  size <- getSize
  resize (min x size) gen

-- cnp https://stackoverflow.com/questions/30805230/uniqueness-and-other-restrictions-for-arbitrary-in-quickcheck
-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups :: (Ord a) => [a] -> Bool
hasNoDups = loop Set.empty
  where
    loop _ []       = True
    loop s (x:xs) | s' <- Set.insert x s, Set.size s' > Set.size s
                    = loop s' xs
                  | otherwise
                    = False

-- predefined list of objects we will use in our test
-- can we be even more awesome and generate these inside a test?
defaultObjects :: [String]
defaultObjects = map (\x -> "object"++show x) [(1::Int)..20]

defaultObjectMap :: ObjectMap
defaultObjectMap = Map.fromList (map (,"white") defaultObjects)

anObject :: String
anObject = head defaultObjects

defaultOutput :: Output
defaultOutput = emptyOutput {
    _objectList = defaultObjectMap
  }


legendKeys :: [Char]
legendKeys = ['a'..'u'] ++ ['w'..'z'] -- 'v' is used for velocity 😂


-- ExpressionParser tests

instance Arbitrary KeyboardInput where
  arbitrary = elements [K_NONE, K_LEFT, K_RIGHT, K_DOWN, K_UP, K_Z, K_X]

instance Arbitrary Boolean where
  arbitrary = sized arbSized_Boolean where
    arbSized_Boolean 0 = oneof [
      --Boolean_Var <$> arbitrary, -- TODO use known boolean vars names I guess?
      Boolean_Input <$> arbitrary,
      return Boolean_True,
      return Boolean_False]
    arbSized_Boolean n = oneof [
      Boolean_Not <$> arbSized_Boolean (n-1),
      do
        op <- elements [And, Or]
        (s1, s2) <- splitSizeAndReduce_ n
        b1 <- arbSized_Boolean s1
        b2 <- arbSized_Boolean s2
        return $ Boolean_Bin op b1 b2]

newtype KnownROrientation = KnownROrientation ROrientation
instance Arbitrary KnownROrientation where
  arbitrary = do
    absorrel <- elements [Abs, Rel, Default]
    orient <- elements $ Map.keys knownOrientations
    return $ KnownROrientation $ SpaceModifiedString absorrel orient

newtype KnownRVelocity = KnownRVelocity RVelocity
instance Arbitrary KnownRVelocity where
  arbitrary = do
    absorrel <- elements [Abs, Rel, Default]
    vel <- elements $ Map.keys knownVelocities
    return $ KnownRVelocity $ SpaceModifiedString absorrel vel

newtype KnownObject = KnownObject Object deriving(Show)
instance Arbitrary KnownObject where
  arbitrary = KnownObject <$> elements defaultObjects

instance Arbitrary SingleObject where
  arbitrary = oneof $ [SingleObject <$> elements defaultObjects,
    do
      KnownObject obj <- arbitrary
      KnownROrientation orient <- arbitrary
      return $ SingleObject_Orientation orient obj]

instance Arbitrary ObjectExpr where
  arbitrary = sized arbSized_ObjectExpr where
    arbSized_ObjectExpr :: Int -> Gen ObjectExpr
    arbSized_ObjectExpr 0 = ObjectExpr_Single <$> arbitrary
    arbSized_ObjectExpr n = do
      (s1, s2) <- splitSizeAndReduce_ n
      l <- arbSized_ObjectExpr s1
      r <- arbSized_ObjectExpr s2
      op <- elements [And_Obj, Or_Obj]
      return $ ObjectExpr_Bin op l r
