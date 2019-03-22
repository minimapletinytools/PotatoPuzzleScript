{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


import Potato.Math.Integral.Rot
import Potato.Math.Integral.TR

import Linear.V3
import qualified Linear.Matrix as M

import Test.QuickCheck

import Debug.Trace

prop_TODO :: Bool
prop_TODO = True

instance (Bounded a, Integral a) => Arbitrary (V3 a) where
  arbitrary :: Gen (V3 a)
  arbitrary = do
    x <- arbitrarySizedIntegral
    y <- arbitrarySizedIntegral
    z <- arbitrarySizedIntegral
    return (V3 x y z)

instance (Bounded a, Integral a) => Arbitrary (M.M33 a) where
  arbitrary :: Gen (M.M33 a)
  arbitrary = do
    v1 <- arbitrary
    v2 <- arbitrary
    v3 <- arbitrary
    return (V3 v1 v2 v3)

e :: Integral a => M.M33 a
e =   V3 (V3 1 0 0)
         (V3 0 1 0)
         (V3 0 0 1)

x :: Integral a => M.M33 a
x =   V3 (V3 1 0 0)
         (V3 0 0 (-1))
         (V3 0 1 0)

z :: Integral a => M.M33 a
z =   V3 (V3 0 (-1) 0)
         (V3 1 0 0)
         (V3 0 0 1)

y :: Integral a => M.M33 a
y =   V3 (V3 0 0 (-1))
         (V3 0 1 0)
         (V3 1 0 0)

newtype OrthogonalRotation a = OrthogonalRotation (M.M33 a)
instance (Integral a) => Arbitrary (OrthogonalRotation a) where
  arbitrary :: Gen (OrthogonalRotation a)
  arbitrary = do
    rotlist <- listOf $ elements [x,y,z,e]
    return $ OrthogonalRotation (foldl (M.!*!) e rotlist)

instance Arbitrary TR where
  arbitrary :: Gen TR
  arbitrary = do
    trans <- arbitrary
    OrthogonalRotation rot <- arbitrary
    return (TR trans rot)

prop_invTR :: TR -> Bool
prop_invTR tr = (invTR tr) !*! tr == identity


--Template haskell nonsense to run all properties prefixed with "prop_" in this file
return []

main :: IO Bool
main = $quickCheckAll
--main = $verboseCheckAll
