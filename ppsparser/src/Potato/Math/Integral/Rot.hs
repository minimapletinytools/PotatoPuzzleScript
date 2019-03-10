{-|
Module      : Rot
Description :
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

see http://www.euclideanspace.com/maths/discrete/groups/categorise/finite/cube/index.htm for maths
-}


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}


module Potato.Math.Integral.Rot
  (
  Rotation(..)
  ) where

data Rotation = Rotation

{- WIP
import Lens.Micro.Platform
import GHC.Generics (Generic)
import Linear.Conjugate
import qualified Linear.Matrix as M
import Linear.V3
import Linear.V4
import Linear.Vector

import Data.Group


data BaseRot = X | Y
data Rot = [Rot]

simplify :: Rot -> Rot
simplify (X:X:X:X:xs) = xs
simplify (Y:Y:Y:Y:xs) = xs
simplify (X:Y:Y:X:xs) = Y:Y:xs
simplify (Y:Y:X:X:xs) = X:X:Y:Y:xs
simplify (X:X:Y:X:X:xs) = Y:Y:Y:xs


mul :: Rot -> Rot -> Rot
mul [] x -> x
mul x [] -> x
mul _ _ -> undefined

instance Monoid Rot where
  mempty r = []
  mappend = mul

instance Group Rot where
  invert r = undefined

toM33 :: Rot -> M.M33 Int
toM33 [] = V3 (V3 1 0 0)
             (V3 0 1 0)
             (V3 0 0 1)
toM33 [X] = V3 (V3 1 0 0)
             (V3 0 0 (-1))
             (V3 0 1 0)
toM33 [Y] = V3 (V3 0 0 (-1))
             (V3 0 1 0)
             (V3 1 0 0)
-}
