{-|
Module      : CubeRot
Description :
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

see http://www.euclideanspace.com/maths/discrete/groups/categorise/finite/cube/index.htm for maths
-}


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}


module Potato.Math.CubeRot
  ( 
  ) where

import Lens.Micro.Platform
import GHC.Generics (Generic)
import Linear.Conjugate
import qualified Linear.Matrix as M
import Linear.V3
import Linear.V4
import Linear.Vector

e :: M.M33 Int
e =   V3 (V3 1 0 0)
         (V3 0 1 0)
         (V3 0 0 1)

x :: M.M33 Int
x =   V3 (V3 1 0 0)
         (V3 0 0 (-1))
         (V3 0 1 0)

z :: M.M33 Int
z =   V3 (V3 0 (-1) 0)
         (V3 1 0 0)
         (V3 0 0 1)

y :: M.M33 Int
y =   V3 (V3 0 0 (-1))
         (V3 0 1 0)
         (V3 1 0 0)
