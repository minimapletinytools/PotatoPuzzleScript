{-|
Module      : TR
Description : types represented int translations and rotations in R3
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

-}


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}


module Potato.Math.Integral.TR
  ( Translation
  , Rotation
  , TR(..)
  ) where

import Lens.Micro.Platform
import Control.DeepSeq
import GHC.Generics (Generic)
import Linear.Conjugate
import qualified Linear.Matrix as M
import Linear.V3
import Linear.V4
import Linear.Vector

type Translation = V3 Int

-- TODO enforce 90 degree increment rotation at the type level
-- only 24 possibilities
type Rotation = M.M33 Int


-- | matrix::M44 = T * R
data TR = TR
  {
  _trans :: Translation
  , _rot :: Rotation
  } deriving (Show, Generic, NFData)

makeLenses ''TR

axisX :: (Num a) => V3 a
axisX = V3 1 0 0

axisY :: (Num a) => V3 a
axisY = V3 0 1 0

axisZ :: (Num a) => V3 a
axisZ = V3 0 0 1

up :: TR -> V3 Int
up tr = transformV3 tr axisY

identity :: TR
identity = TR (V3 0 0 0) M.identity

m33_to_homogenous_m44 :: (Num a) => M.M33 a -> M.M44 a
m33_to_homogenous_m44 (V3 (V3 a b c) (V3 d e f) (V3 g h i)) =
    V4  (V4 a b c 0)
        (V4 d e f 0)
        (V4 g h i 0)
        (V4 0 0 0 1)

fromTranslation :: Translation -> M.M44 Int
fromTranslation (V3 x y z) =
  V4 (V4 1 0 0 x) (V4 0 1 0 y) (V4 0 0 1 z) (V4 0 0 0 1)

fromTR :: TR -> M.M44 Int
fromTR (TR t r) =
    fromTranslation t M.!*! m33_to_homogenous_m44 r

transformV3 :: TR -> V3 Int -> V3 Int
--transformV3 (TR pt pr ps) ct = pt ^+^ (pr `rotate` (ps M.!* ct))
transformV3 tr (V3 x y z) = V3 x' y' z' where V4 x' y' z' _ = transformV4 tr (V4 x y z 1)

transformV4 :: TR -> V4 Int -> V4 Int
transformV4 tr v = fromTR tr M.!* v

infixl 7 !*!
-- inherit P C returns P * C, i.e. C in the frame of P
(!*!) :: TR -> TR -> TR
(TR pt pr) !*! (TR ct cr) = TR
  (pt ^+^ (pr M.!* ct))
  (pr M.!*! cr)

invTR :: TR -> TR
invTR _ = undefined
--invTR (TR t r s) = undefined
