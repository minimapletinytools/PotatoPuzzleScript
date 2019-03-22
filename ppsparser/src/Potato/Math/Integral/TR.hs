{-|
Module      : TR
Description : data type representing translations and rotations in R3 Int
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental


-- TODO replace Int with (Integral a) => a
-}


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}


module Potato.Math.Integral.TR
  ( Translation
  , Rotation
  , TR(..)
  , identity
  , toM44
  , transformV4
  , transformV3
  , (!*)
  , (!*!)
  , invTR
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
  _translation :: Translation
  -- TODO switch to cube rotation group
  , _rotation :: Rotation
  } deriving (Eq, Show, Generic, NFData)

makeLenses ''TR

identity :: TR
identity = TR (V3 0 0 0) M.identity

axisX :: (Num a) => V3 a
axisX = V3 1 0 0

axisY :: (Num a) => V3 a
axisY = V3 0 1 0

axisZ :: (Num a) => V3 a
axisZ = V3 0 0 1

--up :: TR -> V3 Int
--up tr = transformV3 tr axisY

m33_to_homogenous_m44 :: (Num a) => M.M33 a -> M.M44 a
m33_to_homogenous_m44 (V3 (V3 a b c) (V3 d e f) (V3 g h i)) =
    V4  (V4 a b c 0)
        (V4 d e f 0)
        (V4 g h i 0)
        (V4 0 0 0 1)

translation_to_homogenuous_m44 :: Translation -> M.M44 Int
translation_to_homogenuous_m44 (V3 x y z) =
  V4 (V4 1 0 0 x) (V4 0 1 0 y) (V4 0 0 1 z) (V4 0 0 0 1)

-- | toM44 converts TR to its homogeneous 4x4 Matrix representation
toM44 :: TR -> M.M44 Int
toM44 (TR t r) =
    translation_to_homogenuous_m44 t M.!*! m33_to_homogenous_m44 r

-- | transformV4 applies TR to a homegeneous V4
transformV4 :: TR -> V4 Int -> V4 Int
transformV4 tr v = toM44 tr M.!* v

-- | transformV3 applies TR to a V3
transformV3 :: TR -> V3 Int -> V3 Int
transformV3 tr (V3 x y z) = V3 x' y' z' where V4 x' y' z' _ = transformV4 tr (V4 x y z 1)

-- | alias for transformV3
infixl 7 !*
(!*) :: TR -> V3 Int -> V3 Int
(!*) = transformV3

-- | product of 2 TRs (parent * child)
infixl 7 !*!
-- inherit P C returns P * C, i.e. C in the frame of P
(!*!) :: TR -> TR -> TR
(TR pt pr) !*! (TR ct cr) = TR
  (pt ^+^ (pr M.!* ct))
  (pr M.!*! cr)

-- | invTR returns the inverse of a TR
-- TODO test
invTR :: TR -> TR
invTR (TR trans rot) = TR (rot_inv M.!* (-trans)) rot_inv where
  rot_inv = M.transpose rot
