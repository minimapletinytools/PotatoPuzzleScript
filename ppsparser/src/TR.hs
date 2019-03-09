{-|
Module      : TR
Description : types represented int translations and rotations in R3
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

-}

module TR
  (
  TR(..)
  ) where

data TR = TR

{-
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}


module TR
  ( Translation
  , Rotation
  , TR(..)
  ) where

import Lens.Micro.Platform
import Control.DeepSeq
import GHC.Generics (Generic)
import Linear.Conjugate
import qualified Linear.Matrix as M
import Linear.Quaternion
import Linear.V3
import Linear.V4
import Linear.Vector

type Translation Int = V3 Int
type Rotation Int = M.M33 Int

-- | matrix::M44 = T * R
data TR Int = TR
  {
  _trans :: Translation Int
  , _rot :: Rotation Int
  } deriving (Show, Generic, NFData)

makeLenses ''TR

axisX :: (Num a) => V3 a
axisX = V3 1 0 0

axisY :: (Num a) => V3 a
axisY = V3 0 1 0

axisZ :: (Num a) => V3 a
axisZ = V3 0 0 1

up :: (RealFloat a) => TR a -> V3 a
up trs = transformV3 trs axisY

identity :: (Num a) => TR a
identity = TR (V3 0 0 0) M.identity

m33_to_homogenous_m44 :: (Num a) => M.M33 a -> M.M44 a
m33_to_homogenous_m44 (V3 (V3 a b c) (V3 d e f) (V3 g h i)) =
    V4  (V4 a b c 0)
        (V4 d e f 0)
        (V4 g h i 0)
        (V4 0 0 0 1)

fromTranslation :: (RealFloat a) => Translation a -> M.M44 a
fromTranslation (V3 x y z) =
  V4 (V4 1 0 0 x) (V4 0 1 0 y) (V4 0 0 1 z) (V4 0 0 0 1)

fromRotation :: (Num a) => Rotation a -> M.M33 a
fromRotation = M.fromQuaternion

fromTR :: (RealFloat a) => TR Int -> M.M44 Int
fromTR (TR t r) =
    fromTranslation t M.!*! m33_to_homogenous_m44 r

transformV3 :: TR Int -> V3 Int -> V3 Int
--transformV3 (TR pt pr ps) ct = pt ^+^ (pr `rotate` (ps M.!* ct))
transformV3 trs (V3 x y z) = V3 x' y' z' where V4 x' y' z' _ = transformV4 trs (V4 x y z 1)

transformV4 :: TR Int -> V4 Int -> V4 Int
transformV4 trs v = fromTR trs M.!* v

instance (Conjugate a, RealFloat a) => Hierarchical (TR a) where
  inherit (TR pt pr ps) (TR ct cr cs) =
    TR
      (pt ^+^ (pr `rotate` (ps M.!* ct)))
      (pr * cr)
      (fromRotation (QH.inverse cr) M.!*! ps M.!*! fromRotation cr M.!*! cs)

-- | TODO this is probably wrong
invTR :: TR a -> TR a
invTR _ = undefined
--invTR (TR t r s) = undefined-}
