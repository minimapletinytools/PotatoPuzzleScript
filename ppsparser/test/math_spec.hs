{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


import Potato.Math.Integral.Rot
import Potato.Math.Integral.TR

import Test.QuickCheck

prop_TODO :: Bool
prop_TODO = True


--Template haskell nonsense to run all properties prefixed with "prop_" in this file
return []

main :: IO Bool
main = $quickCheckAll
--main = $verboseCheckAll
