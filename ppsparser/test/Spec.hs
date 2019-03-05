{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import PPSParser
import PPSTypes

import Text.Parsec

import qualified Data.Text.IO as T
import Data.List (nub)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Builder

import Test.QuickCheck
import Test.QuickCheck.IO
import Test.HUnit.Lang

--import Debug.Trace (trace)


-- predefined list of objects we will use in our test
-- I suppose we could be even more awesome and generate these...
defaultObjects :: [String]
defaultObjects = map (\x -> "Obj"++show x) [(1::Int)..20]

defaultOutput :: Output
defaultOutput = emptyOutput {
    _objectList = Map.fromList (map (,white) defaultObjects)
  }

legendKeys :: [Char]
legendKeys = ['a'..'z']
newtype LegendTuples = LegendTuples [(Char,String)] deriving (Show)

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

-- | Always worth to test if we wrote `hasNoDups` properly.
prop_hasNoDups :: [Int] -> Property
prop_hasNoDups xs = hasNoDups xs === (nub xs == xs)

instance Arbitrary LegendTuples where
  arbitrary = LegendTuples <$>
    (zip <$> (listOf . elements $ legendKeys) `suchThat` hasNoDups
    <*> (listOf . elements $ defaultObjects) `suchThat` hasNoDups)

prop_parseLegend_pass :: LegendTuples -> Bool
prop_parseLegend_pass (LegendTuples tuples) = pass where
  text = decodeUtf8 . B.toStrict . toLazyByteString . mconcat $ map (\(a,b)-> charUtf8 a <> stringUtf8 " = " <> stringUtf8 b <> charUtf8 '\n') tuples
  pass = case runParser parseLegend defaultOutput "(test)" text of
    Left _ -> False
    Right _ -> True

prop_parseLegend_fail :: LegendTuples -> Bool
prop_parseLegend_fail (LegendTuples tuples) = not $ prop_parseLegend_pass (LegendTuples (('*',"thisobjectdoesnotexist"):tuples))

prop_parseFile_pass_1 :: Property
prop_parseFile_pass_1 = propertyIO $ do
  text1 <- T.readFile "test.txt"
  -- simpler way to do this o_o?
  case runParser potatoParse emptyOutput "(unknown)" text1 of
    Left x -> assertFailure (show x)
    Right _ -> return ()

-- TODO test for duplicate legend keys when it's supported

--Template haskell nonsense to run all properties prefixed with "prop_" in this file
return []

main :: IO Bool
main = $quickCheckAll
--main = $verboseCheckAll
