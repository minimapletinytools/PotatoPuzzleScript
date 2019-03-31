{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Common

import Potato.PuzzleScript

import Text.Parsec

import Control.Monad
import qualified Data.Text.IO as T
import Data.List (intersperse)

import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Builder

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Debug.Trace (trace)

newtype KnownObjects = KnownObjects [String] deriving (Show)
instance Arbitrary KnownObjects where
  arbitrary = KnownObjects <$>
    (listOf . elements $ defaultObjects) `suchThat` hasNoDups

prop_parseCollisionLayers :: [KnownObjects] -> Property
prop_parseCollisionLayers objects = monadicIO $ do
  let
    objectStrings :: [[String]] = map (\(KnownObjects x) -> x) objects
    interspersedCommas :: [[String]] = map (intersperse ", ") objectStrings
    interspersedLines :: [[String]] = intersperse ["\n"] interspersedCommas
    text = decodeUtf8 . B.toStrict . toLazyByteString . stringUtf8 . concat . concat $ interspersedLines
  assert $ case runParser (parseCollisionLayers >> getState) defaultOutput "(test)" text of
    Left _ -> False
    -- TODO check output matches input
    Right _ -> True

newtype LegendTuples = LegendTuples [(Char,String)] deriving (Show)
instance Arbitrary LegendTuples where
  arbitrary = LegendTuples <$>
    (zip <$> (listOf . elements $ legendKeys) `suchThat` hasNoDups
    <*> (listOf . elements $ defaultObjects) `suchThat` hasNoDups)

prop_parseLegend_pass :: LegendTuples -> Bool
prop_parseLegend_pass (LegendTuples tuples) = pass where
  text = decodeUtf8 . B.toStrict . toLazyByteString . mconcat $
    map (\(a,b)-> charUtf8 a <> stringUtf8 " = " <> stringUtf8 b <> charUtf8 '\n') tuples
  pass = case runParser (parseLegend >> getState) defaultOutput "(test)" text of
    Left _ -> False
    -- TODO check output matches input
    -- all (map (\x -> elem x tuples) )
    Right _ -> True

prop_parseLegend_fail :: LegendTuples -> Bool
prop_parseLegend_fail (LegendTuples tuples) = not $ prop_parseLegend_pass (LegendTuples (('*',"thisobjectdoesnotexist"):tuples))

-- test the full parser on a file
prop_parseFile_pass_1 :: Property
prop_parseFile_pass_1 = monadicIO $ assert =<< (run $ do
  text1 <- T.readFile "test.txt"
  -- simpler way to do this o_o?
  case runParser potatoParse emptyOutput "(unknown)" text1 of
    Left _ -> return False
    Right _ -> return True)

--Template haskell nonsense to run all properties prefixed with "prop_" in this file
return []

main :: IO Bool
main = $quickCheckAll
--main = $verboseCheckAll
