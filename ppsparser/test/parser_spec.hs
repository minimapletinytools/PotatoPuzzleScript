{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Potato.PuzzleScript
import Potato.PuzzleScript.ExpressionParsers

import Text.Parsec

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (nub, intersperse)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Builder

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Debug.Trace (trace)


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


instance Arbitrary ROrientation where
  arbitrary = do
    absorrel <- elements [Abs, Rel]
    orient <- elements $ Map.keys knownOrientations
    return $ absorrel orient

newtype KnownObject = KnownObject Object deriving(Show)
instance Arbitrary KnownObject where
  arbitrary = elements defaultObjects >>= return . KnownObject

instance Arbitrary SingleObject where
  arbitrary = oneof $ [elements defaultObjects >>= return . SingleObject,
    do
      KnownObject obj <- arbitrary
      orient <- arbitrary
      return $ SingleObject_Orientation orient obj]

instance Arbitrary ObjectExpr where
  arbitrary = sized arbSized_ObjectExpr where
    arbSized_ObjectExpr :: Int -> Gen ObjectExpr
    arbSized_ObjectExpr 0 = arbitrary >>= return . ObjectExpr_Single
    arbSized_ObjectExpr n = do
      split <- choose (0,n-1)
      l <- arbSized_ObjectExpr split
      r <- arbSized_ObjectExpr (n-split-1)
      op <- elements [And_Obj, Or_Obj]
      return $ ObjectExpr_Bin op l r

prop_parseObject :: KnownObject -> Bool
prop_parseObject (KnownObject obj) = case runParser (parse_Object defaultObjectMap) defaultOutput "(test)" (T.pack $ obj) of
  Left err -> trace (show err) $ False
  Right x -> obj == x

prop_parseObjectExpr :: ObjectExpr -> Bool
prop_parseObjectExpr expr = case runParser (parse_ObjectExpr defaultObjectMap) defaultOutput "(test)" (T.pack $ show expr) of
  Left err -> trace (show err) $ False
  Right x -> expr == x
  --Right x -> trace (show expr ++ " =? " ++ show x) $ expr == x






newtype KnownObjects = KnownObjects [String] deriving (Show)
instance Arbitrary KnownObjects where
  arbitrary = KnownObjects <$>
    (listOf . elements $ defaultObjects) `suchThat` hasNoDups

legendKeys :: [Char]
legendKeys = ['a'..'z']
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

prop_parseCollisionLayers :: [KnownObjects] -> Property
prop_parseCollisionLayers objects = monadicIO $ do
  let
    objectStrings :: [[String]] = map (\(KnownObjects x) -> x) objects
    interspersedCommas :: [[String]] = map (intersperse ", ") objectStrings
    interspersedLines :: [[String]] = intersperse ["\n"] interspersedCommas
    text = decodeUtf8 . B.toStrict . toLazyByteString . stringUtf8 . concat . concat $ interspersedLines
  assert $ case runParser (parseCollisionLayers >> getState) defaultOutput "(test)" text of
    Left x -> False
    -- TODO check output matches input
    Right _ -> True


prop_parseFile_pass_1 :: Property
prop_parseFile_pass_1 = monadicIO $ assert =<< (run $ do
  text1 <- T.readFile "test.txt"
  -- simpler way to do this o_o?
  case runParser potatoParse emptyOutput "(unknown)" text1 of
    Left x -> return False
    Right _ -> return True)

-- TODO test for duplicate legend keys when it's supported

{-
prop_isWinCondition :: Property
prop_isWinCondition = monadicIO $ do
  assert . isWinCondition $ UnExpr Some (ObjectExpr anObject)
  assert . isWinCondition $ BinExpr On (UnExpr Some (ObjectExpr anObject)) (ObjectExpr anObject)
  assert . not . isWinCondition $ ObjectExpr anObject
  assert . not . isWinCondition $ BinExpr On (ObjectExpr anObject) (ObjectExpr anObject)
-}

-- TODO
--prop_parseWinConditions :: [(KnownObjects, KnownObjects)] -> Bool
--prop_parseWinConditions pairs = pass where

--Template haskell nonsense to run all properties prefixed with "prop_" in this file
return []

main :: IO Bool
main = $quickCheckAll
--main = $verboseCheckAll
