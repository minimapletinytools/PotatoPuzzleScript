{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Potato.PuzzleScript
import Potato.PuzzleScript.ExpressionParsers

import Text.Parsec

import Control.Monad
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

legendKeys :: [Char]
legendKeys = ['a'..'z']

newtype KnownROrientation = KnownROrientation ROrientation
instance Arbitrary KnownROrientation where
  arbitrary = do
    absorrel <- elements [Abs, Rel]
    orient <- elements $ Map.keys knownOrientations
    return $ KnownROrientation $ absorrel orient

newtype KnownRVelocity = KnownRVelocity RVelocity
instance Arbitrary KnownRVelocity where
  arbitrary = do
    absorrel <- elements [Abs, Rel]
    vel <- elements $ Map.keys knownVelocities
    return $ KnownRVelocity $ absorrel vel

newtype KnownObject = KnownObject Object deriving(Show)
instance Arbitrary KnownObject where
  arbitrary = KnownObject <$> elements defaultObjects

instance Arbitrary SingleObject where
  arbitrary = oneof $ [SingleObject <$> elements defaultObjects,
    do
      KnownObject obj <- arbitrary
      KnownROrientation orient <- arbitrary
      return $ SingleObject_Orientation orient obj]

instance Arbitrary ObjectExpr where
  arbitrary = sized arbSized_ObjectExpr where
    arbSized_ObjectExpr :: Int -> Gen ObjectExpr
    arbSized_ObjectExpr 0 = ObjectExpr_Single <$> arbitrary
    arbSized_ObjectExpr n = do
      split <- choose (0,n-1)
      l <- arbSized_ObjectExpr split
      r <- arbSized_ObjectExpr (n-split-1)
      op <- elements [And_Obj, Or_Obj]
      return $ ObjectExpr_Bin op l r

prop_parseObject :: KnownObject -> Bool
prop_parseObject (KnownObject obj) = case runParser parse_Object defaultOutput "(test)" (T.pack $ obj) of
  Left err -> trace (show err) $ False
  Right x -> obj == x

prop_parse_ObjectExpr :: ObjectExpr -> Bool
prop_parse_ObjectExpr expr = case runParser parse_ObjectExpr defaultOutput "(test)" (T.pack $ show expr) of
  Left err -> trace (show err) $ False
  Right x -> expr == x
  --Right x -> trace (show expr ++ " =? " ++ show x) $ expr == x

--newtype LegendExprs = LegendExprs [LegendExpr]
--instance Arbitrary LegendExprs where
--  arbitrary = LegendExprs <$>
--    (zipWith (\k v -> LegendExpr k v) <$> (listOf . elements $ legendKeys) `suchThat` hasNoDups
--    <*> listOf arbitrary)

instance Arbitrary PatternObj where
  arbitrary = oneof $ [PatternObject <$> arbitrary,
    do
      obj <- arbitrary
      KnownRVelocity vel <- arbitrary
      return $ PatternObject_Velocity vel obj]

instance Arbitrary Pattern where
  arbitrary = sized arbSized_Pattern where
    arbSized_Pattern :: Int -> Gen Pattern
    arbSized_Pattern 0 = Pattern_PatternObj <$> arbitrary
    arbSized_Pattern n = do
      obj <- arbitrary
      pat <- arbSized_Pattern (n-1)
      return $ Pattern_Bin Pipe obj pat

instance Arbitrary Patterns where
  arbitrary = do
    size <- getSize
    -- TODO better size distribution
    splits <- choose (1,3)
    pats <- replicateM splits (resize (size `div` splits) arbitrary)
    sublistOf pats >>= return . Patterns

prop_parse_Patterns :: Patterns -> Bool
prop_parse_Patterns pat = case runParser parse_Patterns defaultOutput "(test)" (T.pack $ show pat) of
  Left err -> trace (show err) $ False
  Right x -> pat == x













-- ugly old tests for legend expression and collision layers, please update with new ones
newtype KnownObjects = KnownObjects [String] deriving (Show)
instance Arbitrary KnownObjects where
  arbitrary = KnownObjects <$>
    (listOf . elements $ defaultObjects) `suchThat` hasNoDups


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



-- test the full parser on a file
prop_parseFile_pass_1 :: Property
prop_parseFile_pass_1 = monadicIO $ assert =<< (run $ do
  text1 <- T.readFile "test.txt"
  -- simpler way to do this o_o?
  case runParser potatoParse emptyOutput "(unknown)" text1 of
    Left x -> return False
    Right _ -> return True)

--Template haskell nonsense to run all properties prefixed with "prop_" in this file
return []

main :: IO Bool
main = $quickCheckAll
--main = $verboseCheckAll
