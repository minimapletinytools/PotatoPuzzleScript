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

splitSizeAndReduce_ :: Int -> Gen (Int, Int)
splitSizeAndReduce_ n = do
  x <- choose (0,n-1)
  return (x, n-x-1)

splitSizeAndReduce :: Gen (Int, Int)
splitSizeAndReduce = getSize >>= splitSizeAndReduce_

splitSize :: Gen (Int, Int)
splitSize = do
  n <- getSize
  x <- choose (0,n)
  return (x, n-x)

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


-- ExpressionParser tests

instance Arbitrary KeyboardInput where
  arbitrary = elements [K_NONE, K_LEFT, K_RIGHT, K_DOWN, K_UP, K_Z, K_X]

instance Arbitrary Boolean where
  arbitrary = sized arbSized_Boolean where
    arbSized_Boolean 0 = oneof [
      --Boolean_Var <$> arbitrary, -- TODO use known boolean vars names I guess?
      Boolean_Input <$> arbitrary,
      return Boolean_True,
      return Boolean_False]
    arbSized_Boolean n = oneof [
      Boolean_Not <$> arbSized_Boolean (n-1),
      do
        op <- elements [And, Or]
        (s1, s2) <- splitSizeAndReduce_ n
        b1 <- arbSized_Boolean s1
        b2 <- arbSized_Boolean s2
        return $ Boolean_Bin op b1 b2]

newtype KnownROrientation = KnownROrientation ROrientation
instance Arbitrary KnownROrientation where
  arbitrary = do
    absorrel <- elements [Abs, Rel, Default]
    orient <- elements $ Map.keys knownOrientations
    return $ KnownROrientation $ SpaceModifiedString absorrel orient

newtype KnownRVelocity = KnownRVelocity RVelocity
instance Arbitrary KnownRVelocity where
  arbitrary = do
    absorrel <- elements [Abs, Rel, Default]
    vel <- elements $ Map.keys knownVelocities
    return $ KnownRVelocity $ SpaceModifiedString absorrel vel

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
      (s1, s2) <- splitSizeAndReduce_ n
      l <- arbSized_ObjectExpr s1
      r <- arbSized_ObjectExpr s2
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


instance Arbitrary BasicWinCondExpr where
  arbitrary = do
    op <- elements [Win_All, Win_Some, Win_No]
    BasicWinCondExpr op <$> arbitrary

instance Arbitrary WinCondExpr where
  arbitrary = oneof $ [WinCondExpr_Basic <$> arbitrary,
    WinCondExpr_Bin Win_On <$> arbitrary <*> arbitrary]

prop_parse_WinCondExpr :: WinCondExpr -> Bool
prop_parse_WinCondExpr expr = case runParser parse_WinCondExpr defaultOutput "(test)" (T.pack $ show expr) of
  Left err -> trace (show err) $ False
  Right x -> expr == x

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
    let splits = max 1 . min 3 $ (size `div` 3)
    pats <- replicateM splits (resize (size `div` splits) arbitrary)
    return $ Patterns pats

instance Arbitrary UnscopedRule where
  arbitrary = oneof [
    do
      size <- getSize
      p1 <- resize size arbitrary
      p2 <- resize size arbitrary
      return $ UnscopedRule_Patterns p1 p2
    , do
      (s1, s2) <- splitSize
      pats <- resize s1 arbitrary
      rule <- resize s2 arbitrary
      return $ UnscopedRule_Rule pats rule]

      -- TODO put back when you have boolean input support
    {-, do
      (s1, s2) <- splitSize
      b <- resize s1 arbitrary
      rule <- resize s2 arbitrary
      return $ UnscopedRule_Boolean b rule]-}

instance Arbitrary Rule where
  arbitrary = sized arbSized_Rule where
    arbSized_Rule 0 = return $ Rule_Command "TODOREPLACEMEWITHAREALCOMMAND"
    arbSized_Rule n = oneof [Rule <$> resize (n-1) arbitrary,
      Rule_Scoped <$> elements (Map.keys knownVelocities) <*> resize (n-1) arbitrary]


prop_parse_Rule :: Rule -> Bool
prop_parse_Rule rule = case runParser parse_Rule defaultOutput "(test)" (T.pack $ show rule) of
  Left err -> trace (show err) $ False
  Right x -> rule == x







-- Parser tests
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
    Left x -> False
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
    Left x -> return False
    Right _ -> return True)

--Template haskell nonsense to run all properties prefixed with "prop_" in this file
return []

main :: IO Bool
main = $quickCheckAll
--main = $verboseCheckAll
