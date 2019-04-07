{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Common

import Potato.PuzzleScript
import Potato.PuzzleScript.ExpressionParsers

import Text.Parsec

import Control.Monad
import qualified Data.Text as T
import qualified Data.Map as Map

import Test.QuickCheck

import Debug.Trace (trace)

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
    arbSized_Rule n = limitSize 10 $ oneof [Rule <$> resize (n-1) arbitrary,
      Rule_Scoped <$> elements (Map.keys knownVelocities) <*> resize (n-1) arbitrary]

instance Arbitrary RuleGroup where
  arbitrary = limitSize 10 $ arbitrary >>= return . RuleGroup
  --shrink (RuleGroup x) = map RuleGroup (shrink x)


{-prop_parse_Rule :: Rule -> Bool
prop_parse_Rule rule = case runParser parse_Rule defaultOutput "(test)" (T.pack $ show rule) of
  Left err -> trace (show err) $ False
  Right x -> rule == x -}

prop_parse_RuleGroup :: RuleGroup -> Bool
prop_parse_RuleGroup rule = case runParser parse_RuleGroup defaultOutput "(test)" (T.pack $ show rule) of
  Left err -> trace (show err) $ False
  Right x -> rule == x

--Template haskell nonsense to run all properties prefixed with "prop_" in this file
return []

main :: IO Bool
main = $quickCheckAll
--main = $verboseCheckAll
