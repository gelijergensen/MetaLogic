{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module LogicSystemSpec where

import Control.Monad (liftM2)
import qualified Data.Set as Set
import qualified LogicSystem as LS
import qualified RApplicative as RApp
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

data TestLogic = TestLogic deriving (Eq, Show)

type TestFormula = LS.Formula TestLogic

instance LS.LogicSystem TestLogic where
  data Formula TestLogic a
    = TRUE
    | FALSE
    | VAR a
    | AND (TestFormula a) (TestFormula a)
    deriving (Eq, Ord, Show)

  newtype Rule TestLogic a = Rule
    { runRule ::
        TestFormula a ->
        TestFormula a
    }
  type RuleConstraint TestLogic = Eq

  rewriteRules = const . map Rule $ [_andTrue, _andFalse, _andSame]
  mapFormula f term@(AND x y) =
    f term <> RApp.liftA2 AND (LS.mapFormula f x) (LS.mapFormula f y)
  mapFormula f term = f term
  runRule = runRule

_andTrue :: TestFormula a -> TestFormula a
_andTrue (AND TRUE x) = x
_andTrue (AND x TRUE) = x
_andTrue x = x

_andFalse :: TestFormula a -> TestFormula a
_andFalse (AND FALSE _) = FALSE
_andFalse (AND _ FALSE) = FALSE
_andFalse x = x

_andSame :: Eq a => TestFormula a -> TestFormula a
_andSame (AND x y)
  | x == y = x
_andSame x = x

complexity :: TestFormula a -> Int
complexity (AND x y) = 1 + complexity x + complexity y
complexity (VAR _) = 1
complexity TRUE = 0
complexity FALSE = 0

instance (Ord a, Arbitrary a) => Arbitrary (LS.Frontier a) where
  arbitrary = LS.Frontier . Set.fromList <$> arbitrary

instance Arbitrary a => Arbitrary (LS.Formula TestLogic a) where
  arbitrary = sized formula'
    where
      formula' 0 = oneof [pure TRUE, pure FALSE, VAR <$> arbitrary]
      formula' n
        | n > 0 =
          frequency
            [ (1, pure TRUE),
              (1, pure FALSE),
              (1, VAR <$> arbitrary),
              (2, liftM2 AND (formula' halfN) (formula' halfN))
            ]
        | otherwise = error "negative size"
        where
          halfN = n `div` 2

rewriteOnceAtRoot :: Ord a => TestFormula a -> Set.Set (TestFormula a)
rewriteOnceAtRoot = LS.rewriteOnceAtRoot TestLogic

rewriteOnce :: Ord a => TestFormula a -> Set.Set (TestFormula a)
rewriteOnce = LS.rewriteOnce TestLogic

rewriteNTimes ::
  Ord a => Int -> TestFormula a -> LS.Completeness (Set.Set (TestFormula a))
rewriteNTimes = LS.rewriteNTimes TestLogic

spec :: Spec
spec = do
  describe "LogicSystem.searchWithNeighbors" $ do
    prop "Performs one step of a BFS (visited)" $
      \frs viss ->
        ( LS.visited
            . LS.searchWithNeighbors
              (\i -> LS.Frontier . Set.fromList $ [i - 1, i + 1])
            $ LS.SearchEnv (LS.Frontier frs) viss ::
            Set.Set Int
        )
          == Set.union frs viss
    prop "Performs one step of a BFS (frontier)" $
      \frs viss ->
        ( LS.unFrontier
            . LS.frontier
            . LS.searchWithNeighbors
              (\i -> LS.Frontier . Set.fromList $ [i - 1, i + 1])
            $ LS.SearchEnv (LS.Frontier frs) viss ::
            Set.Set Int
        )
          == Set.difference
            (Set.union (Set.map (subtract 1) frs) (Set.map (+ 1) frs))
            (Set.union frs viss)

  describe "LogicSystem.rewriteOnce/rewriteOnceAtRoot" $ do
    prop "rewriteOnceAtRoot yields subset of rewriteOnce" $
      \x ->
        Set.isSubsetOf
          (rewriteOnceAtRoot x)
          (rewriteOnce (x :: TestFormula Int))

  describe "LogicSystem.rewriteOnce" $ do
    prop "deeply modifies formula" $
      \x y z ->
        Set.isSubsetOf
          (Set.map (\y -> AND x (AND y z)) $ rewriteOnceAtRoot y)
          (rewriteOnce (AND x (AND y z) :: TestFormula Int))

  describe "LogicSystem.rewriteNTimes" $ do
    prop "Completely rewrites (if given long enough)" $
      \x -> case rewriteNTimes (complexity x + 1) (x :: TestFormula Int) of
        (LS.Complete _) -> True
        _ -> False
