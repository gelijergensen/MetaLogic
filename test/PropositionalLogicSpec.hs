{-# LANGUAGE FlexibleInstances #-}

module PropositionalLogicSpec where

import Control.Monad (liftM2)
import qualified Data.Set as Set
import qualified LogicSystem as LS
import qualified PropositionalLogic as P
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

rewriteOnceAtRoot :: Ord a => P.PropFormula a -> Set.Set (P.PropFormula a)
rewriteOnceAtRoot = LS.rewriteOnceAtRoot P.PropositionalLogic

rewriteOnce :: Ord a => P.PropFormula a -> Set.Set (P.PropFormula a)
rewriteOnce = LS.rewriteOnce P.PropositionalLogic

instance Arbitrary a => Arbitrary (LS.Formula P.PropositionalLogic a) where
  arbitrary = sized formula'
    where
      formula' 0 = oneof [pure P.TRUE, pure P.FALSE, P.VAR <$> arbitrary]
      formula' n
        | n > 0 =
          frequency
            [ (1, pure P.TRUE),
              (1, pure P.FALSE),
              (1, P.VAR <$> arbitrary),
              (2, P.NOT <$> formula' halfN),
              (2, liftM2 P.OR (formula' halfN) (formula' halfN)),
              (2, liftM2 P.AND (formula' halfN) (formula' halfN)),
              (2, liftM2 P.IMPLIES (formula' halfN) (formula' halfN))
            ]
        | otherwise = error "negative size"
        where
          halfN = n `div` 2

spec :: Spec
spec = do
  describe "PropositionalLogic rewrite rules" $ do
    it "NOT TRUE ~~> FALSE" $ do
      Set.member
        (P.FALSE :: P.PropFormula ())
        (rewriteOnceAtRoot $ P.NOT P.TRUE)
    it "NOT FALSE ~~> TRUE" $ do
      Set.member
        (P.TRUE :: P.PropFormula ())
        (rewriteOnceAtRoot $ P.NOT P.FALSE)
    prop "NOT NOT x ~~> x" $
      \x ->
        Set.member
          (x :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.NOT $ P.NOT x)
    prop "OR X Y ~~> OR Y X" $
      \x y ->
        Set.member
          (P.OR y x :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.OR x y)
    prop "OR X (OR Y Z) ~~> OR (OR X Y) Z" $
      \x y z ->
        Set.member
          (P.OR x (P.OR y z) :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.OR (P.OR x y) z)
    prop "OR (OR X Y) Z ~~> OR X (OR Y Z)" $
      \x y z ->
        Set.member
          (P.OR (P.OR x y) z :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.OR x (P.OR y z))
    prop "OR TRUE X ~~> TRUE" $
      \x ->
        Set.member
          (P.TRUE :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.OR P.TRUE x)
    prop "OR X TRUE ~~> TRUE" $
      \x ->
        Set.member
          (P.TRUE :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.OR x P.TRUE)
    prop "OR FALSE X ~~> X" $
      \x ->
        Set.member
          (x :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.OR P.FALSE x)
    prop "OR X FALSE ~~> X" $
      \x ->
        Set.member
          (x :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.OR x P.FALSE)
    prop "OR X X ~~> X" $
      \x -> Set.member (x :: P.PropFormula Int) (rewriteOnceAtRoot $ P.OR x x)
    prop "OR (NOT X) (NOT Y) ~~> NOT (AND X Y)" $
      \x y ->
        Set.member
          (P.NOT (P.AND x y) :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.OR (P.NOT x) (P.NOT y))
    prop "OR (AND W X) (AND W Z) ~~> AND W (OR X Z)" $
      \w x z ->
        Set.member
          (P.AND w (P.OR x z) :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.OR (P.AND w x) (P.AND w z))
    prop "OR (AND W X) (AND Y W) ~~> AND W (OR X Y)" $
      \w x y ->
        Set.member
          (P.AND w (P.OR x y) :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.OR (P.AND w x) (P.AND y w))
    prop "OR (AND W X) (AND X Z) ~~> AND X (OR W Z)" $
      \w x z ->
        Set.member
          (P.AND x (P.OR w z) :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.OR (P.AND w x) (P.AND x z))
    prop "OR (AND W X) (AND Y X) ~~> AND X (OR W Y)" $
      \w x y ->
        Set.member
          (P.AND x (P.OR w y) :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.OR (P.AND w x) (P.AND y x))
    prop "OR X (NOT X) ~~> TRUE" $
      \x ->
        Set.member
          (P.TRUE :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.OR x (P.NOT x))
    prop "OR (NOT X) X ~~> TRUE" $
      \x ->
        Set.member
          (P.TRUE :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.OR (P.NOT x) x)
    prop "AND X Y ~~> AND Y X" $
      \x y ->
        Set.member
          (P.AND y x :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.AND x y)
    prop "AND X (AND Y Z) ~~> AND (AND X Y) Z" $
      \x y z ->
        Set.member
          (P.AND x (P.AND y z) :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.AND (P.AND x y) z)
    prop "AND (AND X Y) Z ~~> AND X (AND Y Z)" $
      \x y z ->
        Set.member
          (P.AND (P.AND x y) z :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.AND x (P.AND y z))
    prop "AND TRUE X ~~> X" $
      \x ->
        Set.member
          (x :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.AND P.TRUE x)
    prop "AND X TRUE ~~> X" $
      \x ->
        Set.member
          (x :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.AND x P.TRUE)
    prop "AND FALSE X ~~> FALSE" $
      \x ->
        Set.member
          (P.FALSE :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.AND P.FALSE x)
    prop "AND X FALSE ~~> FALSE" $
      \x ->
        Set.member
          (P.FALSE :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.AND x P.FALSE)
    prop "AND X X ~~> X" $
      \x ->
        Set.member
          (x :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.AND x x)
    prop "AND (NOT X) (NOT Y) ~~> NOT (OR X Y)" $
      \x y ->
        Set.member
          (P.NOT (P.OR x y) :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.AND (P.NOT x) (P.NOT y))
    prop "AND (OR W X) (OR W Z) ~~> OR W (AND X Z)" $
      \w x z ->
        Set.member
          (P.OR w (P.AND x z) :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.AND (P.OR w x) (P.OR w z))
    prop "AND (OR W X) (OR Y W) ~~> OR W (AND X Y)" $
      \w x y ->
        Set.member
          (P.OR w (P.AND x y) :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.AND (P.OR w x) (P.OR y w))
    prop "AND (OR W X) (OR X Z) ~~> OR X (AND W Z)" $
      \w x z ->
        Set.member
          (P.OR x (P.AND w z) :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.AND (P.OR w x) (P.OR x z))
    prop "AND (OR W X) (OR Y X) ~~> OR X (AND W Y)" $
      \w x y ->
        Set.member
          (P.OR x (P.AND w y) :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.AND (P.OR w x) (P.OR y x))
    prop "AND X (NOT X) ~~> FALSE" $
      \x ->
        Set.member
          (P.FALSE :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.AND x (P.NOT x))
    prop "AND (NOT X) X ~~> FALSE" $
      \x ->
        Set.member
          (P.FALSE :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.AND (P.NOT x) x)
    prop "IMPLIES X Y ~~> OR (NOT X) Y" $
      \x y ->
        Set.member
          (P.OR (P.NOT x) y :: P.PropFormula Int)
          (rewriteOnceAtRoot $ P.IMPLIES x y)

    prop "do not ever increase complexity" $
      \x ->
        P.complexity x <= 20 -- just to ensure the formulas stay small
          ==> all
            (<= P.complexity x)
            (Set.map P.complexity (rewriteOnce (x :: P.PropFormula Int)))
