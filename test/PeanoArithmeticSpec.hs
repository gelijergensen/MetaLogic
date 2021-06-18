{-# LANGUAGE FlexibleInstances #-}

module PeanoArithmeticSpec where

import Control.Monad (liftM2)
import qualified Data.Set as Set
import qualified LogicSystem as LS
import qualified PeanoArithmetic as P
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

rewriteOnceAtRoot :: Ord a => P.PeanoFormula a -> Set.Set (P.PeanoFormula a)
rewriteOnceAtRoot = LS.rewriteOnceAtRoot P.PeanoArithmetic

rewriteOnce :: Ord a => P.PeanoFormula a -> Set.Set (P.PeanoFormula a)
rewriteOnce = LS.rewriteOnce P.PeanoArithmetic

instance Arbitrary a => Arbitrary (LS.Formula P.PeanoArithmetic a) where
  arbitrary = sized formula'
    where
      formula' 0 = oneof [pure P.ZERO, P.VAR <$> arbitrary]
      formula' n
        | n > 0 =
          frequency
            [ (1, pure P.ZERO),
              (1, P.VAR <$> arbitrary),
              (2, P.S <$> formula' halfN),
              (2, liftM2 P.PLUS (formula' halfN) (formula' halfN)),
              (2, liftM2 P.TIMES (formula' halfN) (formula' halfN))
            ]
        | otherwise = error "negative size"
        where
          halfN = n `div` 2

spec :: Spec
spec = do
  describe "PeanoArithmetic rewrite rules" $ do
    prop "PLUS x ZERO ~~> x" $
      \x ->
        Set.member
          (x :: P.PeanoFormula Int)
          (rewriteOnceAtRoot $ P.PLUS x P.ZERO)
    prop "PLUS ZERO x ~~> x" $
      \x ->
        Set.member
          (x :: P.PeanoFormula Int)
          (rewriteOnceAtRoot $ P.PLUS P.ZERO x)
    prop "PLUS x (S y) ~~> S (PLUS x y)" $
      \x y ->
        Set.member
          (P.S (P.PLUS x y) :: P.PeanoFormula Int)
          (rewriteOnceAtRoot $ P.PLUS x (P.S y))
    prop "PLUS (S x) y ~~> S (PLUS x y)" $
      \x y ->
        Set.member
          (P.S (P.PLUS x y) :: P.PeanoFormula Int)
          (rewriteOnceAtRoot $ P.PLUS (P.S x) y)
    prop "TIMES x ZERO ~~> ZERO" $
      \x ->
        Set.member
          (P.ZERO :: P.PeanoFormula Int)
          (rewriteOnceAtRoot $ P.TIMES x P.ZERO)
    prop "TIMES ZERO x ~~> ZERO" $
      \x ->
        Set.member
          (P.ZERO :: P.PeanoFormula Int)
          (rewriteOnceAtRoot $ P.TIMES P.ZERO x)
    prop "TIMES x (S y) ~~> PLUS x (TIMES x y)" $
      \x y ->
        Set.member
          (P.PLUS x (P.TIMES x y) :: P.PeanoFormula Int)
          (rewriteOnceAtRoot $ P.TIMES x (P.S y))
    prop "TIMES (S x) y ~~> PLUS x (TIMES x y)" $
      \x y ->
        Set.member
          (P.PLUS x (P.TIMES x y) :: P.PeanoFormula Int)
          (rewriteOnceAtRoot $ P.TIMES (P.S x) y)
    prop "y < x => PLUS x y ~~> PLUS y x" $
      \x y ->
        Set.member
          (if x <= y then P.PLUS x y else P.PLUS y x :: P.PeanoFormula Int)
          (rewriteOnceAtRoot $ P.PLUS x y)
    prop "PLUS (PLUS x y) z ~~> PLUS x (PLUS y z)" $
      \x y z ->
        Set.member
          (P.PLUS x (P.PLUS y z) :: P.PeanoFormula Int)
          (rewriteOnceAtRoot $ P.PLUS (P.PLUS x y) z)
    prop "y < x => TIMES x y ~~> TIMES y x" $
      \x y ->
        Set.member
          (if x <= y then P.TIMES x y else P.TIMES y x :: P.PeanoFormula Int)
          (rewriteOnceAtRoot $ P.TIMES x y)
    prop "TIMES (TIMES x y) z ~~> TIMES x (TIMES y z)" $
      \x y z ->
        Set.member
          (P.TIMES x (P.TIMES y z) :: P.PeanoFormula Int)
          (rewriteOnceAtRoot $ P.TIMES (P.TIMES x y) z)

    prop "do not ever increase complexity" $
      \x ->
        P.complexity x <= 20 -- just to ensure the formulas stay small
          ==> all
            (<= P.complexity x)
            (Set.map P.complexity (rewriteOnce (x :: P.PeanoFormula Int)))
