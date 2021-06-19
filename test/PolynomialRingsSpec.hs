{-# LANGUAGE FlexibleInstances #-}

module PolynomialRingsSpec where

import Control.Monad (liftM2, replicateM)
import qualified Data.MultiSet as MultiSet
import qualified Data.Set as Set
import qualified LogicSystem as LS
import qualified PolynomialRings as P
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

rewriteOnceAtRoot :: Ord a => P.Polynomial a -> Set.Set (P.Polynomial a)
rewriteOnceAtRoot = LS.rewriteOnceAtRoot P.PolynomialRings

rewriteOnce :: Ord a => P.Polynomial a -> Set.Set (P.Polynomial a)
rewriteOnce = LS.rewriteOnce P.PolynomialRings

instance (Ord a, Arbitrary a) => Arbitrary (LS.Formula P.PolynomialRings a) where
  arbitrary = sized formula'
    where
      formula' 0 =
        oneof
          [ pure P.ZERO,
            pure P.ONE,
            P.CONSTANT <$> arbitrary,
            P.VARIABLE <$> arbitrary
          ]
      formula' n
        | n > 0 =
          frequency
            [ (1, pure P.ZERO),
              (1, pure P.ONE),
              (1, P.CONSTANT <$> arbitrary),
              (1, P.VARIABLE <$> arbitrary),
              (2, P.NEG <$> formula' halfN),
              ( 2,
                P.PLUS . MultiSet.fromList
                  <$> replicateM (min n 3) (formula' halfN)
              ),
              ( 2,
                P.TIMES . MultiSet.fromList
                  <$> replicateM (min n 3) (formula' halfN)
              )
            ]
        | otherwise = error "negative size"
        where
          halfN = n `div` 2

spec :: Spec
spec = do
  describe "PolynomialRings rewrite rules" $ do
    prop "NEG (NEG X) ~~> X" $
      \x ->
        Set.member
          (x :: P.Polynomial Int)
          (rewriteOnceAtRoot $ P.NEG (P.NEG x))
    it "PLUS [] ~~> ZERO" $ do
      Set.member
        (P.ZERO :: P.Polynomial ())
        (rewriteOnceAtRoot $ P.PLUS (MultiSet.fromList []))
    prop "PLUS (xs ++ ZERO : ys) ~~> PLUS (xs ++ ys)" $
      \xs ys ->
        Set.member
          ( P.PLUS
              ( MultiSet.fromList
                  ( filter
                      (/= (P.ZERO :: P.Polynomial Int))
                      (xs ++ ys)
                  )
              )
          )
          (rewriteOnceAtRoot $ P.PLUS (MultiSet.fromList (xs ++ P.ZERO : ys)))
    prop "PLUS x (NEG x) y ~~> PLUS y" $
      \x y ->
        Set.member
          (P.PLUS (MultiSet.fromList [y]) :: P.Polynomial Int)
          (rewriteOnceAtRoot $ P.PLUS (MultiSet.fromList [x, P.NEG x, y]))
    prop "PLUS x ~~> x" $
      \x ->
        Set.member
          (x :: P.Polynomial Int)
          (rewriteOnceAtRoot $ P.PLUS (MultiSet.fromList [x]))
    prop "PLUS (xs ++ PLUS z : ys) ~~> PLUS (xs ++ z : ys)" $
      \xs ys z ->
        Set.member
          ( P.PLUS
              (MultiSet.fromList (z : filter (not . P.isPlus) (xs ++ ys))) ::
              P.Polynomial Int
          )
          ( rewriteOnceAtRoot $
              P.PLUS $
                MultiSet.fromList
                  ( P.PLUS (MultiSet.fromList [z]) :
                    filter (not . P.isPlus) (xs ++ ys)
                  )
          )
    it "TIMES [] ~~> ONE" $ do
      Set.member
        (P.ONE :: P.Polynomial ())
        (rewriteOnceAtRoot $ P.TIMES (MultiSet.fromList []))
    prop "TIMES (xs ++ ONE : ys) ~~> TIMES (xs ++ ys)" $
      \xs ys ->
        Set.member
          ( P.TIMES
              ( MultiSet.fromList
                  ( filter
                      (/= (P.ONE :: P.Polynomial Int))
                      (xs ++ ys)
                  )
              )
          )
          (rewriteOnceAtRoot $ P.TIMES (MultiSet.fromList (xs ++ P.ONE : ys)))
    prop "TIMES (xs ++ ZERO : ys) ~~> ZERO" $
      \xs ys ->
        Set.member
          (P.ZERO :: P.Polynomial Int)
          (rewriteOnceAtRoot $ P.TIMES (MultiSet.fromList (xs ++ P.ZERO : ys)))
    prop "TIMES correctly pulls NEG to the front (iff odd number)" $
      \xs ->
        let unNeg (P.NEG x) = x
            unNeg x = x
            res = P.TIMES $ MultiSet.fromList $ map unNeg xs
         in Set.member
              ( ( if even . MultiSet.size . MultiSet.filter P.isNeg $
                    MultiSet.fromList xs
                    then res
                    else P.NEG res
                ) ::
                  P.Polynomial Int
              )
              (rewriteOnceAtRoot $ P.TIMES $ MultiSet.fromList xs)
    prop "TIMES x ~~> x" $
      \x ->
        Set.member
          (x :: P.Polynomial Int)
          (rewriteOnceAtRoot $ P.TIMES (MultiSet.fromList [x]))
    prop "TIMES (xs ++ TIMES z : ys) ~~> TIMES (xs ++ z : ys)" $
      \xs ys z ->
        Set.member
          ( P.TIMES
              (MultiSet.fromList (z : filter (not . P.isTimes) (xs ++ ys))) ::
              P.Polynomial Int
          )
          ( rewriteOnceAtRoot $
              P.TIMES $
                MultiSet.fromList
                  ( P.TIMES (MultiSet.fromList [z]) :
                    filter (not . P.isTimes) (xs ++ ys)
                  )
          )
    prop "TIMES distributes over PLUS" $
      \x1 x2 y1 y2 z ->
        (not . P.isPlus $ z)
          ==> Set.member
            ( P.PLUS $
                MultiSet.fromList
                  [ P.TIMES $ MultiSet.fromList [x1, y1, z],
                    P.TIMES $ MultiSet.fromList [x1, y2, z],
                    P.TIMES $ MultiSet.fromList [x2, y1, z],
                    P.TIMES $ MultiSet.fromList [x2, y2, z]
                  ] ::
                P.Polynomial Int
            )
            ( rewriteOnceAtRoot $
                P.TIMES $
                  MultiSet.fromList
                    [ P.PLUS $ MultiSet.fromList [x1, x2],
                      P.PLUS $ MultiSet.fromList [y1, y2],
                      z
                    ]
            )
