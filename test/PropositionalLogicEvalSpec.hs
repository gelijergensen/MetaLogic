module PropositionalLogicEvalSpec where

import qualified Assignment
import Control.Monad (liftM2)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified PropositionalLogicEval as Prop
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

newtype VariableName = VariableName Char deriving (Eq, Ord, Show)

instance Arbitrary VariableName where
  arbitrary = oneof $ map (pure . VariableName) ['A' .. 'Z']

instance CoArbitrary VariableName where
  coarbitrary (VariableName c) = variant $ Char.ord c - Char.ord 'A'

instance (Arbitrary a, Ord a) => Arbitrary (Assignment.Assignment a) where
  arbitrary = Assignment.Assignment . Map.fromList <$> sized keyVals
    where
      keyVals n =
        vector n >>= mapM (\a -> oneof [pure (a, True), pure (a, False)])

instance CoArbitrary a => CoArbitrary (Assignment.Assignment a) where
  coarbitrary (Assignment.Assignment m) = coarbitrary (Map.toList m)

instance Arbitrary a => Arbitrary (Prop.PropFormula a) where
  arbitrary = sized formula'
    where
      formula' 0 = oneof [pure Prop.Truth, pure Prop.Fiction]
      formula' n
        | n > 0 =
          frequency
            [ (1, pure Prop.Truth),
              (1, pure Prop.Fiction),
              (1, Prop.Variable <$> arbitrary),
              (1, Prop.Not <$> arbitrary),
              (1, liftM2 Prop.And arbitrary arbitrary),
              (1, liftM2 Prop.Or arbitrary arbitrary)
            ]

instance CoArbitrary a => CoArbitrary (Prop.PropFormula a) where
  coarbitrary Prop.Truth = variant 0
  coarbitrary Prop.Fiction = variant 1
  coarbitrary (Prop.Variable a) = variant 2 . coarbitrary a
  coarbitrary (Prop.Not x) = variant 3 . coarbitrary x
  coarbitrary (Prop.And l r) = variant 4 . coarbitrary l . coarbitrary r
  coarbitrary (Prop.Or l r) = variant 5 . coarbitrary l . coarbitrary r

spec :: Spec
spec = do
  describe "Project runs" $ do
    prop "x == x" $ \x -> (x :: Int) == x

  describe "Prop.simplify" $ do
    prop "Prop.simplify asgns . Prop.simplify asgns == Prop.simplify asgns" $
      \asgns xs ->
        ( Prop.simplify asgns
            . (\(Prop.PartialFormula x) -> x)
            . Prop.simplify asgns
            $ xs
        )
          == Prop.simplify
            (asgns :: Assignment.Assignment VariableName)
            (xs :: Prop.PropFormula VariableName)

  describe "Prop.simplify/Prop.evaluate" $ do
    prop "Prop.evaluate asgns . Prop.simplify == Prop.evaluate asgns" $
      \asgns xs ->
        ( Prop.evaluate asgns
            . (\(Prop.PartialFormula x) -> x)
            . Prop.simplify asgns
            $ xs
        )
          == Prop.evaluate
            (asgns :: Assignment.Assignment VariableName)
            (xs :: Prop.PropFormula VariableName)

  describe "Prop.simplify/Prop.variables" $ do
    prop
      "Prop.variables (Prop.simplify asgns xs) `isSubsetOf` Prop.variables xs \\ Map.keysSet asgns"
      $ \asgns xs ->
        ( Prop.variables
            . (\(Prop.PartialFormula x) -> x)
            . Prop.simplify asgns
            $ xs
        )
          `Set.isSubsetOf` Set.difference
            (Prop.variables (xs :: Prop.PropFormula VariableName))
            (Map.keysSet . (\(Assignment.Assignment x) -> x) $ asgns)
