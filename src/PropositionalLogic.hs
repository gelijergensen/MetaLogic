module PropositionalLogic where

import qualified Assignment
import Control.Applicative (liftA2)
import qualified Data.Bifunctor as Bifunc
import qualified Data.Set as Set
import qualified OperatorSymbols as OS

data PropFormula a
  = Truth
  | Fiction
  | Variable a
  | Not (PropFormula a)
  | And (PropFormula a) (PropFormula a)
  | Or (PropFormula a) (PropFormula a)
  deriving (Eq, Show)

newtype PartialFormula a
  = PartialFormula (PropFormula a)
  deriving (Eq, Show)

-- instance Functor (PartialFormula a) where
--   fmap f (Value v) = Value $ f v
--   fmap _ (PartialFormula x) = PartialFormula x

-- instance Applicative (PartialFormula a) where
--   pure = Value
--   Value f <*> Value a = Value $ f a

evaluate ::
  (Ord a, Show a) =>
  Assignment.Assignment a ->
  PropFormula a ->
  Either Assignment.AssignError Bool
evaluate _ Truth = Right True
evaluate _ Fiction = Right False
evaluate asgns (Variable a) = Assignment.assign asgns a
evaluate asgns (Not a) = not <$> evaluate asgns a
evaluate asgns (And l r) = case (evaluate asgns l, evaluate asgns r) of
  (Right False, _) -> Right False
  (_, Right False) -> Right False
  (l', r') -> liftA2 (&&) l' r'
evaluate asgns (Or l r) = case (evaluate asgns l, evaluate asgns r) of
  (Right True, _) -> Right True
  (_, Right True) -> Right True
  (l', r') -> liftA2 (&&) l' r'

simplify ::
  (Ord a, Show a) =>
  Assignment.Assignment a ->
  PropFormula a ->
  PartialFormula a
simplify _ Truth = PartialFormula Truth
simplify _ Fiction = PartialFormula Fiction
simplify asgns v@(Variable a) = case Assignment.assign asgns a of
  (Right True) -> PartialFormula Truth
  (Right False) -> PartialFormula Fiction
  (Left _) -> PartialFormula v
simplify asgns (Not a) = case simplify asgns a of
  PartialFormula Truth -> PartialFormula Fiction
  PartialFormula Fiction -> PartialFormula Truth
  PartialFormula x -> PartialFormula $ Not x
simplify asgns (And l r) = case (simplify asgns l, simplify asgns r) of
  (PartialFormula Truth, x) -> x
  (y, PartialFormula Truth) -> y
  (PartialFormula Fiction, _) -> PartialFormula Fiction
  (_, PartialFormula Fiction) -> PartialFormula Fiction
  (PartialFormula l', PartialFormula r') -> PartialFormula (And l' r')
simplify asgns (Or l r) = case (simplify asgns l, simplify asgns r) of
  (PartialFormula Truth, _) -> PartialFormula Truth
  (_, PartialFormula Truth) -> PartialFormula Truth
  (PartialFormula Fiction, x) -> x
  (y, PartialFormula Fiction) -> y
  (PartialFormula l', PartialFormula r') -> PartialFormula (Or l' r')

variables :: Ord a => PropFormula a -> Set.Set a
variables Truth = Set.empty
variables Fiction = Set.empty
variables (Variable a) = Set.singleton a
variables (Not x) = variables x
variables (And l r) = Set.union (variables l) (variables r)
variables (Or l r) = Set.union (variables l) (variables r)
