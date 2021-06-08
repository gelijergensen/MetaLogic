module PropositionalLogic where

import Control.Applicative (liftA2)
import qualified Data.Map as Map
import qualified OperatorSymbols as OS

type PropositionalSymbol a = OS.Symbol String a

data PropFormula a
  = Truth
  | Fiction
  | Variable a
  | Not (PropFormula a)
  | And (PropFormula a) (PropFormula a)
  | Or (PropFormula a) (PropFormula a)
  deriving (Eq, Show)

newtype Assignment a = Assignment (Map.Map a Bool) deriving (Eq, Show)

type AssignError = String

evaluate ::
  (Ord a, Show a) => Assignment a -> PropFormula a -> Either AssignError Bool
evaluate _ Truth = Right True
evaluate _ Fiction = Right False
evaluate asgns (Variable a) = assign asgns a
evaluate asgns (Not a) = not <$> evaluate asgns a
evaluate asgns (And l r) = case (evaluate asgns l, evaluate asgns r) of
  (Right False, _) -> Right False
  (_, Right False) -> Right False
  (l', r') -> liftA2 (&&) l' r'
evaluate asgns (Or l r) = case (evaluate asgns l, evaluate asgns r) of
  (Right True, _) -> Right True
  (_, Right True) -> Right True
  (l', r') -> liftA2 (&&) l' r'

assign :: (Ord a, Show a) => Assignment a -> a -> Either AssignError Bool
assign (Assignment as) a = case Map.lookup a as of
  Just x -> Right x
  Nothing -> Left $ "AssignError: Value of " ++ show a ++ " not given"

emptyAssignment :: Assignment a
emptyAssignment = Assignment Map.empty

-- fiction :: PropositionalSymbol a
-- fiction = OS.Symbol "FICTION" OS.Constant

-- truth :: PropositionalSymbol a
-- truth = OS.Symbol "TRUTH" OS.Constant

-- not :: a -> PropositionalSymbol a
-- not a = OS.Symbol "NOT" $ OS.Unary a

-- and :: a -> a -> PropositionalSymbol a
-- and l r = OS.Symbol "AND" $ OS.Binary l r

-- or :: a -> a -> PropositionalSymbol a
-- or l r = OS.Symbol "OR" $ OS.Binary l r

-- implies :: a -> a -> PropositionalSymbol a
-- implies l r = OS.Symbol "IMPLIES" $ OS.Binary l r
