{-# LANGUAGE MultiParamTypeClasses #-}

module PropositionalLogic where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified LogicSystem as LS

newtype PropositionalLogic a = PropositionalLogic a deriving (Eq, Show)

data PropositionalOperator a
  = TRUE a
  | FALSE a
  | NOT a
  | IMPLIES a
  | AND a
  | OR a
  deriving (Eq, Show)

newtype PropositionalLogicInterpreter a = PropositionalLogicInterpreter
  { namedOperators :: Map.Map a (PropositionalOperator a)
  }
  deriving (Eq, Show)

instance LS.LogicSystem PropositionalLogic (PropositionalOperator a) where
  operators = Set.fromList [TRUE, FALSE, NOT, IMPLIES, AND, OR]
  inferenceRules = undefined

instance LS.Operator (PropositionalOperator a) where
  arity (TRUE _) = LS.Constant
  arity (FALSE _) = LS.Constant
  arity (NOT _) = LS.Unary
  arity (IMPLIES _) = LS.Binary
  arity (AND _) = LS.Variadic
  arity (OR _) = LS.Variadic

defaultPropositionalLogicInterpreter :: PropositionalLogicInterpreter String
defaultPropositionalLogicInterpreter =
  propositionalLogicInterpreterFromNames
    [ (TRUE, ["TRUE", "True", "true"]),
      (FALSE, ["FALSE", "False", "false"]),
      (NOT, ["NOT", "Not", "not", "~", "!"]),
      (IMPLIES, ["IMPLIES", "Implies", "implies", "->", "=>"]),
      (AND, ["AND", "And", "and", "&&"]),
      (OR, ["OR", "Or", "or", "||"])
    ]

propositionalLogicInterpreterFromNames ::
  Ord a => [(a -> PropositionalOperator a, [a])] -> PropositionalLogicInterpreter a
propositionalLogicInterpreterFromNames =
  PropositionalLogicInterpreter
    . Map.fromList
    . concatMap (\(op, ns) -> map (\n -> (n, op n)) ns)
