{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module PropositionalLogic where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
-- import qualified Interpreter
import qualified LogicSystem as LS

data PropositionalLogic = PropositionalLogic deriving (Eq, Show)

instance LS.LogicSystem PropositionalLogic where
  data Operator PropositionalLogic
    = TRUE
    | FALSE
    | NOT
    | IMPLIES
    | AND
    | OR
    deriving (Eq, Ord, Show)
  data Formula PropositionalLogic a = PropFormula
    { identifier :: a,
      operator :: LS.Operator PropositionalLogic,
      subformulas :: Seq.Seq (LS.Formula PropositionalLogic a)
    }
  arity TRUE = LS.Constant
  arity FALSE = LS.Constant
  arity NOT = LS.Unary
  arity IMPLIES = LS.Binary
  arity AND = LS.Variadic
  arity OR = LS.Variadic
  inferenceRules = undefined
  identifier = identifier
  subformulas = subformulas
  operators = const $ Set.fromList [TRUE, FALSE, NOT, IMPLIES, AND, OR]

-- newtype PropositionalLogicInterpreter a = PropositionalLogicInterpreter
--   { operatorKinds :: Map.Map a PropositionalOperator
--   }
--   deriving (Eq, Show)

-- instance
--   Ord a =>
--   Interpreter.Interpreter
--     PropositionalOperator
--     a
--     (PropositionalLogicInterpreter a)
--   where
--   operatorKinds = operatorKinds

-- defaultPropositionalLogicInterpreter :: PropositionalLogicInterpreter String
-- defaultPropositionalLogicInterpreter =
--   propositionalLogicInterpreterFromNames
--     [ (TRUE, ["TRUE", "True", "true"]),
--       (FALSE, ["FALSE", "False", "false"]),
--       (NOT, ["NOT", "Not", "not", "~", "!"]),
--       (IMPLIES, ["IMPLIES", "Implies", "implies", "->", "=>"]),
--       (AND, ["AND", "And", "and", "&&"]),
--       (OR, ["OR", "Or", "or", "||"])
--     ]

-- propositionalLogicInterpreterFromNames ::
--   Ord a => [(PropositionalOperator, [a])] -> PropositionalLogicInterpreter a
-- propositionalLogicInterpreterFromNames =
--   PropositionalLogicInterpreter
--     . Map.fromList
--     . concatMap (\(op, ns) -> map (,op) ns)
