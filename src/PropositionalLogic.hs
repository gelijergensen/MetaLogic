{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module PropositionalLogic where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Interpreter
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
    deriving (Eq, Show)
  arity TRUE = LS.Constant
  arity FALSE = LS.Constant
  arity NOT = LS.Unary
  arity IMPLIES = LS.Binary
  arity AND = LS.Variadic
  arity OR = LS.Variadic
  inferenceRules = undefined
  identifier = identifier
  subformulas = subformulas

newtype PropositionalLogicInterpreter a = PropositionalLogicInterpreter
  { operatorByID :: Map.Map a (LS.Operator PropositionalLogic)
  }
  deriving (Eq, Show)

instance Ord a => Interpreter.Interpreter (PropositionalLogicInterpreter a) a where
  type LogicSystem (PropositionalLogicInterpreter a) = PropositionalLogic
  operatorByID = operatorByID
  formula = const PropFormula

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
  Ord a => [(LS.Operator PropositionalLogic, [a])] -> PropositionalLogicInterpreter a
propositionalLogicInterpreterFromNames =
  PropositionalLogicInterpreter
    . Map.fromList
    . concatMap (\(op, ns) -> map (,op) ns)
