{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module PropositionalLogic where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Interpreter
import qualified LogicSystem as LS
import Prelude hiding (and, not, or)

data PropositionalLogic = PropositionalLogic deriving (Eq, Show)

type PropOperator = LS.Operator PropositionalLogic

type PropFormula = LS.Formula PropositionalLogic

instance LS.LogicSystem PropositionalLogic where
  data Operator PropositionalLogic a
    = TRUE
    | FALSE
    | NOT (PropFormula a)
    | OR (PropFormula a) (PropFormula a)
    | AND (PropFormula a) (PropFormula a)
    | IMPLIES (PropFormula a) (PropFormula a)
    deriving (Eq, Ord, Show)
  data Formula PropositionalLogic a = PropFormula
    { identifier :: a,
      formula :: PropOperator a
    }
    deriving (Eq, Ord, Show)
  newtype Rule PropositionalLogic a = Rule
    { rewrite ::
        PropFormula a ->
        PropFormula a
    }

  -- arity TRUE = LS.Constant
  -- arity FALSE = LS.Constant
  -- arity (NOT _) = LS.Unary
  -- arity (OR _ _) = LS.Binary
  -- arity (AND _ _) = LS.Binary
  -- arity (IMPLIES _ _) = LS.Binary
  identifier = identifier

  -- subformulas = subformulas
  rewriteRules = undefined
  rewrite = rewrite

---------- Rewrite Rules ----------

-- rewriteRules :: [LS.Rule PropositionalLogic a]
-- rewriteRules = map Rule []

-- _notTrue x = undefined `onOperator` NOT

-- _rewriteImplies x = case operator x of
--   IMPLIES -> undefined
--   _ -> x

-- onOperator ::
--   (PropFormula a -> PropFormula a) ->
--   PropOperator ->
--   PropFormula a ->
--   PropFormula a
-- onOperator rw op x = if operator x == op then rw x else x

---------- Interpreters ----------
newtype PropositionalLogicInterpreter a = PropositionalLogicInterpreter
  { operatorByID ::
      Map.Map
        a
        ([PropFormula a] -> Either Interpreter.InterpretError (PropOperator a))
  }

instance Ord a => Interpreter.Interpreter (PropositionalLogicInterpreter a) a where
  type LogicSystem (PropositionalLogicInterpreter a) = PropositionalLogic
  operatorByID = operatorByID
  formula = const PropFormula

defaultPropositionalLogicInterpreter :: PropositionalLogicInterpreter String
defaultPropositionalLogicInterpreter =
  propositionalLogicInterpreterFromNames
    [ (true, ["TRUE", "True", "true"]),
      (false, ["FALSE", "False", "false"]),
      (not, ["NOT", "Not", "not", "~", "!"]),
      (or, ["OR", "Or", "or", "||"]),
      (and, ["AND", "And", "and", "&&"]),
      (implies, ["IMPLIES", "Implies", "implies", "->", "=>"])
    ]

propositionalLogicInterpreterFromNames ::
  Ord a =>
  [ ( [PropFormula a] ->
      Either Interpreter.InterpretError (PropOperator a),
      [a]
    )
  ] ->
  PropositionalLogicInterpreter a
propositionalLogicInterpreterFromNames =
  PropositionalLogicInterpreter
    . Map.fromList
    . concatMap (\(op, ns) -> map (,op) ns)

true :: [PropFormula a] -> Either Interpreter.InterpretError (PropOperator a)
true = Interpreter.makeConstant TRUE "PropositionalLogic.TRUE"

false :: [PropFormula a] -> Either Interpreter.InterpretError (PropOperator a)
false = Interpreter.makeConstant FALSE "PropositionalLogic.FALSE"

not :: [PropFormula a] -> Either Interpreter.InterpretError (PropOperator a)
not = Interpreter.makeUnary NOT "PropositionalLogic.NOT"

or :: [PropFormula a] -> Either Interpreter.InterpretError (PropOperator a)
or = Interpreter.makeBinary OR "PropositionalLogic.OR"

and :: [PropFormula a] -> Either Interpreter.InterpretError (PropOperator a)
and = Interpreter.makeBinary AND "PropositionalLogic.AND"

implies :: [PropFormula a] -> Either Interpreter.InterpretError (PropOperator a)
implies = Interpreter.makeBinary IMPLIES "PropositionalLogic.IMPLIES"
