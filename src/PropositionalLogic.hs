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

type PropFormula = LS.Formula PropositionalLogic

instance LS.LogicSystem PropositionalLogic where
  data Formula PropositionalLogic a
    = TRUE
    | FALSE
    | VAR a
    | NOT (PropFormula a)
    | OR (PropFormula a) (PropFormula a)
    | AND (PropFormula a) (PropFormula a)
    | IMPLIES (PropFormula a) (PropFormula a)
    deriving (Eq, Ord, Show)
  newtype Rule PropositionalLogic a = Rule
    { runRule ::
        PropFormula a ->
        PropFormula a
    }

  -- subformulas = undefined
  rewriteRules = undefined
  runRule = runRule

---------- Rewrite Rules ----------

-- _notTrue (NOT (PropFormula _ TRUE)) = PropFormula ""

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
        ([PropFormula a] -> Either Interpreter.InterpretError (PropFormula a))
  }

instance Ord a => Interpreter.Interpreter (PropositionalLogicInterpreter a) a where
  type LogicSystem (PropositionalLogicInterpreter a) = PropositionalLogic
  operatorByID = operatorByID
  variableFromID = const VAR

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
      Either Interpreter.InterpretError (PropFormula a),
      [a]
    )
  ] ->
  PropositionalLogicInterpreter a
propositionalLogicInterpreterFromNames =
  PropositionalLogicInterpreter
    . Map.fromList
    . concatMap (\(op, ns) -> map (,op) ns)

true :: [PropFormula a] -> Either Interpreter.InterpretError (PropFormula a)
true = Interpreter.makeConstant TRUE "PropositionalLogic.TRUE"

false :: [PropFormula a] -> Either Interpreter.InterpretError (PropFormula a)
false = Interpreter.makeConstant FALSE "PropositionalLogic.FALSE"

not :: [PropFormula a] -> Either Interpreter.InterpretError (PropFormula a)
not = Interpreter.makeUnary NOT "PropositionalLogic.NOT"

or :: [PropFormula a] -> Either Interpreter.InterpretError (PropFormula a)
or = Interpreter.makeBinary OR "PropositionalLogic.OR"

and :: [PropFormula a] -> Either Interpreter.InterpretError (PropFormula a)
and = Interpreter.makeBinary AND "PropositionalLogic.AND"

implies :: [PropFormula a] -> Either Interpreter.InterpretError (PropFormula a)
implies = Interpreter.makeBinary IMPLIES "PropositionalLogic.IMPLIES"
