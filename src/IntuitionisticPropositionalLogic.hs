{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module IntuitionisticPropositionalLogic where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified ErrorHandling as EH
import qualified Interpreter
import qualified LogicSystem as LS
import qualified RApplicative as RApp
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
  type RuleConstraint PropositionalLogic a = Eq a

  mapFormula = mapFormula
  rewriteRules =
    const . map Rule $
      []
  runRule = runRule

-- todo for large formulas, e.g.
-- IMPLIES (OR (IMPLIES (AND (AND (IMPLIES TRUE (VAR (-31))) (NOT (VAR (-32)))) (NOT (IMPLIES (VAR 14) TRUE))) (IMPLIES (OR (OR FALSE (VAR 13)) TRUE) TRUE)) (AND (AND (IMPLIES (VAR 1) (AND TRUE TRUE)) FALSE) FALSE)) (IMPLIES (NOT (AND (IMPLIES (NOT (VAR (-4))) (NOT TRUE)) (AND (OR FALSE (VAR 12)) TRUE))) (AND (AND (OR (OR TRUE (VAR (-22))) (IMPLIES (VAR (-23)) (VAR (-24)))) (AND (OR (VAR (-13)) (VAR 26)) (NOT FALSE))) (AND TRUE (IMPLIES (NOT TRUE) (IMPLIES (VAR (-6)) TRUE)))))
-- this can take a long time, due to combinatorial blowup
mapFormula ::
  Ord (PropFormula a) =>
  (PropFormula a -> LS.Frontier (PropFormula a)) ->
  PropFormula a ->
  LS.Frontier (PropFormula a)
mapFormula f term@(IMPLIES x y) =
  f term <> RApp.liftA2 IMPLIES (mapFormula f x) (mapFormula f y)
mapFormula f term@(AND x y) =
  f term <> RApp.liftA2 AND (mapFormula f x) (mapFormula f y)
mapFormula f term@(OR x y) =
  f term <> RApp.liftA2 OR (mapFormula f x) (mapFormula f y)
mapFormula f term@(NOT x) = f term <> RApp.fmap NOT (mapFormula f x)
mapFormula f term = f term

-- Approximate measure of complexity of formula
-- No rewrite rule should increase this
complexity :: PropFormula a -> Int
complexity TRUE = 0
complexity FALSE = 0
complexity (VAR _) = 1
complexity (NOT x) = 1 + complexity x
complexity (OR x y) = 1 + complexity x + complexity y
complexity (AND x y) = 1 + complexity x + complexity y
-- special case, as IMPLIES x y is defined to be OR (NOT x) y
complexity (IMPLIES x y) = 2 + complexity x + complexity y

---------- Rewrite Rules ----------

---------- Interpreters ----------
newtype PropositionalLogicInterpreter a = PropositionalLogicInterpreter
  { operatorByID ::
      Map.Map
        a
        ([PropFormula a] -> Either EH.Error (PropFormula a))
  }

instance Ord a => Interpreter.Interpreter (PropositionalLogicInterpreter a) a a where
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
      Either EH.Error (PropFormula a),
      [a]
    )
  ] ->
  PropositionalLogicInterpreter a
propositionalLogicInterpreterFromNames =
  PropositionalLogicInterpreter
    . Map.fromList
    . concatMap (\(op, ns) -> map (,op) ns)

true :: [PropFormula a] -> Either EH.Error (PropFormula a)
true = Interpreter.makeConstant TRUE "PropositionalLogic.TRUE"

false :: [PropFormula a] -> Either EH.Error (PropFormula a)
false = Interpreter.makeConstant FALSE "PropositionalLogic.FALSE"

not :: [PropFormula a] -> Either EH.Error (PropFormula a)
not = Interpreter.makeUnary NOT "PropositionalLogic.NOT"

or :: [PropFormula a] -> Either EH.Error (PropFormula a)
or = Interpreter.makeBinary OR "PropositionalLogic.OR"

and :: [PropFormula a] -> Either EH.Error (PropFormula a)
and = Interpreter.makeBinary AND "PropositionalLogic.AND"

implies :: [PropFormula a] -> Either EH.Error (PropFormula a)
implies = Interpreter.makeBinary IMPLIES "PropositionalLogic.IMPLIES"
