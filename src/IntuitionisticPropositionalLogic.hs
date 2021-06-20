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
      [ _orCommutative,
        _orAssociativeL,
        _orAssociativeR,
        _orTrue,
        _orFalse,
        _orSame,
        _orDistributive1,
        _orDistributive2,
        _orDistributive3,
        _orDistributive4,
        _andCommutative,
        _andAssociativeL,
        _andAssociativeR,
        _andTrue,
        _andFalse,
        _andSame,
        _andDistributive1,
        _andDistributive2,
        _andDistributive3,
        _andDistributive4,
        _trueImplies,
        _modusPonens,
        _andOrImplies1,
        _andOrImplies2,
        _andOrImplies3,
        _andOrImplies4,
        _andImpliesImplies1,
        _andImpliesImplies2,
        _andImpliesImplies3,
        _andImpliesImplies4,
        _orImpliesImplies1,
        _orImpliesImplies4,
        _importation,
        _tautology,
        _implicativeNegation
      ]
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
-- special case, as NOT x is defined to be IMPLIES x FALSE
complexity (NOT x) = 1 + complexity x
complexity (OR x y) = 1 + complexity x + complexity y
complexity (AND x y) = 1 + complexity x + complexity y
complexity (IMPLIES x y) = 1 + complexity x + complexity y

---------- Rewrite Rules ----------

_orCommutative :: PropFormula a -> PropFormula a
_orCommutative (OR x y) = OR y x
_orCommutative x = x

_orAssociativeL :: PropFormula a -> PropFormula a
_orAssociativeL (OR (OR x y) z) = OR x (OR y z)
_orAssociativeL x = x

_orAssociativeR :: PropFormula a -> PropFormula a
_orAssociativeR (OR x (OR y z)) = OR (OR x y) z
_orAssociativeR x = x

_orTrue :: PropFormula a -> PropFormula a
_orTrue (OR TRUE _) = TRUE
_orTrue (OR _ TRUE) = TRUE
_orTrue x = x

_orFalse :: PropFormula a -> PropFormula a
_orFalse (OR FALSE x) = x
_orFalse (OR x FALSE) = x
_orFalse x = x

_orSame :: Eq a => PropFormula a -> PropFormula a
_orSame (OR x y)
  | x == y = x
_orSame x = x

_orDistributive1 :: Eq a => PropFormula a -> PropFormula a
_orDistributive1 (OR (AND w x) (AND y z))
  | w == y = AND w (OR x z)
_orDistributive1 x = x

_orDistributive2 :: Eq a => PropFormula a -> PropFormula a
_orDistributive2 (OR (AND w x) (AND y z))
  | w == z = AND w (OR x y)
_orDistributive2 x = x

_orDistributive3 :: Eq a => PropFormula a -> PropFormula a
_orDistributive3 (OR (AND w x) (AND y z))
  | x == y = AND x (OR w z)
_orDistributive3 x = x

_orDistributive4 :: Eq a => PropFormula a -> PropFormula a
_orDistributive4 (OR (AND w x) (AND y z))
  | x == z = AND x (OR w y)
_orDistributive4 x = x

_andCommutative :: PropFormula a -> PropFormula a
_andCommutative (AND x y) = AND y x
_andCommutative x = x

_andAssociativeL :: PropFormula a -> PropFormula a
_andAssociativeL (AND (AND x y) z) = AND x (AND y z)
_andAssociativeL x = x

_andAssociativeR :: PropFormula a -> PropFormula a
_andAssociativeR (AND x (AND y z)) = AND (AND x y) z
_andAssociativeR x = x

_andTrue :: PropFormula a -> PropFormula a
_andTrue (AND TRUE x) = x
_andTrue (AND x TRUE) = x
_andTrue x = x

_andFalse :: PropFormula a -> PropFormula a
_andFalse (AND FALSE _) = FALSE
_andFalse (AND _ FALSE) = FALSE
_andFalse x = x

_andSame :: Eq a => PropFormula a -> PropFormula a
_andSame (AND x y)
  | x == y = x
_andSame x = x

_andDistributive1 :: Eq a => PropFormula a -> PropFormula a
_andDistributive1 (AND (OR w x) (OR y z))
  | w == y = OR w (AND x z)
_andDistributive1 x = x

_andDistributive2 :: Eq a => PropFormula a -> PropFormula a
_andDistributive2 (AND (OR w x) (OR y z))
  | w == z = OR w (AND x y)
_andDistributive2 x = x

_andDistributive3 :: Eq a => PropFormula a -> PropFormula a
_andDistributive3 (AND (OR w x) (OR y z))
  | x == y = OR x (AND w z)
_andDistributive3 x = x

_andDistributive4 :: Eq a => PropFormula a -> PropFormula a
_andDistributive4 (AND (OR w x) (OR y z))
  | x == z = OR x (AND w y)
_andDistributive4 x = x

_trueImplies :: PropFormula a -> PropFormula a
_trueImplies (IMPLIES TRUE x) = x
_trueImplies x = x

-- weakens formula slightly
_modusPonens :: Eq a => PropFormula a -> PropFormula a
_modusPonens (AND x (IMPLIES y z))
  | x == y = z
_modusPonens (AND (IMPLIES x y) z)
  | x == z = y
_modusPonens x = x

-- weakens formula slightly
_andOrImplies1 :: Eq a => PropFormula a -> PropFormula a
_andOrImplies1 (AND (OR w x) (IMPLIES y z))
  | w == y = OR x z
_andOrImplies1 x = x

-- weakens formula slightly
_andOrImplies2 :: Eq a => PropFormula a -> PropFormula a
_andOrImplies2 (AND (OR w x) (IMPLIES y z))
  | x == y = OR w z
_andOrImplies2 x = x

-- weakens formula slightly
_andOrImplies3 :: Eq a => PropFormula a -> PropFormula a
_andOrImplies3 (AND (IMPLIES y z) (OR w x))
  | w == y = OR x z
_andOrImplies3 x = x

-- weakens formula slightly
_andOrImplies4 :: Eq a => PropFormula a -> PropFormula a
_andOrImplies4 (AND (IMPLIES y z) (OR w x))
  | x == y = OR w z
_andOrImplies4 x = x

_andImpliesImplies1 :: Eq a => PropFormula a -> PropFormula a
_andImpliesImplies1 (AND (IMPLIES w x) (IMPLIES y z))
  | w == y = IMPLIES w (AND x z)
_andImpliesImplies1 x = x

-- weakens formula slightly
_andImpliesImplies2 :: Eq a => PropFormula a -> PropFormula a
_andImpliesImplies2 (AND (IMPLIES w x) (IMPLIES y z))
  | w == z = IMPLIES y x
_andImpliesImplies2 x = x

-- weakens formula slightly
_andImpliesImplies3 :: Eq a => PropFormula a -> PropFormula a
_andImpliesImplies3 (AND (IMPLIES w x) (IMPLIES y z))
  | x == y = IMPLIES w z
_andImpliesImplies3 x = x

-- weakens formula slightly
_andImpliesImplies4 :: Eq a => PropFormula a -> PropFormula a
_andImpliesImplies4 (AND (IMPLIES w x) (IMPLIES y z))
  | x == z = IMPLIES (OR w y) x
_andImpliesImplies4 x = x

_orImpliesImplies1 :: Eq a => PropFormula a -> PropFormula a
_orImpliesImplies1 (OR (IMPLIES w x) (IMPLIES y z))
  | w == y = IMPLIES w (OR x z)
_orImpliesImplies1 x = x

-- weakens formula slightly
_orImpliesImplies4 :: Eq a => PropFormula a -> PropFormula a
_orImpliesImplies4 (OR (IMPLIES w x) (IMPLIES y z))
  | x == z = IMPLIES (AND w y) z
_orImpliesImplies4 x = x

_importation :: PropFormula a -> PropFormula a
_importation (IMPLIES x (IMPLIES y z)) = IMPLIES (AND x y) z
_importation x = x

_tautology :: PropFormula a -> PropFormula a
_tautology (IMPLIES x TRUE) = TRUE
_tautology x = x

_implicativeNegation :: PropFormula a -> PropFormula a
_implicativeNegation (NOT x) = IMPLIES x FALSE
_implicativeNegation x = x

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
