{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module PeanoArithmetic where

import qualified Data.Map as Map
import qualified ErrorHandling as EH
import qualified Interpreter
import qualified LogicSystem as LS
import qualified RApplicative as RApp
import Prelude hiding (succ)

data PeanoArithmetic = PeanoArithmetic deriving (Eq, Show)

type PeanoFormula = LS.Formula PeanoArithmetic

instance LS.LogicSystem PeanoArithmetic where
  data Formula PeanoArithmetic a
    = ZERO
    | VAR a
    | S (PeanoFormula a)
    | PLUS (PeanoFormula a) (PeanoFormula a)
    | TIMES (PeanoFormula a) (PeanoFormula a)
    deriving (Eq, Ord, Show)

  newtype Rule PeanoArithmetic a = Rule
    { runRule ::
        PeanoFormula a ->
        PeanoFormula a
    }
  type RuleConstraint PeanoArithmetic a = Ord a

  mapFormula = mapFormula
  rewriteRules =
    const . map Rule $
      [ _plusZero1,
        _plusZero2,
        _plusSucc1,
        _plusSucc2,
        _timesZero1,
        _timesZero2,
        _timesSucc1,
        _timesSucc2,
        _plusAssociative,
        _plusCommutative,
        _timesAssociative,
        _timesCommutative
      ]
  runRule = runRule

mapFormula ::
  Ord (PeanoFormula a) =>
  (PeanoFormula a -> LS.Frontier (PeanoFormula a)) ->
  PeanoFormula a ->
  LS.Frontier (PeanoFormula a)
mapFormula f term@(TIMES x y) =
  f term <> RApp.liftA2 TIMES (mapFormula f x) (mapFormula f y)
mapFormula f term@(PLUS x y) =
  f term <> RApp.liftA2 PLUS (mapFormula f x) (mapFormula f y)
mapFormula f term@(S x) = f term <> RApp.fmap S (mapFormula f x)
mapFormula f term = f term

-- Approximate measure of complexity of formula
-- No rewrite rule should increase this
complexity :: PeanoFormula a -> Int
complexity ZERO = 1
complexity (VAR _) = 1
complexity (S x) = 1 + complexity x
complexity (PLUS x y) = 1 + complexity x + complexity y
complexity (TIMES x y) = 2 * (complexity x + 1) * (complexity y + 1)

---------- Rewrite Rules ----------

_plusZero1 :: PeanoFormula a -> PeanoFormula a
_plusZero1 (PLUS x ZERO) = x
_plusZero1 x = x

_plusZero2 :: PeanoFormula a -> PeanoFormula a
_plusZero2 (PLUS ZERO x) = x
_plusZero2 x = x

_plusSucc1 :: PeanoFormula a -> PeanoFormula a
_plusSucc1 (PLUS x (S y)) = S (PLUS x y)
_plusSucc1 x = x

_plusSucc2 :: PeanoFormula a -> PeanoFormula a
_plusSucc2 (PLUS (S x) y) = S (PLUS x y)
_plusSucc2 x = x

_timesZero1 :: PeanoFormula a -> PeanoFormula a
_timesZero1 (TIMES _ ZERO) = ZERO
_timesZero1 x = x

_timesZero2 :: PeanoFormula a -> PeanoFormula a
_timesZero2 (TIMES ZERO _) = ZERO
_timesZero2 x = x

_timesSucc1 :: PeanoFormula a -> PeanoFormula a
_timesSucc1 (TIMES x (S y)) = PLUS x (TIMES x y)
_timesSucc1 x = x

_timesSucc2 :: PeanoFormula a -> PeanoFormula a
_timesSucc2 (TIMES (S x) y) = PLUS x (TIMES x y)
_timesSucc2 x = x

_plusAssociative :: PeanoFormula a -> PeanoFormula a
_plusAssociative (PLUS (PLUS x y) z) = PLUS x (PLUS y z)
_plusAssociative x = x

_plusCommutative :: Ord a => PeanoFormula a -> PeanoFormula a
_plusCommutative (PLUS x y)
  | x <= y = PLUS x y
  | otherwise = PLUS y x
_plusCommutative x = x

_timesAssociative :: PeanoFormula a -> PeanoFormula a
_timesAssociative (TIMES (TIMES x y) z) = TIMES x (TIMES y z)
_timesAssociative x = x

_timesCommutative :: Ord a => PeanoFormula a -> PeanoFormula a
_timesCommutative (TIMES x y)
  | x <= y = TIMES x y
  | otherwise = TIMES y x
_timesCommutative x = x

---------- Interpreters ----------
newtype PeanoArithmeticInterpreter a = PeanoArithmeticInterpreter
  { operatorByID ::
      Map.Map
        a
        ([PeanoFormula a] -> Either EH.Error (PeanoFormula a))
  }

instance Ord a => Interpreter.Interpreter (PeanoArithmeticInterpreter a) a a where
  type LogicSystem (PeanoArithmeticInterpreter a) = PeanoArithmetic
  operatorByID = operatorByID
  variableFromID = const VAR

defaultPeanoArithmeticInterpreter :: PeanoArithmeticInterpreter String
defaultPeanoArithmeticInterpreter =
  peanoArithmeticInterpreterFromNames
    [ (zero, ["0", "ZERO", "Zero", "zero"]),
      (succ, ["S", "SUCC", "Succ", "succ"]),
      (plus, ["+", "PLUS", "Plus", "plus"]),
      (times, ["*", "TIMES", "Times", "times"])
    ]

peanoArithmeticInterpreterFromNames ::
  Ord a =>
  [ ( [PeanoFormula a] ->
      Either EH.Error (PeanoFormula a),
      [a]
    )
  ] ->
  PeanoArithmeticInterpreter a
peanoArithmeticInterpreterFromNames =
  PeanoArithmeticInterpreter
    . Map.fromList
    . concatMap (\(op, ns) -> map (,op) ns)

zero :: [PeanoFormula a] -> Either EH.Error (PeanoFormula a)
zero = Interpreter.makeConstant ZERO "PeanoArithmetic.ZERO"

succ :: [PeanoFormula a] -> Either EH.Error (PeanoFormula a)
succ = Interpreter.makeUnary S "PeanoArithmetic.S"

plus :: [PeanoFormula a] -> Either EH.Error (PeanoFormula a)
plus = Interpreter.makeBinary PLUS "PeanoArithmetic.PLUS"

times :: [PeanoFormula a] -> Either EH.Error (PeanoFormula a)
times = Interpreter.makeBinary TIMES "PeanoArithmetic.TIMES"
