{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module PeanoArithmetic where

import qualified Data.Map as Map
import qualified Interpreter
import qualified LogicSystem as LS
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

  mapFormula = undefined
  rewriteRules = const . map Rule $ []
  runRule = runRule

---------- Interpreters ----------
newtype PeanoArithmeticInterpreter a = PeanoArithmeticInterpreter
  { operatorByID ::
      Map.Map
        a
        ([PeanoFormula a] -> Either Interpreter.InterpretError (PeanoFormula a))
  }

instance Ord a => Interpreter.Interpreter (PeanoArithmeticInterpreter a) a where
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
      Either Interpreter.InterpretError (PeanoFormula a),
      [a]
    )
  ] ->
  PeanoArithmeticInterpreter a
peanoArithmeticInterpreterFromNames =
  PeanoArithmeticInterpreter
    . Map.fromList
    . concatMap (\(op, ns) -> map (,op) ns)

zero :: [PeanoFormula a] -> Either Interpreter.InterpretError (PeanoFormula a)
zero = Interpreter.makeConstant ZERO "PeanoArithmetic.ZERO"

succ :: [PeanoFormula a] -> Either Interpreter.InterpretError (PeanoFormula a)
succ = Interpreter.makeUnary S "PeanoArithmetic.S"

plus :: [PeanoFormula a] -> Either Interpreter.InterpretError (PeanoFormula a)
plus = Interpreter.makeBinary PLUS "PeanoArithmetic.PLUS"

times :: [PeanoFormula a] -> Either Interpreter.InterpretError (PeanoFormula a)
times = Interpreter.makeBinary TIMES "PeanoArithmetic.TIMES"
