{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module PolynomialRings where

import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified ErrorHandling as EH
import qualified Interpreter
import qualified LogicSystem as LS
import qualified RApplicative as RApp

data PolynomialRings = PolynomialRings deriving (Eq, Show)

type Polynomial = LS.Formula PolynomialRings

instance LS.LogicSystem PolynomialRings where
  data Formula PolynomialRings a
    = VARIABLE a
    | CONSTANT a
    | NEG (Polynomial a)
    | PLUS (Polynomial a) (Polynomial a)
    | TIMES (Polynomial a) (Polynomial a)
    deriving (Eq, Ord, Show)

  newtype Rule PolynomialRings a = Rule
    { runRule ::
        Polynomial a ->
        Polynomial a
    }

  mapFormula = undefined
  rewriteRules = const . map Rule $ []
  runRule = runRule

---------- Rewrite Rules ----------

---------- Interpreters ----------
data PolynomialRingsInterpreter a = PolynomialRingsInterpreter
  { operatorByID ::
      Map.Map
        a
        ([Polynomial a] -> Either EH.Error (Polynomial a)),
    isVariable :: a -> Bool
  }

instance Ord a => Interpreter.Interpreter (PolynomialRingsInterpreter a) a a where
  type LogicSystem (PolynomialRingsInterpreter a) = PolynomialRings
  operatorByID = operatorByID
  variableFromID i a
    | isVariable i a = VARIABLE a
    | otherwise = CONSTANT a

defaultPolynomialRingsInterpreter :: PolynomialRingsInterpreter String
defaultPolynomialRingsInterpreter =
  polynomialRingsInterpreterFromNames
    [ (neg, ["-", "NEG", "Neg", "neg"]),
      (plus, ["+", "PLUS", "Plus", "plus"]),
      (times, ["*", "TIMES", "Times", "times"])
    ]
    firstIsUpper
  where
    firstIsUpper "" = False
    firstIsUpper (x : _) = Char.isUpper x

polynomialRingsInterpreterFromNames ::
  Ord a =>
  [ ( [Polynomial a] ->
      Either EH.Error (Polynomial a),
      [a]
    )
  ] ->
  (a -> Bool) ->
  PolynomialRingsInterpreter a
polynomialRingsInterpreterFromNames namesMap =
  PolynomialRingsInterpreter
    ( Map.fromList
        . concatMap (\(op, ns) -> map (,op) ns)
        $ namesMap
    )

neg :: [Polynomial a] -> Either EH.Error (Polynomial a)
neg = Interpreter.makeUnary NEG "PolynomialRings.NEG"

plus :: [Polynomial a] -> Either EH.Error (Polynomial a)
plus = Interpreter.makeBinary PLUS "PolynomialRings.PLUS"

times :: [Polynomial a] -> Either EH.Error (Polynomial a)
times = Interpreter.makeBinary TIMES "PolynomialRings.TIMES"
