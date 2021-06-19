module Main where

import qualified ClassicalPropositionalLogic as CPL
import Control.Monad ((<=<), (>=>))
import Data.Either (fromRight)
import qualified Data.Set as Set
import qualified ErrorHandling as EH
import Interpreter (interpret)
import qualified IntuitionisticPropositionalLogic as IPL
import qualified LogicSystem as LS
import Parser (parseAST)
import PeanoArithmetic
import PolynomialRings

main :: IO ()
main = putStrLn "Main does run"

parseAsClassicalPropFormula ::
  String -> Either EH.Error (CPL.PropFormula String)
parseAsClassicalPropFormula =
  interpret CPL.defaultPropositionalLogicInterpreter
    <=< parseAST

rewriteClassicalProp ::
  Ord a => CPL.PropFormula a -> Set.Set (CPL.PropFormula a)
rewriteClassicalProp = LS.rewrite CPL.PropositionalLogic

parseAsIntuitionisticPropFormula ::
  String -> Either EH.Error (IPL.PropFormula String)
parseAsIntuitionisticPropFormula =
  interpret IPL.defaultPropositionalLogicInterpreter
    <=< parseAST

rewriteIntuitionisticProp ::
  Ord a => IPL.PropFormula a -> Set.Set (IPL.PropFormula a)
rewriteIntuitionisticProp = LS.rewrite IPL.PropositionalLogic

parseAsPeanoFormula ::
  String -> Either EH.Error (PeanoFormula String)
parseAsPeanoFormula =
  interpret defaultPeanoArithmeticInterpreter
    <=< parseAST

rewritePeano :: Ord a => PeanoFormula a -> Set.Set (PeanoFormula a)
rewritePeano = LS.rewrite PeanoArithmetic

parseAsPolynomial ::
  String -> Either EH.Error (Polynomial String)
parseAsPolynomial =
  interpret defaultPolynomialRingsInterpreter
    <=< parseAST

rewritePolynomial :: Ord a => Polynomial a -> Set.Set (Polynomial a)
rewritePolynomial = LS.rewrite PolynomialRings

unRight :: Either a b -> b
unRight = fromRight undefined
