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
import qualified PeanoArithmetic as PA
import qualified PolynomialRings as PR

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
  String -> Either EH.Error (PA.PeanoFormula String)
parseAsPeanoFormula =
  interpret PA.defaultPeanoArithmeticInterpreter
    <=< parseAST

rewritePeano :: Ord a => PA.PeanoFormula a -> Set.Set (PA.PeanoFormula a)
rewritePeano = LS.rewrite PA.PeanoArithmetic

parseAsPolynomial ::
  String -> Either EH.Error (PR.Polynomial String)
parseAsPolynomial =
  interpret PR.defaultPolynomialRingsInterpreter
    <=< parseAST

rewritePolynomial :: Ord a => PR.Polynomial a -> Set.Set (PR.Polynomial a)
rewritePolynomial = LS.rewrite PR.PolynomialRings

unRight :: Either a b -> b
unRight = fromRight undefined
