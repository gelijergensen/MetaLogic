module Main where

import Control.Monad ((<=<), (>=>))
import Data.Either (fromRight)
import qualified Data.Set as Set
import qualified ErrorHandling as EH
import Interpreter (interpret)
import qualified LogicSystem as LS
import Parser (parseAST)
import PeanoArithmetic
import PropositionalLogic

main :: IO ()
main = putStrLn "Main does run"

parseAsPropFormula ::
  String -> Either EH.Error (Formula PropositionalLogic String)
parseAsPropFormula =
  interpret defaultPropositionalLogicInterpreter
    <=< parseAST

rewriteProp :: Ord a => PropFormula a -> Set.Set (PropFormula a)
rewriteProp = LS.rewrite PropositionalLogic

parseAsPeanoFormula ::
  String -> Either EH.Error (Formula PeanoArithmetic String)
parseAsPeanoFormula =
  interpret defaultPeanoArithmeticInterpreter
    <=< parseAST

rewritePeano :: Ord a => PeanoFormula a -> Set.Set (PeanoFormula a)
rewritePeano = LS.rewrite PeanoArithmetic

unRight :: Either a b -> b
unRight = fromRight undefined
