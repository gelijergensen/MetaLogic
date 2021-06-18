module Main where

import Data.Either (fromRight)
import qualified Data.Set as Set
import Interpreter (interpret)
import qualified LogicSystem as LS
import Parser (parseAST)
import PeanoArithmetic
import PropositionalLogic

main :: IO ()
main = putStrLn "Main does run"

parseAsPropFormula :: String -> Formula PropositionalLogic String
parseAsPropFormula =
  fromRight undefined
    . interpret defaultPropositionalLogicInterpreter
    . fromRight undefined
    . parseAST

rewriteProp :: Ord a => PropFormula a -> Set.Set (PropFormula a)
rewriteProp = LS.rewrite PropositionalLogic

parseAsPeanoFormula :: String -> Formula PeanoArithmetic String
parseAsPeanoFormula =
  fromRight undefined
    . interpret defaultPeanoArithmeticInterpreter
    . fromRight undefined
    . parseAST

rewritePeano :: Ord a => PeanoFormula a -> Set.Set (PeanoFormula a)
rewritePeano = LS.rewrite PeanoArithmetic
