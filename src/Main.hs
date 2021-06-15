module Main where

import Data.Either (fromRight)
import qualified Data.Set as Set
import Interpreter (interpret)
import qualified LogicSystem as LS
import Parser (parseAST)
import PropositionalLogic

main :: IO ()
main = putStrLn "Main does run"

parseAsPropFormula =
  fromRight undefined
    . interpret defaultPropositionalLogicInterpreter
    . fromRight undefined
    . parseAST

rewrite :: Ord a => PropFormula a -> Set.Set (PropFormula a)
rewrite = LS.rewrite PropositionalLogic
