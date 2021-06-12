module Main where

import Data.Either (fromRight)
import Interpreter (interpret)
import Parser (parseAST)
import PropositionalLogic
import qualified PropositionalLogicEval as Prop

main :: IO ()
main = putStrLn "Main does run"

x = Prop.And y (Prop.Not (Prop.Variable "C"))

y = Prop.Or (Prop.Variable "A") Prop.Truth

parseAsPropFormula =
  fromRight undefined
    . interpret defaultPropositionalLogicInterpreter
    . fromRight undefined
    . parseAST
