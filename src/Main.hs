module Main where

import qualified OperatorSymbols as OS
import qualified PropositionalLogic as Prop

main :: IO ()
main = putStrLn "Main does run"

t = OS.Symbol "Truth" OS.Constant

x = Prop.And y (Prop.Not (Prop.Variable "C"))

y = Prop.Or (Prop.Variable "A") Prop.Truth
