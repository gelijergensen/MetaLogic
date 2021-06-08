module Main where

import qualified OperatorSymbols as OS
import qualified PropositionalLogic as Prop

main :: IO ()
main = putStrLn "Main does run"

t = OS.Symbol "Truth" OS.Constant

x = Prop.And (Prop.Variable "A") (Prop.Not Prop.Truth)

y = Prop.Or (Prop.Variable "A") (Prop.Not Prop.Truth)
