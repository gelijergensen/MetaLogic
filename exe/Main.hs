{-# LANGUAGE ExistentialQuantification #-}

module Main where

import qualified ClassicalPropositionalLogic as CPL
import Control.Monad ((<=<), (>=>))
import Data.Either (fromRight)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified ErrorHandling as EH
import Interpreter (Interpreter (LogicSystem), interpret)
import qualified IntuitionisticPropositionalLogic as IPL
import qualified LogicSystem as LS
import Parser (parseAST)
import qualified PeanoArithmetic as PA
import qualified PolynomialRings as PR

data System = forall s.
  LS.LogicSystem s =>
  System
  { name :: String,
    shortName :: String,
    system :: s
  }

data UserInput
  = Quit
  | NewSystem System
  | NewFormula Identifier String
  | Step Identifier
  | Rewrite Identifier
  | Undetermined

newtype Identifier = Identifier {getID :: String}

main :: IO ()
main = startup

startup :: IO ()
startup = do
  putStrLn "Welcome to the MetaLogic Calculator!"
  putStrLn "You may quit at any time by typing \"quit\"\n"
  loopInputSystem
  where
    loopInputSystem = do
      putStrLn "What logic system would you like to work with today?"
      putStrLn $ unlines $ map printLogicSystem availableLogicSystems
      ins <- handleInput processSystem
      case ins of
        Quit -> quit
        NewSystem currentSystem ->
          do
            putStrLn $
              name currentSystem
                ++ " selected. "
                ++ "You may change this at any time by typing "
                ++ "\"set \" followed by a logic system name.\n"
            startRepl currentSystem
        _ -> do
          putStrLn "I don't recognize that system. Please try again."
          loopInputSystem

quit :: IO ()
quit = putStrLn "Goodbye!"

notImplemented :: IO ()
notImplemented = putStrLn "Sorry, that has not been implemented yet." *> quit

startRepl :: System -> IO ()
startRepl currentSystem = do
  putStrLn $
    "Please enter a formula. You may name formulas for later reference "
      ++ "by writing <name> = <formula>. For example, \"x = TRUE\"."
  repl currentSystem

repl :: System -> IO ()
repl currentSystem = do
  notImplemented

--------- User input ----------
handleInput :: (String -> UserInput) -> IO UserInput
handleInput f = withQuit f <$> getLine

withQuit :: (String -> UserInput) -> String -> UserInput
withQuit _ "quit" = Quit
withQuit f x = f x

processSystem :: String -> UserInput
processSystem xs =
  maybe Undetermined NewSystem (Map.lookup xs logicSystemsMap)

--------- Logic Systems ----------
printLogicSystem :: System -> String
printLogicSystem x = name x ++ " (" ++ shortName x ++ ")"

logicSystemsMap :: Map.Map String System
logicSystemsMap =
  Map.fromList $
    concatMap (\x -> [(shortName x, x), (name x, x)]) availableLogicSystems

availableLogicSystems :: [System]
availableLogicSystems =
  [ System "Classical Propositional Logic" "CPL" CPL.PropositionalLogic,
    System "Intuitionistic Propositional Logic" "IPL" IPL.PropositionalLogic,
    System "Peano Arithmetic" "PA" PA.PeanoArithmetic,
    System "Polynomial Rings" "PR" PR.PolynomialRings
  ]

--------- Library Shorthands ----------
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
