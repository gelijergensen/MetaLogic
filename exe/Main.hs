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
import qualified ReplParser

data System = forall s.
  LS.LogicSystem s =>
  System
  { name :: String,
    shortName :: String,
    system :: s
  }

-- data Envs
--   = forall s.
--     LS.LogicSystem s =>
--     Envs (Map.Map String (Map.Map Identifier (LS.Formula s String)))

-- newtype Identifier = Identifier {getID :: String}

main :: IO ()
main = startup

startup :: IO ()
startup = do
  putStrLn "Welcome to the MetaLogic Calculator!"
  putStrLn $
    "You may quit at any time by typing \":quit\". "
      ++ "For help, type \":help\".\n"
  loopInputSystem
  where
    loopInputSystem = do
      putStrLn "What logic system would you like to work with today?"
      putStrLn $ unlines $ map printLogicSystem availableLogicSystems
      ins <- getLine
      case ins of
        ":quit" -> quit
        ":h" -> help
        ":help" -> help
        x -> case chooseSystem x of
          Nothing -> do
            putStrLn "I don't recognize that system. Please try again."
            loopInputSystem
          Just currentSystem -> do
            putStrLn $
              name currentSystem
                ++ " selected. "
                ++ "You may change this at any time by typing "
                ++ "\":set system\" followed by a logic system name.\n"
            startRepl currentSystem

quit :: IO ()
quit = putStrLn "Goodbye!"

notImplemented :: IO ()
notImplemented = putStrLn "Sorry, that has not been implemented yet." *> quit

startRepl :: System -> IO ()
startRepl currentSystem = do
  putStrLn $
    "Please enter a formula. You may name formulas for later reference "
      ++ "by writing <name> = <formula>. For example, \"x = TRUE\"."
  putStrLn $
    "Once a formula has been entered, you may perform a single rewrite step "
      ++ "on it by typing \":step <name>\" or a complete rewrite by typing "
      ++ "\":rewrite <name>\". If no name is given, the last unnamed formula "
      ++ "will be used."
  repl currentSystem

repl :: System -> IO ()
repl currentSystem = do
  ins <- ReplParser.parseInput <$> getLine
  case ins of
    ReplParser.Quit -> quit
    ReplParser.Help -> help *> repl currentSystem
    ReplParser.NewSystem sys -> case chooseSystem sys of
      Nothing -> do
        putStrLn $
          "I don't recognize that system. The current system is "
            ++ name currentSystem
            ++ ". You can choose from"
        putStrLn $ unlines $ map printLogicSystem availableLogicSystems
        repl currentSystem
      Just newSystem -> do
        putStrLn $ "Switched to " ++ name newSystem ++ "."
        repl newSystem
    x -> print x *> notImplemented

help :: IO ()
help = do
  putStrLn "Typing \":help\" or \":h\" shows this help."
  putStrLn "You can quit at any time by typing \":quit\"."
  putStrLn $
    "You can choose a new logical system by typing "
      ++ "\":set system\" followed by a logic system name."
  putStrLn $
    "You can rewrite a formula completely by typing "
      ++ "\":rewrite\" followed by a formula identifier. "
      ++ "If no formula is specified, the last unnamed formula will be used."
  putStrLn $
    "You can perform a single rewrite step on a formula by typing "
      ++ "\":step\" followed by a formula identifier. "
      ++ "If no formula is specified, the last unnamed formula will be used."
  putStrLn $
    "Any other input will be interpreted as a formula. "
      ++ "You can name a formula by typing <name> = <formula>. "
      ++ "For example, \"x = TRUE\". "
      ++ "In rare situations, it might be necessary to wrap the formula in "
      ++ "parentheses for it to be interpreted correctly."

--------- User input ----------
-- handleInput :: (String -> UserInput) -> IO UserInput
-- handleInput f = withQuit f <$> getLine

-- withQuit :: (String -> UserInput) -> String -> UserInput
-- withQuit _ "quit" = Quit
-- withQuit f x = f x

--------- Logic Systems ----------
printLogicSystem :: System -> String
printLogicSystem x = name x ++ " (" ++ shortName x ++ ")"

chooseSystem :: String -> Maybe System
chooseSystem = (Map.!?) logicSystemsMap

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
