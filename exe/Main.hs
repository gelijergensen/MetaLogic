{-# LANGUAGE FlexibleContexts #-}

-- {-# LANGUAGE ScopedTypeVariables #-}

-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Main where

import qualified ClassicalPropositionalLogic as CPL
import Control.Monad ((<=<), (>=>))
import Data.Either (fromRight)
import Data.Functor ((<&>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified ErrorHandling as EH
import qualified Interpreter as I
import qualified IntuitionisticPropositionalLogic as IPL
import qualified LogicSystem as LS
import Parser (parseAST)
import qualified PeanoArithmetic as PA
import qualified PolynomialRings as PR
import qualified ReplParser

data StringInterpreter
  = IntCPL (CPL.PropositionalLogicInterpreter String)
  | IntIPL (IPL.PropositionalLogicInterpreter String)

data System = System
  { name :: String,
    shortName :: String,
    interpreter :: StringInterpreter
  }

data Env a b = Env
  { formulas :: Map.Map Identifier (LS.Formula a b),
    unnamedFormula :: Maybe (LS.Formula a b)
  }

data StringEnv
  = EnvCPL (Env CPL.PropositionalLogic String)
  | EnvIPL (Env IPL.PropositionalLogic String)

type Envs = Map.Map String StringEnv

newtype Identifier = Identifier {getID :: String} deriving (Eq, Ord)

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
  putStrLn "To list available formulas, type \":list\" or \":l\" at any time."
  repl emptyEnvMap currentSystem

repl :: Envs -> System -> IO ()
repl envs currentSystem = do
  ins <- ReplParser.parseInput <$> getLine
  case ins of
    ReplParser.Quit -> quit
    ReplParser.Help -> help *> repl envs currentSystem
    ReplParser.List -> list envs currentSystem *> repl envs currentSystem
    ReplParser.NewSystem sys -> case chooseSystem sys of
      Nothing -> do
        putStrLn $
          "I don't recognize that system. The current system is "
            ++ name currentSystem
            ++ ". You can choose from"
        putStrLn $ unlines $ map printLogicSystem availableLogicSystems
        repl envs currentSystem
      Just newSystem -> do
        putStrLn $ "Switched to " ++ name newSystem ++ "."
        repl envs newSystem
    ReplParser.NewFormula idString formString ->
      case parseAndAddFormula envs currentSystem (Identifier idString, formString) of
        Left err -> print err *> repl envs currentSystem
        Right newEnvs -> repl newEnvs currentSystem
    --todo formulas
    x -> print x *> notImplemented

help :: IO ()
help = do
  putStrLn "Typing \":help\" or \":h\" shows this help."
  putStrLn "You can quit at any time by typing \":quit\" or \":q\"."
  putStrLn "For a list of available formulas, type \":list\" or \":l\"."
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

list :: Envs -> System -> IO ()
list envs sys = sequence_ (mapMEnv printFormula env) *> putStrLn ""
  where
    printFormula (Identifier idString, form) =
      putStrLn $ idString ++ " = " ++ show form
    env = case envs Map.!? name sys of
      Just (EnvCPL env) -> env
      _ -> error "Unexpected env in Main.list"

--------- Envs ----------
emptyEnvMap :: Envs
emptyEnvMap =
  Map.fromList $
    map (\x -> (name x, emptyEnv x)) availableLogicSystems

emptyEnv :: System -> StringEnv
emptyEnv sys = case interpreter sys of
  IntCPL _ -> EnvCPL env
  IntIPL _ -> EnvIPL env
  where
    env = Env {formulas = Map.empty, unnamedFormula = Nothing}

mapMEnv :: ((Identifier, LS.Formula a b) -> c) -> Env a b -> [c]
mapMEnv f env = map f $ namedFormulaPairs ++ unnamedFormulaPair
  where
    namedFormulaPairs = Map.assocs $ formulas env
    unnamedFormulaPair = case unnamedFormula env of
      Nothing -> []
      Just x -> [(Identifier "_", x)]

parseAndAddFormula ::
  Envs -> System -> (Identifier, String) -> Either EH.Error Envs
parseAndAddFormula envs sys (id, formString) =
  (\x -> Map.insert envKey x envs) <$> newEnv
  where
    envKey = name sys
    newEnv = case (envs Map.!? envKey, interpreter sys) of
      (Just (EnvCPL env), IntCPL i) ->
        EnvCPL . addFormula env id <$> parseAndInterpret i formString
      (Just (EnvIPL env), IntIPL i) ->
        EnvIPL . addFormula env id <$> parseAndInterpret i formString
      _ -> error "Mismatched interpreter and env in Main.parseAndAddFormula"

addFormula :: Env a b -> Identifier -> LS.Formula a b -> Env a b
addFormula env id form
  | id == Identifier "_" =
    Env {formulas = formulas env, unnamedFormula = Just form}
  | otherwise =
    Env
      { formulas = Map.insert id form (formulas env),
        unnamedFormula = unnamedFormula env
      }

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
  [ System
      "Classical Propositional Logic"
      "CPL"
      (IntCPL CPL.defaultPropositionalLogicInterpreter),
    System
      "Intuitionistic Propositional Logic"
      "IPL"
      (IntIPL IPL.defaultPropositionalLogicInterpreter)
      -- System
      --   "Peano Arithmetic"
      --   "PA"
      --   PA.defaultPeanoArithmeticInterpreter,
      -- System
      --   "Polynomial Rings"
      --   "PR"
      --   PR.defaultPolynomialRingsInterpreter
  ]

--------- Library Shorthands ----------
parseAndInterpret ::
  I.Interpreter i String b =>
  i ->
  String ->
  Either EH.Error (LS.Formula (I.LogicSystem i) b)
parseAndInterpret i = parseAST >=> I.interpret i

rewriteClassicalProp ::
  Ord a => CPL.PropFormula a -> Set.Set (CPL.PropFormula a)
rewriteClassicalProp = LS.rewrite CPL.PropositionalLogic

rewriteIntuitionisticProp ::
  Ord a => IPL.PropFormula a -> Set.Set (IPL.PropFormula a)
rewriteIntuitionisticProp = LS.rewrite IPL.PropositionalLogic

rewritePeano :: Ord a => PA.PeanoFormula a -> Set.Set (PA.PeanoFormula a)
rewritePeano = LS.rewrite PA.PeanoArithmetic

rewritePolynomial :: Ord a => PR.Polynomial a -> Set.Set (PR.Polynomial a)
rewritePolynomial = LS.rewrite PR.PolynomialRings
