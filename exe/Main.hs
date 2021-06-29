{-# LANGUAGE FlexibleContexts #-}

-- {-# LANGUAGE ScopedTypeVariables #-}

-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Main where

import qualified ClassicalPropositionalLogic as CPL
import Control.Monad (foldM, (<=<), (>=>))
import Data.Either (fromRight)
import Data.Functor (($>), (<&>))
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

data StringInterpreter
  = IntCPL (CPL.PropositionalLogicInterpreter String)
  | IntIPL (IPL.PropositionalLogicInterpreter String)
  | IntPA (PA.PeanoArithmeticInterpreter String)
  | IntPR (PR.PolynomialRingsInterpreter String)

data System = System
  { name :: String,
    shortName :: String,
    interpreter :: StringInterpreter
  }

data Env t a = Env
  { formulas :: Map.Map Identifier (LS.Formula t a),
    unnamedFormula :: Maybe (LS.Formula t a)
  }

data StringEnv
  = EnvCPL (Env CPL.PropositionalLogic String)
  | EnvIPL (Env IPL.PropositionalLogic String)
  | EnvPA (Env PA.PeanoArithmetic String)
  | EnvPR (Env PR.PolynomialRings String)

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
        ":q" -> quit
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
    ReplParser.DoNothing -> repl envs currentSystem
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
      let id = Identifier idString
       in case parseAndAddFormula envs currentSystem (id, formString) of
            Left err -> print err *> repl envs currentSystem
            Right newEnvs -> do
              putStrLn ""
              repl newEnvs currentSystem
    ReplParser.Step idString -> do
      newEnvs <-
        Map.alterF
          (mapM (stepFormula (Identifier idString)))
          (name currentSystem)
          envs
      putStrLn ""
      repl newEnvs currentSystem
    ReplParser.Rewrite idString -> do
      newEnvs <-
        Map.alterF
          (mapM (rewriteFormula (Identifier idString)))
          (name currentSystem)
          envs
      putStrLn ""
      repl newEnvs currentSystem
    ReplParser.Undetermined ->
      do
        putStrLn $
          "I don't recognize that command. You can type "
            ++ "\":help\" or \":h\" for a list of valid commands"
        repl envs currentSystem

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
list envs sys = sequence_ printAll *> putStrLn ""
  where
    printAll = case envs Map.!? name sys of
      Just (EnvCPL env) -> mapMEnv printFormula env
      Just (EnvIPL env) -> mapMEnv printFormula env
      Just (EnvPA env) -> mapMEnv printFormula env
      Just (EnvPR env) -> mapMEnv printFormula env
      Nothing ->
        error $ "No env found for " ++ show (name sys) ++ " in Main.list"

--------- Envs ----------
emptyEnvMap :: Envs
emptyEnvMap =
  Map.fromList $
    map (\x -> (name x, emptyEnv x)) availableLogicSystems

emptyEnv :: System -> StringEnv
emptyEnv sys = case interpreter sys of
  IntCPL _ -> EnvCPL env
  IntIPL _ -> EnvIPL env
  IntPA _ -> EnvPA env
  IntPR _ -> EnvPR env
  where
    env = Env {formulas = Map.empty, unnamedFormula = Nothing}

mapMEnv :: ((Identifier, LS.Formula t a) -> c) -> Env t a -> [c]
mapMEnv f env = map f $ namedFormulaPairs ++ unnamedFormulaPair
  where
    namedFormulaPairs = Map.assocs $ formulas env
    unnamedFormulaPair = case unnamedFormula env of
      Nothing -> []
      Just x -> [(Identifier "_", x)]

--------- Formulas ---------
printFormula ::
  Show (LS.Formula t String) => (Identifier, LS.Formula t String) -> IO ()
printFormula (Identifier idString, form) =
  putStrLn $ idString ++ " = " ++ show form

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
      (Just (EnvPA env), IntPA i) ->
        EnvPA . addFormula env id <$> parseAndInterpret i formString
      (Just (EnvPR env), IntPR i) ->
        EnvPR . addFormula env id <$> parseAndInterpret i formString
      _ -> error "Mismatched interpreter and env in Main.parseAndAddFormula"

addFormula :: Env t a -> Identifier -> LS.Formula t a -> Env t a
addFormula env id form
  | id == Identifier "_" =
    Env {formulas = formulas env, unnamedFormula = Just form}
  | otherwise =
    Env
      { formulas = Map.insert id form (formulas env),
        unnamedFormula = unnamedFormula env
      }

stepFormula :: Identifier -> StringEnv -> IO StringEnv
stepFormula id senv = case senv of
  EnvCPL env -> EnvCPL <$> doStep CPL.PropositionalLogic env
  EnvIPL env -> EnvIPL <$> doStep IPL.PropositionalLogic env
  EnvPA env -> EnvPA <$> doStep PA.PeanoArithmetic env
  EnvPR env -> EnvPR <$> doStep PR.PolynomialRings env
  where
    doStep logicsys env =
      maybe
        (pure env)
        (insertFormulas env id . LS.rewriteOnce logicsys)
        (getFormula env id)

rewriteFormula :: Identifier -> StringEnv -> IO StringEnv
rewriteFormula id senv = case senv of
  EnvCPL env -> EnvCPL <$> doStep CPL.PropositionalLogic env
  EnvIPL env -> EnvIPL <$> doStep IPL.PropositionalLogic env
  EnvPA env -> EnvPA <$> doStep PA.PeanoArithmetic env
  EnvPR env -> EnvPR <$> doStep PR.PolynomialRings env
  where
    doStep logicsys env =
      maybe
        (pure env)
        (insertFormulas env id . LS.rewrite logicsys)
        (getFormula env id)

insertFormulas ::
  Show (LS.Formula t String) =>
  Env t String ->
  Identifier ->
  Set.Set (LS.Formula t String) ->
  IO (Env t String)
insertFormulas env id formulasSet =
  newFormulas
    <&> ( \x ->
            Env
              { formulas = x,
                unnamedFormula = unnamedFormula env
              }
        )
  where
    newFormulas =
      foldM
        ( \acc (i, form) ->
            let newKey = newID id i
             in printFormula (newKey, form) $> Map.insert newKey form acc
        )
        (formulas env)
        . zip [0, 1 ..]
        . Set.toDescList
        $ formulasSet
    newID (Identifier "_") i = Identifier $ "_" ++ show i
    newID (Identifier idString) i = Identifier $ idString ++ "_" ++ show i

getFormula :: Env t a -> Identifier -> Maybe (LS.Formula t a)
getFormula env id = case id of
  Identifier "_" -> unnamedFormula env
  id -> formulas env Map.!? id

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
      (IntIPL IPL.defaultPropositionalLogicInterpreter),
    System
      "Peano Arithmetic"
      "PA"
      (IntPA PA.defaultPeanoArithmeticInterpreter),
    System
      "Polynomial Rings"
      "PR"
      (IntPR PR.defaultPolynomialRingsInterpreter)
  ]

--------- Library Shorthands ----------
parseAndInterpret ::
  Interpreter i String b =>
  i ->
  String ->
  Either EH.Error (LS.Formula (LogicSystem i) b)
parseAndInterpret i = parseAST >=> interpret i
