{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Interpreter where

import qualified AbstractSyntaxTree as AST
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified LogicSystem as LS

class Ord a => Interpreter i a where
  type LogicSystem i :: *

  -- todo later upgrade this using the Reader type / monad
  operatorByID :: i -> Map.Map a (LS.Operator (LogicSystem i))
  formula ::
    i ->
    a ->
    LS.Operator (LogicSystem i) ->
    Seq.Seq (LS.Formula (LogicSystem i) a) ->
    LS.Formula (LogicSystem i) a

newtype InterpretError = InterpretError String deriving (Show)

interpret ::
  (Show a, Interpreter i a) =>
  i ->
  AST.AST a ->
  Either InterpretError (LS.Formula (LogicSystem i) a)
interpret i (AST.AST n children) = case Map.lookup n (operatorByID i) of
  Nothing -> Left $ InterpretError $ "No operator for " ++ show n
  (Just op) -> formula i n op <$> mapM (interpret i) children
