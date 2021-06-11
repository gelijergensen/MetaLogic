{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interpreter where

import qualified AbstractSyntaxTree as AST
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified LogicFormula as LF

class Ord a => Interpreter o a i where
  operatorKinds :: i -> Map.Map a o

newtype InterpretError = InterpretError String deriving (Show)

interpret ::
  (Show a, Interpreter o a i) =>
  i ->
  AST.AST a ->
  Either InterpretError (LF.LogicFormula o a)
interpret i (AST.AST n args) = case Map.lookup n (operatorKinds i) of
  Nothing -> Left $ InterpretError $ "No operator for " ++ show n
  (Just op) -> LF.Operator n op <$> mapM (interpret i) args
