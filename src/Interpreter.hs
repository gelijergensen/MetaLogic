{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Interpreter where

import qualified AbstractSyntaxTree as AST
import Data.Functor ((<&>))
import qualified Data.Map as Map
import qualified LogicSystem as LS

class (Ord a, LS.LogicSystem (LogicSystem i)) => Interpreter i a where
  type LogicSystem i :: *

  -- todo later upgrade this using the Reader type / monad
  operatorByID ::
    i ->
    Map.Map
      a
      ( [LS.Formula (LogicSystem i) a] ->
        Either Interpreter.InterpretError (LS.Operator (LogicSystem i) a)
      )
  formula ::
    i -> a -> LS.Operator (LogicSystem i) a -> LS.Formula (LogicSystem i) a

newtype InterpretError = InterpretError String deriving (Show)

interpret ::
  (Show a, Interpreter i a) =>
  i ->
  AST.AST a ->
  Either InterpretError (LS.Formula (LogicSystem i) a)
interpret i (AST.AST n children) = case Map.lookup n (operatorByID i) of
  Nothing -> Left $ InterpretError $ "No operator for " ++ show n
  (Just op) -> mapM (interpret i) children >>= op <&> formula i n

makeConstant ::
  LS.Operator t a ->
  String ->
  [LS.Formula t a] ->
  Either InterpretError (LS.Operator t a)
makeConstant op _ [] = Right op
makeConstant _ name _ =
  Left $ InterpretError $ name ++ " doesn't expect children"

makeUnary ::
  (LS.Formula t a -> LS.Operator t a) ->
  String ->
  [LS.Formula t a] ->
  Either InterpretError (LS.Operator t a)
makeUnary _ name [] =
  Left $ InterpretError $ name ++ " expects 1 child; recieved none"
makeUnary op _ [x] = Right $ op x
makeUnary _ name _ =
  Left $ InterpretError $ name ++ " expects 1 child; recieved multiple"

makeBinary ::
  (LS.Formula t a -> LS.Formula t a -> LS.Operator t a) ->
  String ->
  [LS.Formula t a] ->
  Either InterpretError (LS.Operator t a)
makeBinary _ name [] =
  Left $ InterpretError $ name ++ " expects 2 children; recieved none"
makeBinary _ name [_] =
  Left $ InterpretError $ name ++ " expects 2 children; recieved 1"
makeBinary op _ [x, y] = Right $ op x y
makeBinary _ name _ =
  Left $ InterpretError $ name ++ " expects 2 children; recieved 3 or more"
