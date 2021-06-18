{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Interpreter where

import qualified AbstractSyntaxTree as AST
import Data.Functor ((<&>))
import qualified Data.Map as Map
import qualified ErrorHandling as EH
import qualified LogicSystem as LS

class (Ord a, LS.LogicSystem (LogicSystem i)) => Interpreter i a b where
  type LogicSystem i :: *

  -- todo later upgrade this using the Reader type / monad
  operatorByID ::
    i ->
    Map.Map
      a
      ( [LS.Formula (LogicSystem i) b] ->
        Either EH.Error (LS.Formula (LogicSystem i) b)
      )
  variableFromID ::
    i -> a -> LS.Formula (LogicSystem i) b

interpret ::
  Interpreter i a b =>
  i ->
  AST.AST a ->
  Either EH.Error (LS.Formula (LogicSystem i) b)
interpret i (AST.AST n children) = case Map.lookup n (operatorByID i) of
  Nothing -> pure $ variableFromID i n
  (Just op) -> mapM (interpret i) children >>= op

makeConstant ::
  LS.Formula t a ->
  String ->
  [LS.Formula t a] ->
  Either EH.Error (LS.Formula t a)
makeConstant op _ [] = Right op
makeConstant _ name _ =
  Left $ EH.interpretError $ name ++ " doesn't expect children"

makeUnary ::
  (LS.Formula t a -> LS.Formula t a) ->
  String ->
  [LS.Formula t a] ->
  Either EH.Error (LS.Formula t a)
makeUnary _ name [] =
  Left $ EH.interpretError $ name ++ " expects 1 child; recieved none"
makeUnary op _ [x] = Right $ op x
makeUnary _ name _ =
  Left $ EH.interpretError $ name ++ " expects 1 child; recieved multiple"

makeBinary ::
  (LS.Formula t a -> LS.Formula t a -> LS.Formula t a) ->
  String ->
  [LS.Formula t a] ->
  Either EH.Error (LS.Formula t a)
makeBinary _ name [] =
  Left $ EH.interpretError $ name ++ " expects 2 children; recieved none"
makeBinary _ name [_] =
  Left $ EH.interpretError $ name ++ " expects 2 children; recieved 1"
makeBinary op _ [x, y] = Right $ op x y
makeBinary _ name _ =
  Left $ EH.interpretError $ name ++ " expects 2 children; recieved 3 or more"
