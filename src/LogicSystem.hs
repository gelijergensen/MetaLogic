{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module LogicSystem where

import qualified Data.Sequence as Seq

class LogicSystem t where
  data Operator t :: *
  data Formula t :: * -> *
  arity :: Operator t -> Arity
  identifier :: Formula t a -> a
  subformulas :: Formula t a -> Seq.Seq (Formula t a)
  inferenceRules :: t -> InferenceRules

data Arity
  = Constant
  | Unary
  | Binary
  | Ternary
  | Quaternary
  | Quinary
  | Variadic
  deriving (Eq, Show)

data InferenceRules = InferenceRules
