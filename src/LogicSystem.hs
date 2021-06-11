{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LogicSystem where

import qualified Data.Set as Set

class Operator o => LogicSystem t o where
  operators :: Set.Set o
  inferenceRules :: InferenceRules o

class Operator a where
  arity :: a -> Arity

data Arity
  = Constant
  | Unary
  | Binary
  | Ternary
  | Quaternary
  | Quinary
  | Variadic
  deriving (Eq, Show)

data InferenceRules o = InferenceRules
