{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module LogicSystem where

class LogicSystem t where
  data Operator t :: * -> *
  data Formula t :: * -> *
  data Rule t :: * -> *

  -- arity :: Operator t a -> Arity
  identifier :: Formula t a -> a

  -- subformulas :: Formula t a -> [Formula t a]

  -- todo upgrade container
  rewriteRules :: t -> [Rule t a]
  rewrite :: Rule t a -> Formula t a -> Formula t a

-- data Arity
--   = Constant
--   | Unary
--   | Binary
--   | Ternary
--   | Quaternary
--   | Quinary
--   | Variadic
--   deriving (Eq, Show)
