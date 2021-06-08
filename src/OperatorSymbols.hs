module OperatorSymbols where

import qualified Data.Sequence as Seq

data Symbol t a = Symbol
  { context :: t,
    arguments :: Arity a
  }
  deriving (Eq, Show)

data Arity a
  = Constant
  | Unary a
  | Binary a a
  | Ternary a a a
  | Quaternary a a a a
  | Quinary a a a a a
  | Variadic (Seq.Seq a)
  deriving (Eq, Show)

(!!) :: Arity a -> Int -> a
Constant !! _ = error "Out of index in call to OperatorSymbols.(!!): Constant"
(Unary a) !! 0 = a
(Unary _) !! i =
  error $ "Out of index in call to OperatorSymbols.(!!): Unary " ++ show i
(Binary a _) !! 0 = a
(Binary _ b) !! 1 = b
(Binary _ _) !! i =
  error $ "Out of index in call to OperatorSymbols.(!!): Binary " ++ show i
(Ternary a _ _) !! 0 = a
(Ternary _ b _) !! 1 = b
(Ternary _ _ c) !! 2 = c
Ternary {} !! i =
  error $ "Out of index in call to OperatorSymbols.(!!): Ternary " ++ show i
(Quaternary a _ _ _) !! 0 = a
(Quaternary _ b _ _) !! 1 = b
(Quaternary _ _ c _) !! 2 = c
(Quaternary _ _ _ d) !! 3 = d
Quaternary {} !! i =
  error $ "Out of index in call to OperatorSymbols.(!!): Quaternary " ++ show i
(Quinary a _ _ _ _) !! 0 = a
(Quinary _ b _ _ _) !! 1 = b
(Quinary _ _ c _ _) !! 2 = c
(Quinary _ _ _ d _) !! 3 = d
(Quinary _ _ _ _ e) !! 4 = e
Quinary {} !! i =
  error $ "Out of index in call to OperatorSymbols.(!!): Quinary " ++ show i
(Variadic as) !! i = Seq.index as i
