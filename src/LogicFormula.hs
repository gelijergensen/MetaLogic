module LogicFormula where

import qualified Data.Sequence as Seq

data LogicFormula o a = Operator
  { name :: a,
    kind :: o,
    args :: Seq.Seq (LogicFormula o a)
  }
  deriving (Eq, Show)
