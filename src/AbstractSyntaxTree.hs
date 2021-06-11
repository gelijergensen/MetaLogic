module AbstractSyntaxTree where

import qualified Data.Sequence as Seq

data AST a = AST
  { name :: a,
    children :: Seq.Seq (AST a)
  }
  deriving (Eq, Show)

ast :: a -> [AST a] -> AST a
ast n cs = AST n (Seq.fromList cs)
