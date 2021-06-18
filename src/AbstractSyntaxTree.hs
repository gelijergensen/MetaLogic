module AbstractSyntaxTree where

import qualified Data.Sequence as Seq

data AST a = AST
  { name :: a,
    children :: [AST a]
  }
  deriving (Eq, Show)

ast :: a -> [AST a] -> AST a
ast = AST

instance Functor AST where
  fmap f (AST n as) = AST (f n) (fmap (fmap f) as)
