module AbstractSyntaxTree.Internal where

import qualified Data.Sequence as Seq

data AST a = AST
  { name :: a,
    children :: [AST a]
  }
  deriving (Eq, Show)

ast :: a -> [AST a] -> AST a
ast = AST
